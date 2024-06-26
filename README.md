# waifuVault-fortran-api-unofficial

![tests](https://github.com/nakedmcse/waifuVault-fortran-api-unofficial/actions/workflows/build.yml/badge.svg)
[![GitHub issues](https://img.shields.io/github/issues/nakedmcse/waifuvault-fortran-api-unofficial.png)](https://github.com/nakedmcse/waifuvault-fortran-api-unofficial/issues)
[![last-commit](https://img.shields.io/github/last-commit/nakedmcse/waifuvault-fortran-api-unofficial)](https://github.com/nakedmcse/waifuvault-fortran-api-unofficial/commits/master)

This contains the unofficial API bindings for uploading, deleting and obtaining files
with [waifuvault.moe](https://waifuvault.moe/). Contains a full up to date API for interacting with the service.

This is unofficial and as such will not be supported officially.  Use it at your own risk.  Updates to keep it comparable to the official
SDKs will be on a best effort basis only.

## Installation

The SDK uses the libcurl-fortran bindings to provide the transport mechanism.  First you must build the libcurl-fortran 
library.

```sh
cd libcurl
gcc -c curl_macro.c
gfortran -c curl_util.f90
gfortran -c curl_easy.f90
gfortran -c curl_multi.f90
gfortran -c curl_urlapi.f90
gfortran -c curl.f90
ar rcs libfortran-curl.a curl.o curl_easy.o curl_multi.o curl_urlapi.o curl_util.o curl_macro.o
cp *.mod ../
```

After that you can then build the waifuvault module.

```sh
gfortran -c httpcallbackModule.f90
gfortran -c waifuModelsModule.f90
gfortran -cpp -c waifuAPIModule.f90
ar rcs lib-waifuvault.a httpcallbackModule.o waifuModelsModule.o waifuAPIModule.o
```

You can then reference the waifuvault module at the top of your code.

```fortran
use http_callback
use waifuvault_models
use waifuvault_api
```

You can then finally link your program against waifuvault and libcurl.

```sh
gfortran -o your-code lib-waifuvault.a libcurl/libfortran-curl.a your-code.f90 -lcurl
```

## Error Handling

After each call to the SDK, you need to use a call to getError to check for any errors during the call.  If it returns status 0, then all was OK.

```fortran
type(file_response) :: response
type(error_response) :: error

response = fileInfo('bad-token', .true.)

call getError(error)
if (error%status /= 0) then 
    print *, '--Error Object--'
    print *, 'Name:', trim(error%name)
    print *, 'Status:', error%status
    print *, 'Message:', trim(error%message)
end if
```

## Usage

This API contains 5 interactions:

1. [Upload File](#upload-file)
2. [Get File Info](#get-file-info)
3. [Update File Info](#update-file-info)
4. [Delete File](#delete-file)
5. [Get File](#get-file)

You need to include the module files in your code for the package:

```fortran
use http_callback
use waifuvault_models
use waifuvault_api
```

You also need to handle opening and closing a CURL session.  At the top of your code put:

```fortran
call openCurl()
```

And at the bottom:

```fortran
call closeCurl()
```

### Upload File<a id="upload-file"></a>

To Upload a file, use the `uploadFile` function. This function takes the following options as an object:

| Option            | Type         | Description                                                     | Required       | Extra info                       |
|-------------------|--------------|-----------------------------------------------------------------|----------------|----------------------------------|
| `filename`        | `string `    | The path to the file to upload                                  | true if File   | File path                        |
| `url`             | `string`     | The URL of the file to target                                   | true if URL    | Filename with extension          |
| `buffer`          | `byte array` | Byte array containing file to upload                            | true if buffer | Needs filename set also          |
| `expires`         | `string`     | A string containing a number and a unit (1d = 1day)             | false          | Valid units are `m`, `h` and `d` |
| `hideFilename`    | `logical`    | If true, then the uploaded filename won't appear in the URL     | false          | Defaults to `false`              |
| `password`        | `string`     | If set, then the uploaded file will be encrypted                | false          |                                  |
| `oneTimeDownload` | `logical`    | if supplied, the file will be deleted as soon as it is accessed | false          |                                  |

Using a URL:

```fortran
type(file_upload) :: url_upload
type(file_response) :: response

call url_upload%create_upload('https://waifuvault.moe/assets/custom/images/08.png', '10m', '', .false., .false.)
response = uploadFile(url_upload)
print *, '--URL Upload Response Object--'
print *, 'Token:', trim(response%token)
print *, 'URL:', trim(response%url)
```

Using a file path:

```fortran
type(file_upload) :: upload
type(file_response) :: response

call upload%create_upload('./acoolfile.png', '10m', '', .false., .false.)
response = uploadFile(upload)
print *, '--File Upload Response Object--'
print *, 'Token:', trim(response%token)
print *, 'URL:', trim(response%url)
```

Using a buffer:

```fortran
type(file_upload) :: buffer_upload
type(file_response) :: response
integer :: iostatus

buffer_upload%filename = 'RoryMercuryFromBuffer.png'
buffer_upload%url = ''  !IMPORTANT to init url empty
buffer_upload%expires = '10m'
buffer_upload%password = ''
buffer_upload%hideFilename = .false.
buffer_upload%oneTimeDownload = .false.

open(unit=10, file='RoryMercury.png', form='unformatted', access='stream', action='read', iostat=iostatus)
inquire(unit=10, size=buffer_upload%buffer_size)
allocate(character(len=buffer_upload%buffer_size) :: buffer_upload%buffer)
read(10, iostat=iostatus) buffer_upload%buffer

response = uploadFile(buffer_upload)
close(10)
deallocate(buffer_upload%buffer)
print *, '--File Upload Response Object--'
print *, 'Token:', trim(response%token)
print *, 'URL:', trim(response%url)
```

### Get File Info<a id="get-file-info"></a>

If you have a token from your upload. Then you can get file info. This results in the following info:

* token
* url
* protected
* retentionPeriod

Use the `fileInfo` function. This function takes the following options as parameters:

| Option      | Type      | Description                                                        | Required | Extra info        |
|-------------|-----------|--------------------------------------------------------------------|----------|-------------------|
| `token`     | `string`  | The token of the upload                                            | true     |                   |
| `formatted` | `logical` | If you want the `retentionPeriod` to be human-readable or an epoch | false    | defaults to false |

Epoch timestamp:

```fortran
type(file_response) :: response

response = fileInfo(token, .false.)
print *, '--FileInfo Response Object--'
print *, 'Token:', trim(response%token)
print *, 'URL:', trim(response%url)
print *, 'Retention:', trim(response%retentionPeriod)
print *, 'Options/hideFilename:', response%options%hideFilename
print *, 'Options/oneTimeDownload:', response%options%oneTimeDownload
print *, 'Options/protected:', response%options%protected
```

Human-readable timestamp:

```fortran
type(file_response) :: response

response = fileInfo(token, .true.)
print *, '--FileInfo Response Object--'
print *, 'Token:', trim(response%token)
print *, 'URL:', trim(response%url)
print *, 'Retention:', trim(response%retentionPeriod)
print *, 'Options/hideFilename:', response%options%hideFilename
print *, 'Options/oneTimeDownload:', response%options%oneTimeDownload
print *, 'Options/protected:', response%options%protected
```

### Update File Info<a id="update-file-info"></a>

If you have a token from your upload, then you can update the information for the file.  You can change the password or remove it,
you can set custom expiry time or remove it, and finally you can choose whether the filename is hidden.

Use the `fileUpdate` function. This function takes the following options as parameters:

| Option              | Type      | Description                                             | Required | Extra info                                  |
|---------------------|-----------|---------------------------------------------------------|----------|---------------------------------------------|
| `token`             | `string`  | The token of the upload                                 | true     |                                             |
| `password`          | `string`  | The current password of the file                        | false    | Set to empty string to remove password      |
| `previousPassword`  | `string`  | The previous password of the file, if changing password | false    |                                             |
| `customExpiry`      | `string`  | Custom expiry in the same form as upload command        | false    | Set to empty string to remove custom expiry |
| `hideFilename`      | `logical` | Sets whether the filename is visible in the URL or not  | false    |                                             |

```fortran
type(file_response) :: response

response = fileUpdate(response%token, 'password', 'previouspassword', '1h', .false.)
print *, '--FileUpdate Response Object--'
print *, 'Token:', trim(response%token)
print *, 'URL:', trim(response%url)
print *, 'Retention:', trim(response%retentionPeriod)
print *, 'Options/hideFilename:', response%options%hideFilename
print *, 'Options/oneTimeDownload:', response%options%oneTimeDownload
print *, 'Options/protected:', response%options%protected
```

### Delete File<a id="delete-file"></a>

To delete a file, you must supply your token to the `deletefile` function.

This function takes the following options as parameters:

| Option  | Type     | Description                              | Required | Extra info |
|---------|----------|------------------------------------------|----------|------------|
| `token` | `string` | The token of the file you wish to delete | true     |            |

Standard delete:

```fortran
logical :: delete_response

delete_response = deleteFile(token)
print *, '--Delete File Response--'
print *, 'Response:', delete_response
```

### Get File<a id="get-file"></a>

This lib also supports obtaining a file from the API as a Buffer by supplying either the token or the unique identifier
of the file (epoch/filename).

Use the `getFile` function. This function takes the following options an object:

| Option     | Type     | Description                                | Required                           | Extra info                                      |
|------------|----------|--------------------------------------------|------------------------------------|-------------------------------------------------|
| `token`    | `string` | The token of the file you want to download | true only if `filename` is not set | if `filename` is set, then this can not be used |
| `url`      | `string` | The URL of the file                        | true only if `token` is not set    | if `token` is set, then this can not be used    |
| `download` | `buffer` | Allocatable to hold download               | true                               | Passed as a parameter on the function call      |
| `password` | `string` | The password for the file                  | true if file is encrypted          | Passed as a parameter on the function call      |

> **Important!** The Unique identifier filename is the epoch/filename only if the file uploaded did not have a hidden
> filename, if it did, then it's just the epoch.
> For example: `1710111505084/08.png` is the Unique identifier for a standard upload of a file called `08.png`, if this
> was uploaded with hidden filename, then it would be `1710111505084.png`

Obtain an encrypted file

```fortran
type(response_type) :: filebuffer
type(file_response) :: response

call getFile(response, filebuffer, 'dangerWaifu')
print *, '--Download File--'
print *, 'Size: ', len(filebuffer%content)
! Do something with buffer
deallocate(filebuffer%content)
```