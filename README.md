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
make curl
```

After that you can then build the waifuvault module.

```sh
make sdk
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

After each call to the SDK, you need to use a call to `getError` to check for any errors during the call.  If it returns status 0, then all was OK.

After you have checked the error, you must consume it with a call to `clearError`.

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
    call clearError()
end if
```

## Usage

This API contains 11 interactions:

1. [Upload File](#upload-file)
2. [Get File Info](#get-file-info)
3. [Update File Info](#update-file-info)
4. [Delete File](#delete-file)
5. [Get File](#get-file)
6. [Create Bucket](#create-bucket)
7. [Delete Bucket](#delete-bucket)
8. [Get Bucket](#get-bucket)
9. [Get Restrictions](#get-restrictions)
10. [Clear Restrictions](#clear-restrictions)
11. [Set Alternate Base URL](#set-alt-baseurl)

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
| `bucketToken`     | `string`     | The token of a bucket to upload the file to                     | false          |                                  |
| `url`             | `string`     | The URL of the file to target                                   | true if URL    | Filename with extension          |
| `buffer`          | `byte array` | Byte array containing file to upload                            | true if buffer | Needs filename set also          |
| `expires`         | `string`     | A string containing a number and a unit (1d = 1day)             | false          | Valid units are `m`, `h` and `d` |
| `hideFilename`    | `logical`    | If true, then the uploaded filename won't appear in the URL     | false          | Defaults to `false`              |
| `password`        | `string`     | If set, then the uploaded file will be encrypted                | false          |                                  |
| `oneTimeDownload` | `logical`    | if supplied, the file will be deleted as soon as it is accessed | false          |                                  |

> **NOTE:** If you use `GetRestrictions` then server restrictions are checked by the SDK client side *before* upload, and will record an error if they are violated

Using a URL:

```fortran
type(file_upload) :: url_upload
type(file_response) :: response
type(error_response) :: error

call url_upload%create_upload('https://waifuvault.moe/assets/custom/images/08.png', '', '10m', '', .false., .false.)
response = uploadFile(url_upload)
call getError(error)
if (error%status > 0) then
    print *, '--Error Object--'
    print *, 'Name:', trim(error%name)
    print *, 'Status:', error%status
    print *, 'Message:', trim(error%message)
    print *, ''
    call clearError()
else
    print *, '--URL Upload Response--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, ''
end if
```

Using a file path:

```fortran
type(file_upload) :: upload
type(file_response) :: response
type(error_response) :: error

call upload%create_upload('./acoolfile.png', '', '10m', '', .false., .false.)
response = uploadFile(upload)
call getError(error)
if (error%status > 0) then
    print *, '--Error Object--'
    print *, 'Name:', trim(error%name)
    print *, 'Status:', error%status
    print *, 'Message:', trim(error%message)
    print *, ''
    call clearError()
else
    print *, '--File Upload Response--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, ''
end if
```

Using a file path to a bucket:

```fortran
type(file_upload) :: upload
type(file_response) :: response
type(error_response) :: error

call upload%create_upload('./acoolfile.png', 'some-bucket-token', '10m', '', .false., .false.)
response = uploadFile(upload)
call getError(error)
if (error%status > 0) then
    print *, '--Error Object--'
    print *, 'Name:', trim(error%name)
    print *, 'Status:', error%status
    print *, 'Message:', trim(error%message)
    print *, ''
    call clearError()
else
    print *, '--File Upload Response--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, ''
end if
```

Using a buffer:

```fortran
type(file_upload) :: buffer_upload
type(file_response) :: response
type(error_response) :: error
integer :: iostatus

buffer_upload%filename = 'RoryMercuryFromBuffer.png'
buffer_upload%url = ''  !IMPORTANT to init url empty
buffer_upload%bucketToken = ''  !IMPORTANT to init bucket empty
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
call getError(error)
if (error%status > 0) then
    print *, '--Error Object--'
    print *, 'Name:', trim(error%name)
    print *, 'Status:', error%status
    print *, 'Message:', trim(error%message)
    print *, ''
    call clearError()
else
    print *, '--File Upload Response--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, ''
end if
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

### Create Bucket<a id="create-bucket"></a>

Buckets are virtual collections that are linked to your IP and a token. When you create a bucket, you will receive a bucket token that you can use in Get Bucket to get all the files in that bucket

> **NOTE:** Only one bucket is allowed per client IP address, if you call it more than once, it will return the same bucket token

To create a bucket, use the `createBucket` function. This function does not take any arguments.

```fortran
type(bucket_response) :: response

response = createBucket()
print *, '--Create Bucket Response--'
print *, 'Token:', trim(response%token)
```

### Delete Bucket<a id="delete-bucket"></a>

Deleting a bucket will delete the bucket and all the files it contains.

> **IMPORTANT:**  All contained files will be **DELETED** along with the Bucket!

To delete a bucket, you must call the `deleteBucket` function with the following options as parameters:

| Option      | Type      | Description                       | Required | Extra info        |
|-------------|-----------|-----------------------------------|----------|-------------------|
| `token`     | `string`  | The token of the bucket to delete | true     |                   |

> **NOTE:** `deleteBucket` will only ever either return `true` or throw an exception if the token is invalid

```fortran
logical :: delete_response

delete_response = deleteBucket(response%token)
print *, '--Delete Bucket Response--'
print *, 'Response:', delete_response
```

### Get Bucket<a id="get-bucket"></a>

To get the list of files contained in a bucket, you use the `getBucket` function and supply the token.
This function takes the following options as parameters:

| Option      | Type      | Description             | Required | Extra info        |
|-------------|-----------|-------------------------|----------|-------------------|
| `token`     | `string`  | The token of the bucket | true     |                   |

This will respond with the bucket and all the files the bucket contains.

```fortran
type(bucket_response) :: get_response

get_response = getBucket(trim(response%token))
print *, '--Get Bucket Response--'
print *, 'Token:', trim(get_response%token)
print *, 'File 1 Token:', trim(get_response%files(1)%token)
print *, 'File 1 URL:', trim(get_response%files(1)%url)
print *, 'File 2 Token:', trim(get_response%files(2)%token)
print *, 'File 2 URL:', trim(get_response%files(2)%url)
```

### Get Restrictions<a id="get-restrictions"></a>

To get the list of restrictions applied to the server, you use the `getRestrictions` function.

This will respond with an array of name, value entries describing the restrictions applied to the server.

> **NOTE:** This loads the server restrictions into the SDK and they will be validated client side before attempting to send

```fortran
type(restriction_response) :: response

! Get Restrictions
response = getRestrictions()
print *, '--Get Restrictions Response--'
print *, 'First Entry:', trim(response%restrictions(1)%type), " ", trim(response%restrictions(1)%value)
print *, 'Second Entry:', trim(response%restrictions(2)%type), " ", trim(response%restrictions(2)%value)
print *, 'Third Entry:', trim(response%restrictions(3)%type), " ", trim(response%restrictions(3)%value)
```

### Clear Restrictions<a id="clear-restrictions"></a>

To clear the loaded restrictions in the SDK, you use the `clearRestrictions` function.

This will remove the loaded restrictions and the SDK will no longer validate client side.

```fortran
type(restriction_response) :: response

! Clear Restrictions
response = clearRestrictions()
print *, '--Clear Restrictions Response--'
print *, 'First Entry:', trim(response%restrictions(1)%type), " ", trim(response%restrictions(1)%value)
print *, 'Second Entry:', trim(response%restrictions(2)%type), " ", trim(response%restrictions(2)%value)
print *, 'Third Entry:', trim(response%restrictions(3)%type), " ", trim(response%restrictions(3)%value)
```

### Set Alternate Base URL<a id="set-alt-baseurl"></a>

To set a custom base URL in the SDK, you use the `setAltBaseURL` function.

This will change the base URL used for all functions within the SDK.

```fortran
! Set Alternate Base URL
call setAltBaseURL("https://waifuvault.walker.moe/rest")
print *, '--Set Alternate Base URL--'
```