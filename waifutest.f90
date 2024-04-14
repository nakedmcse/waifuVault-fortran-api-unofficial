! Test Waifuvault module
program test_waifuvault
    use waifuvault_models
    use waifuvault_api
    implicit none

    character(len=512) :: built_url
    type(file_upload) :: url_upload
    type(file_options) :: options
    type(file_response) :: response
    type(error_response) :: error

    call url_upload%create_upload('https://somesite/somefile.png', '1d', 'somepassword', .true., .true.)

    built_url = url_upload%build_url()
    print *, '--Upload Object Build URL--'
    print *, trim(built_url)
    print *, ''

    call options%create_options(.true., .true., .true.)
    print *, '--Options Object--'
    print *, 'hideFilename:', options%hideFilename
    print *, 'oneTimeDownload:', options%oneTimeDownload
    print *, 'protected:', options%protected
    print *, ''

    call response%create_response('some-token','https://someurl','1 Day',options)
    print *, '--Response Object--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, 'Retention:', trim(response%retentionPeriod)
    print *, 'Options/protected:', response%options%protected
    print *, ''

    call error%create_error_response('BAD REQUEST', 400, 'There was a bad request made, oopsie')
    print *, '--Error Object--'
    print *, 'Name:', trim(error%name)
    print *, 'Status:', error%status
    print *, 'Message:', trim(error%message)
    print *, ''

    call openCurl()
    response = fileInfo('a7db7d50-7fd4-45cc-8f53-f0dee4a6fa9f', .true.)
    print *, '--FileInfo Response Object--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, 'Retention:', trim(response%retentionPeriod)
    print *, 'Options/hideFilename:', response%options%hideFilename
    print *, 'Options/oneTimeDownload:', response%options%oneTimeDownload
    print *, 'Options/protected:', response%options%protected
    print *, ''
    call closeCurl()
end program test_waifuvault