! Test Waifuvault module
program test_waifuvault
    use http_callback
    use waifuvault_models
    use waifuvault_api
    implicit none

    character(len=512) :: built_url
    type(file_upload) :: url_upload, realfile_upload, buffer_upload
    type(file_options) :: options
    type(file_response) :: response
    type(error_response) :: error
    type(response_type) :: filebuffer
    integer :: iostatus
    logical :: delete_response

    ! Object Tests
    call url_upload%create_upload('https://somesite/somefile.png', '', '1d', 'somepassword', .true., .true.)

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

    call response%create_response('some-token','','https://someurl','1 Day',options)
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

    response = fileInfo('balls', .true.)
    print *, '--FileInfo Response Object--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, 'Retention:', trim(response%retentionPeriod)
    print *, 'Options/hideFilename:', response%options%hideFilename
    print *, 'Options/oneTimeDownload:', response%options%oneTimeDownload
    print *, 'Options/protected:', response%options%protected
    print *, ''
    call getError(error)
    print *, '--Error Object--'
    print *, 'Name:', trim(error%name)
    print *, 'Status:', error%status
    print *, 'Message:', trim(error%message)
    print *, ''
    call sleep(1)

    ! URL Upload
    call url_upload%create_upload('https://waifuvault.moe/assets/custom/images/08.png', '', '10m', 'dangerWaifu', .false., .false.)
    response = uploadFile(url_upload)
    print *, '--URL Upload Response Object--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, ''
    call sleep(1)

    response = fileInfo(response%token, .true.)
    print *, '--FileInfo Response Object--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, 'Retention:', trim(response%retentionPeriod)
    print *, 'Options/hideFilename:', response%options%hideFilename
    print *, 'Options/oneTimeDownload:', response%options%oneTimeDownload
    print *, 'Options/protected:', response%options%protected
    print *, ''
    call sleep(1)

    call getFile(response, filebuffer, 'dangerWaifu')
    print *, '--Download File--'
    print *, 'Size: ', len_trim(filebuffer%content)
    print *, ''
    call sleep(1)

    delete_response = deleteFile(response%token)
    print *, '--Delete File Response--'
    print *, 'Response:', delete_response
    print *, ''
    call sleep(1)

    ! File Upload
    call realfile_upload%create_upload('~/Downloads/rider3.png', '', '10m', '', .false., .false.)
    response = uploadFile(realfile_upload)
    print *, '--File Upload Response Object--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, ''
    call getError(error)
    print *, '--Error Object--'
    print *, 'Name:', trim(error%name)
    print *, 'Status:', error%status
    print *, 'Message:', trim(error%message)
    print *, ''
    call sleep(1)

    response = fileInfo(response%token, .true.)
    print *, '--FileInfo Response Object--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, 'Retention:', trim(response%retentionPeriod)
    print *, 'Options/hideFilename:', response%options%hideFilename
    print *, 'Options/oneTimeDownload:', response%options%oneTimeDownload
    print *, 'Options/protected:', response%options%protected
    print *, ''
    call sleep(1)

    response = fileUpdate(response%token, '', '', '1h', .false.)
    print *, '--FileUpdate Response Object--'
    print *, 'Token:', trim(response%token)
    print *, 'URL:', trim(response%url)
    print *, 'Retention:', trim(response%retentionPeriod)
    print *, 'Options/hideFilename:', response%options%hideFilename
    print *, 'Options/oneTimeDownload:', response%options%oneTimeDownload
    print *, 'Options/protected:', response%options%protected
    print *, ''
    call sleep(1)

    delete_response = deleteFile(response%token)
    print *, '--Delete File Response--'
    print *, 'Response:', delete_response
    call sleep(1)

    ! Buffer Upload
    buffer_upload%filename = 'RoryMercuryFromBuffer.png'
    buffer_upload%url = ''  !IMPORTANT to init url empty
    buffer_upload%expires = '10m'
    buffer_upload%password = 'dangerWaifu'
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
    print *, ''
    call sleep(1)

    delete_response = deleteFile(response%token)
    print *, '--Delete File Response--'
    print *, 'Response:', delete_response
    call sleep(1)

    call closeCurl()

end program test_waifuvault