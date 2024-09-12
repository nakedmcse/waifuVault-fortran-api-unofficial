! Test Waifuvault Restrictions module
program test_waifuvault_restrictions
    use http_callback
    use waifuvault_models
    use waifuvault_api
    implicit none

    type(restriction_response) :: response
    type(file_upload) :: badfile_upload, goodfile_upload
    type(file_response) :: realfile_response, checkinfo_response
    type(error_response) :: error

    call openCurl()

    ! Set Alternate Base URL
    call setAltBaseURL("https://waifuvault.walker.moe/rest")
    print *, '--Set Alternate Base URL--'

    ! Get Restrictions
    response = getRestrictions()
    print *, '--Get Restrictions Response--'
    print *, 'First Entry:', trim(response%restrictions(1)%type), " ", trim(response%restrictions(1)%value)
    print *, 'Second Entry:', trim(response%restrictions(2)%type), " ", trim(response%restrictions(2)%value)
    print *, 'Third Entry:', trim(response%restrictions(3)%type), " ", trim(response%restrictions(3)%value)
    print *, ''
    call sleep(1)

    ! Upload Bad File
    call badfile_upload%create_upload('~/Dropbox/Public/filebundler.exe', '', '10m', '', .false., .false.)
    realfile_response = uploadFile(badfile_upload)
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
        print *, 'Token:', trim(realfile_response%token)
        print *, 'URL:', trim(realfile_response%url)
        print *, ''
    end if
    call sleep(1)

    ! Upload Good File
    call goodfile_upload%create_upload('~/Downloads/rory2.jpg', '', '', '', .false., .false.)
    realfile_response = uploadFile(goodfile_upload)
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
        print *, 'Token:', trim(realfile_response%token)
        print *, 'URL:', trim(realfile_response%url)
        print *, 'Retention:', trim(realfile_response%retentionPeriod)
        print *, ''
    end if
    call sleep(1)

    ! Check file info
    checkinfo_response = fileInfo(trim(realfile_response%token), .false.)
    print *, '--FileInfo Response Object--'
    print *, 'Token:', trim(checkinfo_response%token)
    print *, 'URL:', trim(checkinfo_response%url)
    print *, 'Retention:', trim(checkinfo_response%retentionPeriod)
    print *, 'Options/hideFilename:', checkinfo_response%options%hideFilename
    print *, 'Options/oneTimeDownload:', checkinfo_response%options%oneTimeDownload
    print *, 'Options/protected:', checkinfo_response%options%protected

    ! Clear Restrictions
    response = clearRestrictions()
    print *, '--Clear Restrictions Response--'
    print *, 'First Entry:', trim(response%restrictions(1)%type), " ", trim(response%restrictions(1)%value)
    print *, 'Second Entry:', trim(response%restrictions(2)%type), " ", trim(response%restrictions(2)%value)
    print *, 'Third Entry:', trim(response%restrictions(3)%type), " ", trim(response%restrictions(3)%value)
    print *, ''

end program test_waifuvault_restrictions