! Test Waifuvault Restrictions module
program test_waifuvault_restrictions
    use http_callback
    use waifuvault_models
    use waifuvault_api
    implicit none

    type(restriction_response) :: response
    type(file_upload) :: realfile_upload
    type(file_response) :: realfile_response
    type(error_response) :: error

    call openCurl()

    ! Get Restrictions
    response = getRestrictions()
    print *, '--Get Restrictions Response--'
    print *, 'First Entry:', trim(response%restrictions(1)%type), " ", trim(response%restrictions(1)%value)
    print *, 'Second Entry:', trim(response%restrictions(2)%type), " ", trim(response%restrictions(2)%value)
    print *, 'Third Entry:', trim(response%restrictions(3)%type), " ", trim(response%restrictions(3)%value)
    print *, ''
    call sleep(1)

    ! Upload Bad File
    call realfile_upload%create_upload('~/Dropbox/Public/filebundler.exe', '', '10m', '', .false., .false.)
    realfile_response = uploadFile(realfile_upload)
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
    call realfile_upload%create_upload('~/Downloads/rory2.jpg', '', '10m', '', .false., .false.)
    realfile_response = uploadFile(realfile_upload)
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

    ! Clear Restrictions
    response = clearRestrictions()
    print *, '--Clear Restrictions Response--'
    print *, 'First Entry:', trim(response%restrictions(1)%type), " ", trim(response%restrictions(1)%value)
    print *, 'Second Entry:', trim(response%restrictions(2)%type), " ", trim(response%restrictions(2)%value)
    print *, 'Third Entry:', trim(response%restrictions(3)%type), " ", trim(response%restrictions(3)%value)
    print *, ''

end program test_waifuvault_restrictions