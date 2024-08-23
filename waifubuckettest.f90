! Test Waifuvault Buckets module
program test_waifuvault_buckets
    use http_callback
    use waifuvault_models
    use waifuvault_api
    implicit none

    type(file_upload) :: realfile_upload
    type(file_response) :: realfile_response
    type(bucket_response) :: response, get_response
    logical :: delete_response

    call openCurl()

    ! Create Bucket
    response = createBucket()
    print *, '--Create Bucket Response--'
    print *, 'Token:', trim(response%token)
    print *, ''
    call sleep(1)

    ! Upload Two Files
    call realfile_upload%create_upload('~/Downloads/rider3.png', trim(response%token), '10m', '', .false., .false.)
    realfile_response = uploadFile(realfile_upload)
    print *, '--File Upload One Response--'
    print *, 'Token:', trim(realfile_response%token)
    print *, 'URL:', trim(realfile_response%url)
    print *, ''
    call sleep(1)

    call realfile_upload%create_upload('~/Downloads/rory2.jpg', trim(response%token), '10m', '', .false., .false.)
    realfile_response = uploadFile(realfile_upload)
    print *, '--File Upload Two Response--'
    print *, 'Token:', trim(realfile_response%token)
    print *, 'URL:', trim(realfile_response%url)
    print *, ''
    call sleep(1)

    ! Get Bucket
    get_response = getBucket(trim(response%token))
    print *, '--Get Bucket Response--'
    print *, 'Token:', trim(get_response%token)
    print *, 'File 1 Token:', trim(get_response%files(1)%token)
    print *, 'File 1 URL:', trim(get_response%files(1)%url)
    print *, 'File 2 Token:', trim(get_response%files(2)%token)
    print *, 'File 2 URL:', trim(get_response%files(2)%url)
    print *, ''
    call sleep(1)

    ! Delete Bucket
    delete_response = deleteBucket(response%token)
    print *, '--Delete Bucket Response--'
    print *, 'Response:', delete_response
    print *, ''
    call sleep(1)

end program test_waifuvault_buckets