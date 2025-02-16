! Test Waifuvault albums
program test_waifuvault_albums
    use http_callback
    use waifuvault_models
    use waifuvault_api
    implicit none

    type(file_upload) :: realfile_upload
    type(file_response) :: realfile1_response, realfile2_response
    type(album_response) :: create_album_response, get_album_response, associate_album_response, disassociate_album_response
    type(bucket_response) :: response, get_response
    type(response_type) :: buffer
    logical :: delete_response, delete_album_response, revoke_album_response
    character(len=4096) :: share_album_response
    character(len=80), dimension(100) :: file_tokens
    integer, dimension(256) :: files

    call openCurl()

    ! Set Alternate Base URL
    call setAltBaseURL("https://waifuvault.walker.moe/rest")
    print *, '--Set Alternate Base URL--'

    ! Create Bucket
    response = createBucket()
    print *, '--Create Bucket Response--'
    print *, 'Token:', trim(response%token)
    print *, ''
    call sleep(1)

    ! Upload Two Files
    call realfile_upload%create_upload('~/Downloads/rider3.png', trim(response%token), '10m', '', .false., .false.)
    realfile1_response = uploadFile(realfile_upload)
    print *, '--File Upload One Response--'
    print *, 'Token:', trim(realfile1_response%token)
    print *, 'URL:', trim(realfile1_response%url)
    print *, ''
    call sleep(1)

    call realfile_upload%create_upload('~/Downloads/rory2.jpg', trim(response%token), '10m', '', .false., .false.)
    realfile2_response = uploadFile(realfile_upload)
    print *, '--File Upload Two Response--'
    print *, 'Token:', trim(realfile2_response%token)
    print *, 'URL:', trim(realfile2_response%url)
    print *, ''
    call sleep(1)

    ! Create Album
    create_album_response = createAlbum(trim(response%token), 'test-album');
    print *, '--Create Album Response--'
    print *, 'Album Token:', trim(create_album_response%token)
    print *, 'Album Name:', trim(create_album_response%name)
    print *, ''
    call sleep(1)

    ! Associate Files
    file_tokens(1) = trim(realfile1_response%token)
    file_tokens(2) = trim(realfile2_response%token)
    associate_album_response = associateFiles(trim(create_album_response%token),file_tokens,2)
    print *, '--Associate Album Response--'
    print *, 'Album Token:', trim(associate_album_response%token)
    print *, 'Album Name:', trim(associate_album_response%name)
    print *, 'File 1 token:', trim(associate_album_response%files(1)%token)
    print *, 'File 2 token:', trim(associate_album_response%files(2)%token)
    print *, ''
    call sleep(1)

    ! Share Album
    share_album_response = shareAlbum(trim(create_album_response%token))
    print *, '--Share Album Response--'
    print *, 'Response:', trim(share_album_response)
    print *, ''
    call sleep(1)

    ! Get Album
    get_album_response = getAlbum(trim(create_album_response%token));
    print *, '--Get Album Response--'
    print *, 'Album Token:', trim(get_album_response%token)
    print *, 'Album Name:', trim(get_album_response%name)
    print *, 'Album Public Token:', trim(get_album_response%publicToken)
    print *, ''
    call sleep(1)

    ! Revoke Album
    revoke_album_response = revokeAlbum(trim(create_album_response%token))
    print *, '--Revoke Album Response--'
    print *, 'Response:', revoke_album_response
    print *, ''
    call sleep(1)

    ! Download Album
    call downloadAlbum(trim(create_album_response%token), files, 0, buffer)
    print *, '--Download Album Response--'
    print *, 'File Length:', len_trim(buffer%content)
    print *, ''
    call sleep(1)

    ! Disassociate Files
    file_tokens(1) = trim(realfile1_response%token)
    file_tokens(2) = trim(realfile2_response%token)
    disassociate_album_response = disassociateFiles(trim(create_album_response%token),file_tokens,2)
    print *, '--Disassociate Album Response--'
    print *, 'Album Token:', trim(disassociate_album_response%token)
    print *, 'Album Name:', trim(disassociate_album_response%name)
    print *, 'File count:', disassociate_album_response%filecount
    print *, ''
    call sleep(1)

    ! Delete Album
    delete_album_response = deleteAlbum(trim(create_album_response%token), .false.)
    print *, '--Delete Album Response--'
    print *, 'Response:', delete_album_response
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
end program test_waifuvault_albums