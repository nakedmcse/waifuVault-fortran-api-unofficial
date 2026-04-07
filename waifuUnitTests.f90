! Waifuvault Fortran Unit Tests
program waifuvault_unit_tests
    use http_callback
    use waifuvault_models
    use waifuvault_mocks
    use waifuvault_api
    implicit none

    ! Tests
    call openCurl()
    call test_url_upload()
    call test_file_upload()
    call test_file_info()
    call test_file_update()
    call test_create_bucket()
    call test_get_bucket()
    call test_delete_bucket()
    call test_create_album()
    call test_share_album()
    call test_revoke_album()
    call test_associate_files()
    call test_disassociate_files()
    call test_download_album()
    call test_get_album()
    call test_delete_album()
    call test_delete()
    call test_get_restrictions()
    call test_clear_restrictions()
    call closeCurl()

    contains
        subroutine assert(condition, message)
            logical :: condition
            character(len=*) :: message
            if (.not.(condition)) then
                print *, "Assertion failed: ", message
                error stop
            end if
        end subroutine assert

        subroutine test_url_upload()
            ! Given
            type(file_upload) :: upload
            type(file_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_fileInfoOK
            call upload%create_upload("https://waifuvault.moe/assets/custom/images/08.png","","10m","",.false.,.false.)
            ! When
            res = uploadFile(upload)
            ! Then
            call assert(dispatch_mock%calls == 1, "URL Upload should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "PUT", "URL Upload should use PUT method")
            call assert(res%url == "https://waifuvault.moe/f/something", "URL Upload response URL wrong")
            call assert(res%token == "test-token", "URL Upload response token wrong" // res%token)
            call assert(.not. res%options%protected, "URL Upload response options wrong")
            call assert(res%retentionPeriod == "100", "URL Upload retention period wrong")
            print *,"URL Upload test passed"
        end subroutine test_url_upload

        subroutine test_file_upload()
            ! Given
            type(file_upload) :: upload
            type(file_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_fileInfoOK
            call upload%create_upload("RoryMercury.png","","10m","",.false.,.false.)
            ! When
            res = uploadFile(upload)
            ! Then
            call assert(dispatch_mock%calls == 1, "File Upload should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "PUT", "File Upload should use PUT method")
            call assert(res%url == "https://waifuvault.moe/f/something", "File Upload response URL wrong")
            call assert(res%token == "test-token", "File Upload response token wrong" // res%token)
            call assert(.not. res%options%protected, "File Upload response options wrong")
            call assert(res%retentionPeriod == "100", "File Upload retention period wrong")
            print *,"File Upload test passed"
        end subroutine test_file_upload

        subroutine test_file_info()
            ! Given
            type(file_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_fileInfoOKText
            ! When
            res = fileInfo("test-token", .true.)
            ! Then
            call assert(dispatch_mock%calls == 1, "File Info should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "GET", "File Info should use GET method")
            call assert(res%url == "https://waifuvault.moe/f/something", "File Info response URL wrong")
            call assert(res%token == "test-token", "File Info response token wrong" // res%token)
            call assert(.not. res%options%protected, "File Info response options wrong")
            call assert(res%retentionPeriod == "10 minutes", "File Info retention period wrong")
            print *,"File Info test passed"
        end subroutine test_file_info

        subroutine test_file_update()
            ! Given
            type(file_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_fileInfoOKText
            ! When
            res = fileUpdate("test-token", "password", "previous", "exp", .false.)
            ! Then
            call assert(dispatch_mock%calls == 1, "File Update should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "PATCH", "File Update should use PATCH method")
            call assert(dispatch_mock%fields == '{"password":"password","previousPassword":"previous","customExpiry":"exp","hideFilename":false}', "File Update fields wrong")
            call assert(res%url == "https://waifuvault.moe/f/something", "File Update response URL wrong")
            call assert(res%token == "test-token", "File Update response token wrong" // res%token)
            call assert(.not. res%options%protected, "File Update response options wrong")
            call assert(res%retentionPeriod == "10 minutes", "File Update retention period wrong")
            print *,"File Update test passed"
        end subroutine test_file_update

        subroutine test_create_bucket()
            ! Given
            type(bucket_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_empty_bucket
            ! When
            res = createBucket()
            ! Then
            call assert(dispatch_mock%calls == 1, "Create Bucket should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "GET", "Create Bucket should use GET method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/bucket/create", "Create Bucket URL is wrong")
            call assert(res%token == "test-bucket", "Create Bucket return token wrong")
            print *,"Create Bucket test passed"
        end subroutine test_create_bucket

        subroutine test_get_bucket()
            ! Given
            type(bucket_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_used_bucket
            ! When
            res = getBucket("test-token")
            ! Then
            call assert(dispatch_mock%calls == 1, "Get Bucket should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "POST", "Get Bucket should use POST method")
            call assert(dispatch_mock%fields == '{"bucket_token":"test-token"}', "Get Bucket fields wrong")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/bucket/get", "Get Bucket target URL wrong")
            call assert(res%filecount == 2, "Get Bucket files count wrong")
            call assert(res%files(1)%token == "0dd4b9b5-1e7e-4852-bdc5-54a79feb07c9", "Get Bucket file token 1 wrong")
            call assert(res%albumcount == 1, "Get Bucket album count wrong")
            call assert(res%albums(1)%token == "b96413f7-2e34-4691-8f44-6b9fcf83ca7c", "Get Bucket album token 1 wrong")
            print *,"Get Bucket test passed"
        end subroutine test_get_bucket

        subroutine test_delete_bucket()
            ! Given
            logical :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_delete_true
            ! When
            res = deleteBucket("test-token")
            ! Then
            call assert(dispatch_mock%calls == 1, "Delete Bucket should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "DELETE", "Delete Bucket should use DELETE method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/bucket/test-token", "Delete Bucket URL is wrong")
            call assert(res, "Delete Bucket should return true")
            print *,"Delete Bucket test passed"
        end subroutine test_delete_bucket

        subroutine test_create_album()
            ! Given
            type(album_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_album_new
            ! When
            res = createAlbum("test-bucket","test-name")
            ! Then
            call assert(dispatch_mock%calls == 1, "Create Album should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "POST", "Create Album should use POST method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/album/test-bucket", "Create Album URL is wrong")
            call assert(dispatch_mock%fields == '{"name":"test-name"}', "Create Album fields are wrong")
            call assert(res%name == "test-name", "Create Album response name wrong")
            call assert(res%token == "test-album", "Create Album response token wrong")
            print *,"Create Album test passed"
        end subroutine test_create_album

        subroutine test_share_album()
            ! Given
            character(len=:), allocatable :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_general_true
            ! When
            res = shareAlbum("test-album")
            ! Then
            call assert(dispatch_mock%calls == 1, "Share Album should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "GET", "Share Album should use GET method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/album/share/test-album", "Share Album URL is wrong")
            call assert(res == "yes", "Share Album response wrong")
            print *,"Share Album test passed"
        end subroutine test_share_album

        subroutine test_revoke_album()
            ! Given
            logical :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_general_true
            ! When
            res = revokeAlbum("test-album")
            ! Then
            call assert(dispatch_mock%calls == 1, "Revoke Album should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "GET", "Revoke Album should use GET method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/album/revoke/test-album", "Revoke Album URL is wrong")
            call assert(res, "Revoke Album response wrong")
            print *,"Revoke Album test passed"
        end subroutine test_revoke_album

        subroutine test_download_album()
            ! Given
            integer, dimension(256) :: files
            type(response_type) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_file
            files(1) = 6
            files(2) = 7
            ! When
            call downloadAlbum("album-token",files,2,res)
            ! Then
            call assert(dispatch_mock%calls == 1, "Download Album should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "POST", "Download Album should use POST method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/album/download/album-token", "Download Album URL is wrong")
            call assert(dispatch_mock%fields == '[6,7]', "Download Album fields are wrong" // " " // dispatch_mock%fields)
            call assert(res%content == response_file, "Download Album contents are wrong")
            print *,"Download Album test passed"
        end subroutine test_download_album

        subroutine test_associate_files()
            ! Given
            type(album_response) :: res
            character(len=80), dimension(100) :: files
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_album_with_files
            files(1) = "file-token-1"
            files(2) = "file-token-2"
            ! When
            res = associateFiles("album-token",files,2)
            ! Then
            call assert(dispatch_mock%calls == 1, "Associate Files should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "POST", "Associate Files should use POST method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/album/album-token/associate", "Associate Files URL is wrong")
            call assert(dispatch_mock%fields == '{"fileTokens":["file-token-1","file-token-2"]}', "Associate Files fields are wrong")
            call assert(res%filecount == 2, "Associate Files response files count wrong")
            call assert(res%name == "Something", "Associate Files response album name wrong")
            call assert(res%token == "b96413f7-2e34-4691-8f44-6b9fcf83ca7c", "Associate Files response album token wrong")
            call assert(res%publicToken == "ce8c7459-b26f-4844-b65a-4d1668308c8e", "Associate Files response album public token wrong")
            call assert(res%bucket == "56a62473-d3ef-48f9-baef-3628a3d23549", "Associate Files response album bucket token wrong")
            call assert(res%files(1)%token == "bb183720-58eb-44d6-9eff-d72536edf302", "Associate Files response file token 1 wrong")
            call assert(res%files(2)%token == "49cc14d8-c4da-410a-91f7-09848f1e8466", "Associate Files response file token 2 wrong")
            print *,"Associate Files test passed"
        end subroutine test_associate_files

        subroutine test_disassociate_files()
            ! Given
            type(album_response) :: res
            character(len=80), dimension(100) :: files
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_album_with_files
            files(1) = "file-token-1"
            files(2) = "file-token-2"
            ! When
            res = disassociateFiles("album-token",files,2)
            ! Then
            call assert(dispatch_mock%calls == 1, "Disassociate Files should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "POST", "Disassociate Files should use POST method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/album/album-token/disassociate", "Disassociate Files URL is wrong")
            call assert(dispatch_mock%fields == '{"fileTokens":["file-token-1","file-token-2"]}', "Disassociate Files fields are wrong")
            call assert(res%filecount == 2, "Disassociate Files response files count wrong")
            call assert(res%name == "Something", "Disassociate Files response album name wrong")
            call assert(res%token == "b96413f7-2e34-4691-8f44-6b9fcf83ca7c", "Disassociate Files response album token wrong")
            call assert(res%publicToken == "ce8c7459-b26f-4844-b65a-4d1668308c8e", "Disassociate Files response album public token wrong")
            call assert(res%bucket == "56a62473-d3ef-48f9-baef-3628a3d23549", "Disassociate Files response album bucket token wrong")
            call assert(res%files(1)%token == "bb183720-58eb-44d6-9eff-d72536edf302", "Disassociate Files response file token 1 wrong")
            call assert(res%files(2)%token == "49cc14d8-c4da-410a-91f7-09848f1e8466", "Disassociate Files response file token 2 wrong")
            print *,"Disassociate Files test passed"
        end subroutine test_disassociate_files

        subroutine test_get_album()
            ! Given
            type(album_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_album_with_files
            ! When
            res = getAlbum("test-token")
            ! Then
            call assert(dispatch_mock%calls == 1, "Get Album should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "GET", "Get Album should use GET method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/album/test-token", "Get Album target URL wrong")
            call assert(res%filecount == 2, "Get Album files count wrong")
            call assert(res%name == "Something", "Get Album album name wrong")
            call assert(res%token == "b96413f7-2e34-4691-8f44-6b9fcf83ca7c", "Get Album album token wrong")
            call assert(res%publicToken == "ce8c7459-b26f-4844-b65a-4d1668308c8e", "Get Album album public token wrong")
            call assert(res%bucket == "56a62473-d3ef-48f9-baef-3628a3d23549", "Get Album album bucket token wrong")
            call assert(res%files(1)%token == "bb183720-58eb-44d6-9eff-d72536edf302", "Get Album file token 1 wrong")
            call assert(res%files(2)%token == "49cc14d8-c4da-410a-91f7-09848f1e8466", "Get Album file token 2 wrong")
            print *,"Get Album test passed"
        end subroutine test_get_album

        subroutine test_delete_album()
            ! Given
            logical :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_general_true
            ! When
            res = deleteAlbum("test-album",.true.)
            ! Then
            call assert(dispatch_mock%calls == 1, "Delete album should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "DELETE", "Delete Album should use DELETE method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/album/test-album?deleteFiles=true", "Delete Album target URL wrong")
            call assert(res,"Delete Album should return true")
            print *,"Delete Album test passed"
        end subroutine test_delete_album

        subroutine test_delete()
            ! Given
            logical :: result
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_delete_true

            ! When
            result = deleteFile("test-token")

            ! Then
            call assert(dispatch_mock%calls == 1, "Delete should call dispatch exactly once")
            call assert(result, "Delete should return true")
            call assert(dispatch_mock%target_method == "DELETE", "Delete should use DELETE method: " // dispatch_mock%target_method)
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/test-token", "Delete target URL is wrong: " // dispatch_mock%target_url)
            print *,"Delete test passed"
        end subroutine test_delete

        subroutine test_get_restrictions()
            ! Given
            type(restriction_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_restrictionsResponse
            ! When
            res = getRestrictions()
            ! Then
            call assert(dispatch_mock%calls == 1, "Get Restrictions should call dispatch exactly once")
            call assert(dispatch_mock%target_method == "GET", "Get Album should use GET method")
            call assert(dispatch_mock%target_url == "https://waifuvault.moe/rest/resources/restrictions", "Get Restrictions target URL wrong")
            call assert(res%restrictions(1)%type == "MAX_FILE_SIZE", "Get Restrictions MAX_FILE_SIZE type wrong")
            call assert(res%restrictions(1)%value == "100", "Get Restrictions MAX_FILE_SIZE value wrong")
            call assert(res%restrictions(2)%type == "BANNED_MIME_TYPE", "Get Restrictions BANNED_MIME_TYPE type wrong")
            call assert(res%restrictions(2)%value == "application/x-msdownload,application/x-executable", "Get Restrictions BANNED_MIME_TYPE value wrong")
            print *,"Get Restrictions test passed"
        end subroutine test_get_restrictions

        subroutine test_clear_restrictions()
            ! Given
            type(restriction_response) :: res
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_restrictionsResponse
            ! When
            res = clearRestrictions()
            ! Then
            call assert(dispatch_mock%calls == 0, "Clear Restrictions should not call dispatch")
            call assert(res%restrictions(1)%type == "", "Clear Restrictions first type wrong")
            call assert(res%restrictions(1)%value == "", "Clear Restrictions first value wrong")
            call assert(res%restrictions(2)%type == "", "Clear Restrictions second type wrong")
            call assert(res%restrictions(2)%value == "", "Clear Restrictions second value wrong")
            print *,"Clear Restrictions test passed"
        end subroutine test_clear_restrictions
end program waifuvault_unit_tests