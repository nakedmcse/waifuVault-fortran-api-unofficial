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
    call test_delete()
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
end program waifuvault_unit_tests