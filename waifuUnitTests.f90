! Waifuvault Fortran Unit Tests
program waifuvault_unit_tests
    use http_callback
    use waifuvault_models
    use waifuvault_mocks
    use waifuvault_api
    implicit none

    ! Tests
    call openCurl()
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