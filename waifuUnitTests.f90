! Waifuvault Fortran Unit Tests
program waifuvault_unit_tests
    use http_callback
    use waifuvault_models
    use waifuvault_mocks
    use waifuvault_api
    implicit none

#define ASSERT(condition, message) \
    if (.not. (condition)) then; \
        print *, "Assertion failed: ", message; \
        print *, "File: ", __FILE__, ", Line: ", __LINE__; \
        error stop; \
    endif

    ! Tests
    call test_delete()

    contains
        subroutine test_delete()
            ! Given
            logical :: result
            call dispatch_mock%clear_dispatch_mock()
            dispatch_mock%response%content = response_delete_true

            ! When
            result = deleteFile("test-token")

            ! Then
            ASSERT(dispatch_mock%calls == 1, "Delete should call dispatch exactly once")
            ASSERT(result, "Delete should return true")
            ASSERT(dispatch_mock%target_method == "DELETE", "Delete should use DELETE method")
            ASSERT(dispatch_mock%target_url == "https://waifuvault.moe/rest/test-token", "Delete target URL is wrong")
            print *,"Delete test passed"
        end subroutine test_delete
end program waifuvault_unit_tests