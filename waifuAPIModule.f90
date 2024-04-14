! Waifuvault API module
#define BASEURL "https://waifuvault.walker.moe/rest"
module waifuvault_api
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use waifuvault_models
    use curl
    use http_callback
    implicit none

    type(error_response) error
    type(c_ptr) curl_ptr

    public :: openCurl, closeCurl, fileInfo, response_callback
    private

    contains

        subroutine openCurl()
            integer :: rc
            rc = curl_global_init(CURL_GLOBAL_DEFAULT)
            curl_ptr = curl_easy_init()
            if (.not. c_associated(curl_ptr)) then
                stop 'Error: curl init failed'
            end if
        end subroutine openCurl

        subroutine closeCurl()
            call curl_easy_cleanup(curl_ptr)
            call curl_global_cleanup()
        end subroutine closeCurl

        function uploadFile(fileObj) result (res)
            type(file_upload) :: fileObj
            type(file_response) :: res
            ! Build URL
            ! If URL upload with URL params
            ! If file upload with file params
            ! If buffer upload with buffer params
            ! PUT request
            ! Deserialize response
            ! Check for errors
        end function uploadFile

        function fileInfo(token, formatted) result (res)
            type(file_response) :: res
            type(response_type), target :: body
            character(len=*) :: token
            character(len=512) :: url
            logical :: formatted
            integer :: rc

            url = ''
            url = trim(BASEURL) // '/' // trim(token) // '?formatted='
            if (formatted .eqv. .true.) then
                url = trim(url) // 'true'
            else
                url = trim(url) // 'false'
            end if

            curl_ptr = curl_easy_init()
            rc = curl_easy_setopt(curl_ptr, CURLOPT_URL, trim(url))
            rc = curl_easy_setopt(curl_ptr, CURLOPT_HTTPGET, 1)
            rc = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION, 1)
            rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION, c_funloc(response_callback))
            rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA, c_loc(body))
            rc = curl_easy_perform(curl_ptr)
            if (rc /= CURLE_OK) stop curl_easy_strerror(rc)
            ! Deserialize response
            print *, "Body:", body%content
            print *
            ! Check for errors
        end function fileInfo

        function fileUpdate(token, password, previous_password, custom_expiry, hide_filename) result (res)
            type(file_response) :: res
            character(len=*) :: token, password, previous_password, custom_expiry
            logical :: hide_filename
            ! Build update URL
            ! Build update headers based on params
            ! HTTP PATCH request
            ! Deserialize response
            ! Check for errors
        end function fileUpdate

        function deleteFile(token) result (res)
            character(len=*) :: token
            logical :: res
            ! Build delete URL
            ! HTTP DELETE request
            ! Check for errors
            ! Return response bool
        end function deleteFile

        subroutine getFile(fileObj, buffer, password)
            character(len=*) :: password
            type(file_response) :: fileObj
            type(memory_stream) :: buffer
            ! if token and no filename, get info on token
            ! build URL
            ! if password set, then add x-password header
            ! HTTP GET file to buffer
            ! Check for errors
        end subroutine getFile

        function checkError(resp_code, body) result(res)
            character(len=*) :: body
            integer :: resp_code
            logical :: res
            res = .false.
            ! Check for CURL failure, print, return true
            ! Check resp_code > 400
            ! If so deserialize error response
            ! and return true
        end function checkError

        function deserializeResponse(body, string_retention) result (res)
            character(len=*) :: body
            logical :: string_retention
            type(file_options) :: options
            type(file_response) :: res
            ! If string_retention deserialize assuming retention is a string
            ! else deserialize as retention being int
            ! deserialize options
        end function deserializeResponse
end module