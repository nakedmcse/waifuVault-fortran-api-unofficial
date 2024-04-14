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
            character(len=:), allocatable :: splits(:)
            character(len=:), allocatable :: cleaned
            logical :: formatted
            integer :: rc,i

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
            call checkError(rc, body%content)
            res = deserializeResponse(body%content)
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

        subroutine checkError(resp_code, body)
            character(len=*) :: body
            integer :: resp_code
            if (resp_code /= CURLE_OK) stop curl_easy_strerror(resp_code)
            if (index(body, '"status":') > 0) stop body
        end subroutine checkError

        function deserializeResponse(body) result (res)
            character(len=*) :: body
            character(len=:), allocatable :: splits(:)
            character(len=:), allocatable :: vals(:)
            character(len=:), allocatable :: cleaned
            logical :: string_retention
            type(file_options) :: options
            type(file_response) :: res
            integer :: i

            call split_string(trim(body), ',', splits)
            do i = 1, size(splits)
                cleaned = ''
                call remove_characters(trim(splits(i)),'"{}',cleaned)
                call split_string(cleaned, ':', vals)
                if (vals(1) == 'token') then
                    res%token = trim(vals(2))
                elseif (vals(1) == 'url') then
                    res%url = trim(vals(2)) // ':' // trim(vals(3))
                elseif (vals(1) == 'retentionPeriod') then
                    res%retentionPeriod = trim(vals(2))
                elseif (vals(1) == 'options') then
                    options%hideFilename = stringToLogical(vals(3))
                elseif (vals(1) == 'oneTimeDownload') then
                    options%oneTimeDownload = stringToLogical(vals(2))
                elseif (vals(1) == 'protected') then
                    options%protected = stringToLogical(vals(2))
                end if
            end do
            res%options = options
        end function deserializeResponse

        function stringToLogical(input) result (res)
            character(len=*) :: input
            logical :: res
            if(input == 'true') then
                res = .true.
            else
                res = .false.
            end if
        end function stringToLogical

        subroutine split_string(input_string, delimiter, substrings)
            character(len=*), intent(in) :: input_string
            character(len=1), intent(in) :: delimiter
            character(len=:), allocatable, intent(out) :: substrings(:)

            integer :: start, delim_pos, num_substrings, i
            character(len=len(input_string)) :: temp_string

            temp_string = input_string
            num_substrings = 1
            start = 1

            ! Count number of substrings to allocate array
            do while (index(temp_string, delimiter) /= 0)
                num_substrings = num_substrings + 1
                delim_pos = index(temp_string, delimiter)
                temp_string = temp_string(delim_pos+1:)
            end do

            ! Allocate the substrings array
            allocate(character(len=len(input_string)) :: substrings(num_substrings))

            temp_string = input_string
            i = 1
            do while (index(temp_string, delimiter) /= 0)
                delim_pos = index(temp_string, delimiter)
                substrings(i) = temp_string(:delim_pos-1)
                temp_string = temp_string(delim_pos+1:)
                i = i + 1
            end do

            ! Last part after the last delimiter
            substrings(i) = temp_string
        end subroutine split_string

        subroutine remove_characters(original_string, chars_to_remove, new_string)
            character(len=*), intent(in) :: original_string
            character(len=*), intent(in) :: chars_to_remove
            character(len=:), allocatable, intent(out) :: new_string

            integer :: i, j
            logical :: is_remove_char
            character(len=len(original_string)) :: temp_string

            temp_string = ''

            do i = 1, len_trim(original_string)
                is_remove_char = .false.

                do j = 1, len_trim(chars_to_remove)
                    if (original_string(i:i) == chars_to_remove(j:j)) then
                        is_remove_char = .true.
                        exit
                    endif
                end do

                if (.not. is_remove_char) then
                    if (original_string(i:i) == ' ') then
                        temp_string = trim(temp_string) // '_'
                    else
                        temp_string = trim(temp_string) // original_string(i:i)
                    end if
                endif
            end do

            do i = 1, len_trim(temp_string)
                if(temp_string(i:i) == '_') then
                    temp_string(i:i) = ' '
                end if
            end do

            new_string = trim(temp_string)
        end subroutine remove_characters
end module