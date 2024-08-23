! Waifuvault API module
#define BASEURL "https://waifuvault.moe/rest"
module waifuvault_api
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use waifuvault_models
    use waifuvault_utils
    use curl
    use http_callback
    implicit none

    type(error_response) error
    type(c_ptr) curl_ptr

    public :: openCurl, closeCurl, getError, fileInfo, fileUpdate, getFile, uploadFile, deleteFile, createBucket, deleteBucket, getBucket, response_callback
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

        subroutine getError(ret_error)
            type(error_response) :: ret_error
            ret_error%name = error%name
            ret_error%status = error%status
            ret_error%message = error%message
        end subroutine getError

        subroutine dispatch_curl(rc, request_type, url, headers, body, fields)
            character(len=*) :: request_type, url
            character(len=*), target :: fields
            type(response_type), target :: body
            type(c_ptr) :: headers
            integer :: rc

            curl_ptr = curl_easy_init()
            rc = curl_easy_setopt(curl_ptr, CURLOPT_URL, url)
            rc = curl_easy_setopt(curl_ptr, CURLOPT_CUSTOMREQUEST, request_type)
            rc = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION, 1)
            rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION, c_funloc(response_callback))
            rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA, c_loc(body))
            if (.not. c_associated(headers,c_null_ptr)) then
                rc = curl_easy_setopt(curl_ptr, CURLOPT_HTTPHEADER, headers)
            end if
            if (len_trim(fields) > 0) then
                rc = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDS, c_loc(fields))
                rc = curl_easy_setopt(curl_ptr, CURLOPT_POSTFIELDSIZE, len(fields))
            end if
            rc = curl_easy_perform(curl_ptr)
        end subroutine dispatch_curl

        function createBucket() result (res)
            type(response_type), target :: body
            character(len=512) :: url
            integer :: rc
            type(bucket_response) :: res

            url = ''
            url = trim(BASEURL) // '/bucket/create'

            call dispatch_curl(rc, 'GET', trim(url), c_null_ptr, body, '')
            call checkError(rc, body%content)

            res = deserializeBucketResponse(body%content)
            deallocate(body%content)
        end function createBucket

        function deleteBucket(token) result (res)
            type(response_type), target :: body
            character(len=*) :: token
            character(len=512) :: url
            integer :: rc
            logical :: res

            url = ''
            url = trim(BASEURL) // '/bucket/' // trim(token)

            call dispatch_curl(rc, 'DELETE', trim(url), c_null_ptr, body, '')
            call checkError(rc, body%content)

            res = body%content(1:4) == 'true'
            deallocate(body%content)
        end function deleteBucket

        function getBucket(token) result (res)
            character(len=*) :: token
            character(len=:), allocatable :: url, content
            type(response_type), target :: body
            type(c_ptr) :: headers = c_null_ptr
            type(bucket_response) :: res
            integer :: rc

            url = trim(BASEURL) // '/bucket/get';
            headers = curl_slist_append(headers, ('Content-Type: application/json'))
            content = '{"bucket_token": "' // trim(token) // '"}'

            call dispatch_curl(rc, 'POST', url, headers, body, content)
            call curl_slist_free_all(headers)
            call checkError(rc, body%content)

            res = deserializeBucketResponse(body%content)
            deallocate(body%content)
        end function getBucket

        function uploadFile(fileObj) result (res)
            type(file_upload) :: fileObj
            type(file_response) :: res
            type(response_type), target :: body
            type(c_ptr) :: headers = c_null_ptr
            character(len=512) :: target_url, stringsize
            character(len=:), allocatable :: fullfilename
            character(len=:), allocatable, target :: fields, seperator, filebuffer
            integer :: rc, iostatus, filesize

            target_url = fileObj%build_url()
            if (len_trim(fileObj%url) > 0) then
                ! URL Upload
                fields = 'url='
                fields = trim(fields) // curl_easy_escape(curl_ptr, trim(fileObj%url), len_trim(fileObj%url))

                if (len_trim(fileObj%password) > 0) then
                    fields = trim(fields) // '&password=' &
                     // curl_easy_escape(curl_ptr, trim(fileObj%password), len_trim(fileObj%password))
                end if

                call dispatch_curl(rc, 'PUT', trim(target_url), c_null_ptr, body, fields)
            elseif (len_trim(fileObj%filename) > 0 .and. .not. allocated(fileObj%buffer)) then
                ! File Upload
                call expandHomedir(trim(fileObj%filename), fullfilename)
                open(unit=10, file=fullfilename, form='unformatted', access='stream', action='read', iostat=iostatus)
                inquire(unit=10, size=filesize)
                write(stringsize, '(I32)') filesize
                stringsize = adjustl(stringsize)
                allocate(character(len=filesize) :: filebuffer)
                read(10, iostat=iostatus) filebuffer
                close(10)
                seperator = '-----' // trim(stringsize) // '-----'
                fields = trim(MIMEFile(seperator, trim(basename(fullfilename)), stringsize)) &
                        // achar(13) // achar(10) // filebuffer // achar(13) // achar(10)

                if (len_trim(fileObj%password) > 0) then
                    fields = fields // trim(MIMEPassword(seperator, fileObj%password))
                endif

                fields = fields // '--' // seperator // '--'

                headers = c_null_ptr
                headers = curl_slist_append(headers, ('Content-Type: multipart/form-data; boundary="'  &
                        // seperator // '"'))
                call dispatch_curl(rc, 'PUT', trim(target_url), headers, body, fields)
                call curl_slist_free_all(headers)
                deallocate(filebuffer)
            else
                ! Buffer Upload
                filesize = len(fileObj%buffer)
                write(stringsize, '(I32)') filesize
                stringsize = adjustl(stringsize)
                seperator = '-----' // trim(stringsize) // '-----'
                fields = trim(MIMEFile(seperator, fileObj%filename, stringsize)) &
                        // achar(13) // achar(10) // fileObj%buffer // achar(13) // achar(10)

                if (len_trim(fileObj%password) > 0) then
                    fields = fields // trim(MIMEPassword(seperator, fileObj%password))
                endif

                fields = fields // '--' // seperator // '--'

                headers = c_null_ptr
                headers = curl_slist_append(headers, ('Content-Type: multipart/form-data; boundary="'  &
                        // seperator // '"'))
                call dispatch_curl(rc, 'PUT', trim(target_url), headers, body, fields)
                call curl_slist_free_all(headers)
            end if

            call checkError(rc, body%content)
            res = deserializeResponse(body%content)
            deallocate(body%content)
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
            url = merge(trim(url) // 'true', trim(url) // 'false', formatted)

            call dispatch_curl(rc, 'GET', trim(url), c_null_ptr, body, '')
            call checkError(rc, body%content)
            res = deserializeResponse(body%content)
            deallocate(body%content)
        end function fileInfo

        function fileUpdate(token, password, previous_password, custom_expiry, hide_filename) result (res)
            type(file_response) :: res
            type(response_type), target :: body
            type(c_ptr) :: headers = c_null_ptr
            character(len=*) :: token, password, previous_password, custom_expiry
            character(len=512) :: url
            character(len=4096), target :: fields
            logical :: hide_filename
            integer :: rc

            url = ''
            url = trim(BASEURL) // '/' // trim(token)

            headers = curl_slist_append(headers, 'Content-Type: application/json; charset=utf-8')

            fields = ''
            fields = trim(fields) // '{"password":"'
            if (len_trim(password)>0) then
                fields = trim(fields) // password
            end if
            fields = trim(fields) // '","previousPassword":"'
            if (len_trim(previous_password)>0) then
                fields = trim(fields) // previous_password
            end if
            fields = trim(fields) // '","customExpiry":"'
            if (len_trim(custom_expiry)>0) then
                fields = trim(fields) // custom_expiry
            end if
            fields = trim(fields) // '","hideFilename":'
            fields = merge(trim(fields) // 'true ', trim(fields) // 'false', hide_filename)
            fields = trim(fields) // '}'

            call dispatch_curl(rc, 'PATCH', trim(url), headers, body, trim(fields))
            call checkError(rc, body%content)
            res = deserializeResponse(body%content)
            deallocate(body%content)
        end function fileUpdate

        function deleteFile(token) result (res)
            type(response_type), target :: body
            character(len=*) :: token
            character(len=512) :: url
            integer :: rc
            logical :: res

            url = ''
            url = trim(BASEURL) // '/' // trim(token)

            call dispatch_curl(rc, 'DELETE', trim(url), c_null_ptr, body, '')
            call checkError(rc, body%content)

            res = body%content(1:4) == 'true'
            deallocate(body%content)
        end function deleteFile

        subroutine getFile(fileObj, buffer, password)
            type(response_type), target :: buffer
            type(c_ptr) :: headers = c_null_ptr
            character(len=*) :: password
            type(file_response) :: fileObj, fileUrl
            integer :: rc

            if (len_trim(fileObj%url) == 0 .and. len_trim(fileObj%token) > 0) then
                fileUrl = fileInfo(fileObj%token, .true.)
                fileObj%url = fileUrl%url
            end if

            curl_ptr = curl_easy_init()
            if (len_trim(password)>0) then
                headers = curl_slist_append(headers, ('x-password: ' // password))
                call dispatch_curl(rc, 'GET', trim(fileObj%url), headers, buffer, '')
            else
                call dispatch_curl(rc, 'GET', trim(fileObj%url), c_null_ptr, buffer, '')
            end if
            call checkError(rc, buffer%content)
            if (len_trim(password)>0) then
                call curl_slist_free_all(headers)
            end if
        end subroutine getFile

        subroutine checkError(resp_code, body)
            character(len=*) :: body
            character(len=:), allocatable :: splits(:), vals(:), cleaned
            type(error_response) :: ret_error
            integer :: resp_code, p_err, i

            ret_error%name = ''
            ret_error%status = 0
            ret_error%message = ''

            if (resp_code /= CURLE_OK) stop curl_easy_strerror(resp_code)
            if (index(body, '"status":') > 0) then
                ! Deserialize error
                call split_string(trim(body), ',', splits)
                do i = 1, size(splits)
                    cleaned = ''
                    call remove_characters(trim(splits(i)),'"{}',cleaned)
                    call split_string(cleaned, ':', vals)
                    if (vals(1) == 'name') then
                        ret_error%name = trim(vals(2))
                    elseif (vals(1) == 'status') then
                        read(vals(2), *, IOSTAT=p_err) ret_error%status
                    elseif (vals(1) == 'message') then
                        ret_error%message = trim(vals(2))
                    end if
                end do
            end if
            error = ret_error
        end subroutine checkError

        function deserializeResponse(body) result (res)
            character(len=*) :: body
            character(len=:), allocatable :: splits(:), vals(:), cleaned
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

        function deserializeBucketResponse(body) result (res)
            character(len=*) :: body
            character(len=:), allocatable :: splits(:), vals(:), cleaned
            type(bucket_response) :: res
            type(file_options) :: options
            logical :: firstToken
            integer :: i,j

            call split_string(trim(body), ',', splits)
            j = 0
            firstToken = .true.
            do i = 1, size(splits)
                cleaned = ''
                call remove_characters(trim(splits(i)),'"{}[]',cleaned)
                call split_string(cleaned, ':', vals)
                if (vals(1) == 'token' .and. firstToken) then
                    res%token = trim(vals(2))
                    firstToken = .false.
                elseif (vals(1) == 'token') then
                    if(j<100) then
                        j = j + 1
                    end if
                    res%files(j)%token = trim(vals(2))
                elseif (vals(2) == 'token') then
                    if(j<100) then
                        j = j + 1
                    end if
                    res%files(j)%token = trim(vals(3))
                elseif (vals(1) == 'url') then
                    res%files(j)%url = trim(vals(2)) // ':' // trim(vals(3))
                elseif (vals(1) == 'retentionPeriod') then
                    res%files(j)%retentionPeriod = trim(vals(2))
                end if
            end do
        end function deserializeBucketResponse

        function MIMEFile(seperator, filename, stringsize) result (res)
            character(len=*) :: seperator, filename, stringsize
            character(len=1024) :: res
            res = '--' // seperator // achar(13) // achar(10) &
                    // 'Content-Disposition: form-data; name="file"; filename="' // trim(filename) &
                    // '"' // achar(13) // achar(10) &
                    // 'Content-Length: ' // trim(stringsize) // achar(13) // achar(10)  &
                    // 'Content-Type: octet-stream' // achar(13) // achar(10) // 'Content-Transfer-Encoding: binary' &
                    // achar(13) // achar(10) &
                    // achar(13) // achar(10)
        end function MIMEFile

        function MIMEPassword(seperator, password) result (res)
            character(len=*) :: seperator, password
            character(len=1024) :: res
            res = '--' // seperator // achar(13) // achar(10) &
                    // 'Content-Disposition: form-data; name="password"' // achar(13) // achar(10) &
                    // 'Content-Type: text/plain' // achar(13) // achar(10) &
                    // achar(13) // achar(10) // trim(password) // achar(13) // achar(10)
        end function MIMEPassword
end module