! Waifuvault Fortran Models
module waifuvault_models
    implicit none
    ! file_upload
    type file_upload
        character(len=512) :: filename
        character(len=512) :: url
        character, allocatable :: buffer(:)
        integer :: buffer_size
        character(len=10) :: expires
        character(len=512) :: password
        logical :: hideFilename
        logical :: oneTimeDownload
        contains
        procedure :: create_upload
        procedure :: build_url
    end type file_upload

    ! file_options
    type file_options
        logical :: hideFilename
        logical :: oneTimeDownload
        logical :: protected
        contains
        procedure :: create_options
    end type file_options

    ! file_response
    type file_response
        character(len=80) :: token
        character(len=512) :: url
        character(len=80) :: retentionPeriod
        type(file_options) :: options
        contains
        procedure :: create_response
    end type file_response

    ! ErrorResponse
    type error_response
        character(len=80) :: name
        integer :: status
        character(len=4096) :: message
        contains
        procedure :: create_error_response
    end type error_response

    ! MemoryStream
    type memory_stream
        character, allocatable :: buffer(:)
        integer :: buffer_size
    end type memory_stream

    contains

        subroutine create_upload(this, target, expires, password, hide_filename, one_time_download)
            class(file_upload) :: this
            character(len=*) :: target
            character(len=*) :: expires
            character(len=*) :: password
            logical :: hide_filename, one_time_download

            this%url = ''
            this%filename = ''
            this%expires = ''
            this%password = ''

            if(target(1:7) == 'http://' .or. target(1:8) == 'https://') then
                this%url = target
            else
                this%filename = target
            end if

            this%expires = expires
            this%password = password
            this%hideFilename = hide_filename
            this%oneTimeDownload = one_time_download
        end subroutine create_upload

        function build_url(this) result (res)
            class(file_upload) :: this
            integer :: len
            character(len=512) :: res
            res = 'https://waifuvault.walker.moe/rest?'
            if(len_trim(this%password)>0) then
                res = trim(res) // 'password=' // trim(this%password) // '&'
            end if
            if(len_trim(this%expires)>0) then
                res = trim(res) // 'expires=' // trim(this%expires) // '&'
            end if
            if(this%hideFilename) then
                res = trim(res) // 'hide_filename=true&'
            end if
            if(this%oneTimeDownload) then
                res = trim(res) // 'one_time_download=true&'
            end if
            len = len_trim(res)
            if(res(len:len) == '&') then
                res(len:len) = ' '
                res = trim(res)
            end if
        end function build_url

        subroutine create_options(this, hideFilename, oneTimeDownload, protected)
            class(file_options) :: this
            logical :: hideFilename, oneTimeDownload, protected
            this%hideFilename = hideFilename
            this%oneTimeDownload = oneTimeDownload
            this%protected = protected
        end subroutine create_options

        subroutine create_response(this, token, url, retention, options)
            class(file_response) :: this
            character(len=*) :: token, retention
            character(len=*) :: url
            type(file_options) :: options

            this%token = ''
            this%url = ''
            this%retentionPeriod = ''

            this%token = token
            this%url = url
            this%retentionPeriod = retention
            this%options = options
        end subroutine create_response

        subroutine create_error_response(this, name, status, message)
            class(error_response) :: this
            character(len=*) :: name
            integer :: status
            character(len=*) :: message

            this%name = ''
            this%message = ''

            this%name = name
            this%status = status
            this%message = message
        end subroutine create_error_response
end module waifuvault_models
