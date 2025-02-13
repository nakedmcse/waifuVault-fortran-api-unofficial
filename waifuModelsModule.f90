! Waifuvault Fortran Models
module waifuvault_models
    implicit none
    ! file_upload
    type, public :: file_upload
        character(len=512) :: filename
        character(len=512) :: url
        character(len=80) :: bucketToken
        character(len=:), allocatable :: buffer
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
    type, public :: file_options
        logical :: hideFilename
        logical :: oneTimeDownload
        logical :: protected
        contains
        procedure :: create_options
    end type file_options

    ! album_info
    type, public :: album_info
        character(len=80) :: token
        character(len=80) :: publicToken
        character(len=512) :: name
        character(len=80) :: bucket
        integer :: dateCreated
    end type album_info

    ! file_response
    type, public :: file_response
        character(len=80) :: token
        character(len=80) :: bucket
        character(len=512) :: url
        character(len=80) :: retentionPeriod
        integer :: id
        integer :: views
        type(file_options) :: options
        type(album_info) :: album
        contains
        procedure :: create_response
    end type file_response

    ! album_response
    type, public :: album_response
        character(len=80) :: token
        character(len=80) :: publicToken
        character(len=512) :: name
        character(len=80) :: bucket
        integer :: dateCreated
        type(file_response), dimension(256) :: files
    end type album_response

    ! bucket_response
    type, public :: bucket_response
        character(len=80) :: token
        type(file_response), dimension(256) :: files
        type(album_response), dimension(256) :: albums
    end type bucket_response

    ! Restriction
    type, public :: restriction
        character(len=80) :: type
        character(len=512) :: value
    end type restriction

    ! RestrictionResponse
    type, public :: restriction_response
        type(restriction), dimension(100) :: restrictions
    end type restriction_response

    ! ErrorResponse
    type, public :: error_response
        character(len=80) :: name
        integer :: status
        character(len=4096) :: message
        contains
        procedure :: create_error_response
    end type error_response

    ! general_response
    type, public :: general_response
        logical :: success
        character(len=4096) :: description
    end type general_response

    contains

        subroutine create_upload(this, target, bucket, expires, password, hide_filename, one_time_download)
            class(file_upload) :: this
            character(len=*) :: target, bucket, expires, password
            logical :: hide_filename, one_time_download

            this%url = ''
            this%bucketToken = ''
            this%filename = ''
            this%expires = ''
            this%password = ''

            if(target(1:7) == 'http://' .or. target(1:8) == 'https://') then
                this%url = target
            else
                this%filename = target
            end if

            this%bucketToken = bucket
            this%expires = expires
            this%password = password
            this%hideFilename = hide_filename
            this%oneTimeDownload = one_time_download
        end subroutine create_upload

        function build_url(this) result (res)
            class(file_upload) :: this
            integer :: len
            character(len=512) :: res
            res = ''
            if(len_trim(this%bucketToken)>0) then
                res = trim(res) // '/' // trim(this%bucketToken)
            end if
            res = trim(res) // '?'
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

        subroutine create_response(this, token, bucket, url, retention, options)
            class(file_response) :: this
            character(len=*) :: token, bucket, retention, url
            type(file_options) :: options

            this%token = ''
            this%bucket = ''
            this%bucket = ''
            this%url = ''
            this%retentionPeriod = ''

            this%token = token
            this%bucket = bucket
            this%url = url
            this%retentionPeriod = retention
            this%options = options
        end subroutine create_response

        subroutine create_error_response(this, name, status, message)
            class(error_response) :: this
            character(len=*) :: name, message
            integer :: status

            this%name = ''
            this%message = ''

            this%name = name
            this%status = status
            this%message = message
        end subroutine create_error_response
end module waifuvault_models
