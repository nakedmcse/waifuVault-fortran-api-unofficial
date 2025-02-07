! Waifuvault utils
module waifuvault_utils
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: stringToLogical, basename, expandHomedir, extension, getHomeDirectory, split_string, remove_characters, getMime

    contains

    function stringToLogical(input) result (res)
        character(len=*) :: input
        logical :: res
        res = input == 'true'
    end function stringToLogical

    function stringToInt(input) result (res)
        character(len=*) :: input
        integer :: res, ios
        read(input, '(I10)', IOSTAT=ios) res
        if (ios /= 0) then
            res = -1
        end if
    end function stringToInt

    function basename(path)
        character(len=*), intent(in) :: path
        character(len=len(path)) :: basename
        integer :: i

        do i = len(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                basename = path(i+1:)
                return
            endif
        end do

        basename = path
    end function basename

    function extension(pathname) result(ext)
        character(len=*), intent(in) :: pathname
        character(len=:), allocatable :: ext
        integer :: i

        i = len_trim(pathname)
        do while (i > 0)
            if (pathname(i:i) == '.') then
                ext = pathname(i:)
                return
            end if
            i = i - 1
        end do

        ext = ''
    end function extension

    subroutine expandHomedir(path, result)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: result
        character(len=1000) :: home
        integer :: status

        call getHomeDirectory(home, status)

        result = merge(trim(home) // path(2:), path, (path(1:1) == '~' .and. status == 0))
    end subroutine expandHomedir

    subroutine getHomeDirectory(home, status)
        character(len=*), intent(out) :: home
        integer, intent(out) :: status

        call get_environment_variable("HOME", home, status=status)

        if (status /= 0) then
            home = ''
        end if
    end subroutine getHomeDirectory

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

    function getMime(ext) result (mime)
        character(len=*) :: ext
        character(len=:), allocatable :: mime
        ! List from https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types

        select case (trim(ext))
            case ('.aac')
                mime = 'audio/aac'
            case ('.abw')
                mime = 'application/x-abiword'
            case ('.arc')
                mime = 'application/x-freearc'
            case ('.avif')
                mime = 'image/avif'
            case ('.avi')
                mime = 'video/x-msvideo'
            case ('.azw')
                mime = 'application/vnd.amazon.ebook'
            case ('.bin')
                mime = 'application/octet-stream'
            case ('.bmp')
                mime = 'image/bmp'
            case ('.bz')
                mime = 'application/x-bzip'
            case ('.bz2')
                mime = 'application/x-bzip2'
            case ('.cda')
                mime = 'application/x-cdf'
            case ('.csh')
                mime = 'application/x-csh'
            case ('.css')
                mime = 'text/css'
            case ('.csv')
                mime = 'text/csv'
            case ('.doc')
                mime = 'application/msword'
            case ('.docx')
                mime = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
            case ('.eot')
                mime = 'application/vnd.ms-fontobject'
            case ('.epub')
                mime = 'application/epub+zip'
            case ('.exe')
                mime = 'application/x-dosexec'
            case ('.gz')
                mime = 'application/gzip'
            case ('.gif')
                mime = 'image/gif'
            case ('.htm', '.html')
                mime = 'text/html'
            case ('.ico')
                mime = 'image/vnd.microsoft.icon'
            case ('.ics')
                mime = 'text/calendar'
            case ('.jar')
                mime = 'application/java-archive'
            case ('.jpeg', '.jpg')
                mime = 'image/jpeg'
            case ('.js')
                mime = 'text/javascript'
            case ('.json')
                mime = 'application/json'
            case ('.jsonld')
                mime = 'application/ld+json'
            case ('.mid', '.midi')
                mime = 'audio/midi'
            case ('.mjs')
                mime = 'text/javascript'
            case ('.mp3')
                mime = 'audio/mpeg'
            case ('.mp4')
                mime = 'video/mp4'
            case ('.mpeg')
                mime = 'video/mpeg'
            case ('.mpkg')
                mime = 'application/vnd.apple.installer+xml'
            case ('.odp')
                mime = 'application/vnd.oasis.opendocument.presentation'
            case ('.ods')
                mime = 'application/vnd.oasis.opendocument.spreadsheet'
            case ('.odt')
                mime = 'application/vnd.oasis.opendocument.text'
            case ('.oga')
                mime = 'audio/ogg'
            case ('.ogv')
                mime = 'video/ogg'
            case ('.ogx')
                mime = 'application/ogg'
            case ('.opus')
                mime = 'audio/opus'
            case ('.otf')
                mime = 'font/otf'
            case ('.png')
                mime = 'image/png'
            case ('.pdf')
                mime = 'application/pdf'
            case ('.php')
                mime = 'application/x-httpd-php'
            case ('.ppt')
                mime = 'application/vnd.ms-powerpoint'
            case ('.pptx')
                mime = 'application/vnd.openxmlformats-officedocument.presentationml.presentation'
            case ('.rar')
                mime = 'application/vnd.rar'
            case ('.rtf')
                mime = 'application/rtf'
            case ('.sh')
                mime = 'application/x-sh'
            case ('.svg')
                mime = 'image/svg+xml'
            case ('.tar')
                mime = 'application/x-tar'
            case ('.tif', '.tiff')
                mime = 'image/tiff'
            case ('.ts')
                mime = 'video/mp2t'
            case ('.ttf')
                mime = 'font/ttf'
            case ('.txt')
                mime = 'text/plain'
            case ('.vsd')
                mime = 'application/vnd.visio'
            case ('.wav')
                mime = 'audio/wav'
            case ('.weba')
                mime = 'audio/webm'
            case ('.webm')
                mime = 'video/webm'
            case ('.webp')
                mime = 'image/webp'
            case ('.woff')
                mime = 'font/woff'
            case ('.woff2')
                mime = 'font/woff2'
            case ('.xhtml')
                mime = 'application/xhtml+xml'
            case ('.xls')
                mime = 'application/vnd.ms-excel'
            case ('.xlsx')
                mime = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
            case ('.xml')
                mime = 'application/xml'
            case ('.xul')
                mime = 'application/vnd.mozilla.xul+xml'
            case ('.zip')
                mime = 'application/zip'
            case ('.3gp')
                mime = 'video/3gpp'
            case ('.3g2')
                mime = 'video/3gpp2'
            case ('.7z')
                mime = 'application/x-7z-compressed'
            case default
                mime = 'application/octet-stream'
        end select
    end function getMime

end module waifuvault_utils