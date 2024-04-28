! Waifuvault utils
module waifuvault_utils
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: stringToLogical, basename, expandHomedir, getHomeDirectory, split_string, remove_characters

    contains

    function stringToLogical(input) result (res)
        character(len=*) :: input
        logical :: res
        res = input == 'true'
    end function stringToLogical

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
end module waifuvault_utils