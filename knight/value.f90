! INCLUDE 'stringh.f90'

MODULE knight_value
! USE shared
IMPLICIT NONE
    TYPE value
        integer :: x
    contains
    procedure :: free, to_str
    end type
contains
    subroutine free(this)
        class(value) :: this
    end subroutine

    subroutine to_str(this)
        class(value) :: this
    end subroutine
end module
