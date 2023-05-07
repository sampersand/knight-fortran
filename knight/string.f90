INCLUDE 'shared.f90'
! INCLUDE 'value.f90'

MODULE knight_string
USE shared
IMPLICIT NONE

    TYPE string
        INTEGER :: refcount = 1
        CHARACTER(:), ALLOCATABLE :: str
    CONTAINS
        ! PROCEDURE :: free, run
        PROCEDURE :: to_int, to_bool, to_ary
        ! PROCEDURE :: set_int, set_str, set_ary
        ! PROCEDURE :: clone, free
    END TYPE string

CONTAINS

    FUNCTION to_int(this) RESULT(int)
        CLASS(string), INTENT(IN) :: this
        INTEGER :: int
        READ(this%str, *) int
    END 

    FUNCTION to_bool(this) RESULT(logic)
        CLASS(string), INTENT(IN) :: this
        LOGICAL :: logic
        logic = LEN(this%str) .NE. 0
    END 

    SUBROUTINE reverse(ary)
        TYPE(string), DIMENSION(:), ALLOCATABLE :: ary
        TYPE(string) :: tmp
        INTEGER :: i

        DO i = 1, SIZE(ary) / 2
            tmp = ary(i)
            ary(i) = ary(SIZE(ary) - i + 1)
            ary(SIZE(ary) - i + 1) = tmp
        END DO
    END SUBROUTINE

    FUNCTION to_ary(this) RESULT(int)
        CLASS(string), INTENT(IN) :: this
        INTEGER :: int
        ! READ(this%str, *) int
    END 

END MODULE

