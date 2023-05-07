INCLUDE 'shared.f90'
INCLUDE 'value.f90'

MODULE knight_array
USE shared
USE knight_value, ONLY: value
IMPLICIT NONE

    TYPE array
        INTEGER :: rc = 1
        TYPE(value), DIMENSION(:), ALLOCATABLE :: ary
    CONTAINS
        PROCEDURE :: free
        PROCEDURE :: to_int, to_bool!, to_str
        ! PROCEDURE :: set_int, set_str, set_ary
        ! PROCEDURE :: clone, free
    END TYPE array

CONTAINS
    SUBROUTINE free(this)
        CLASS(array) :: this
        INTEGER :: i

        CALL assert(this%rc .NE. 0, "zero refcount given to free")

        this%rc = this%rc - 1
        IF (this%rc .NE. 0) THEN
            RETURN
        END IF

        DO i = 1, SIZE(this%ary)
            CALL this%ary(i)%free()
        END DO

        DEALLOCATE(this%ary)
    END SUBROUTINE free

    PURE FUNCTION to_int(this) RESULT(int)
        CLASS(array), INTENT(in) :: this
        INTEGER :: int

        int = SIZE(this%ary)
    END FUNCTION to_int

    PURE FUNCTION to_bool(this) RESULT(bool)
        CLASS(array), INTENT(in) :: this
        LOGICAL :: bool

        bool = SIZE(this%ary) .NE. 0
    END FUNCTION to_bool

    ! PURE FUNCTION to_str(this) RESULT(bool)
    !     CLASS(array), INTENT(in) :: this
    !     LOGICAL :: bool

    !     bool = SIZE(this%ary) .NE. 0
    ! END FUNCTION to_bool
END MODULE

