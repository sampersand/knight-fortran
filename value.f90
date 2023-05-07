MODULE shared
CONTAINS
    SUBROUTINE die(msg)
        CHARACTER(len=*), INTENT(IN) :: msg
        PRINT*, msg
        STOP 1
    END SUBROUTINE

    SUBROUTINE bug(msg)
        CHARACTER(len=*), INTENT(IN) :: msg
        CALL die("bug: " // msg)
    END SUBROUTINE

    SUBROUTINE assert(cond)
        LOGICAL :: cond
        IF (.NOT. cond) THEN
            CALL bug("assertion failed")
        END IF
    END SUBROUTINE
END MODULE

MODULE knight_string
    USE shared
IMPLICIT NONE
    TYPE string
        INTEGER :: refcount = 1
        CHARACTER(:), ALLOCATABLE :: str
    CONTAINS
        PROCEDURE :: dump, run
        PROCEDURE :: to_int, to_bool, to_str, to_ary
        PROCEDURE :: set_int, set_str, set_ary
        PROCEDURE :: clone, free
    END TYPE

MODULE knight_value
    USE shared
IMPLICIT NONE
    INTEGER, PARAMETER, PRIVATE :: &
        kundef = 0, &
        kint   = 1, &
        ktrue  = 2, &
        kfalse = 3, &
        knull  = 4, &
        kstr   = 5, &
        kary   = 6, &
        kblock = 7, &
        kident = 8, &
        kfunc  = 9

    TYPE value
        INTEGER :: kind, int = 0
        CHARACTER(:), ALLOCATABLE :: str
        TYPE(value), DIMENSION(:), ALLOCATABLE :: ary
    CONTAINS
        PROCEDURE :: dump, run
        PROCEDURE :: to_int, to_bool, to_str, to_ary
        PROCEDURE :: set_int, set_str, set_ary
        PROCEDURE :: clone, free
    END TYPE

    TYPE(value), PARAMETER, PUBLIC :: &
        vtrue  = value(kind = ktrue), &
        vfalse = value(kind = kfalse), &
        vnull  = value(kind = knull), &
        vundef = value(kind = kundef)

CONTAINS
    SUBROUTINE clone(this)
        CLASS(value), INTENT(INOUT) :: this
        IF (this%kind .GE. kstr) THEN
            this%int = this%int + 1
        END IF
    END SUBROUTINE

    RECURSIVE SUBROUTINE free(this)
        CLASS(value), INTENT(INOUT) :: this
        INTEGER :: i

        IF (this%kind .LT. kstr) THEN
            RETURN
        END IF

        CALL assert(this%int .NE. 0)
        this%int = this%int - 1
        IF (this%int .NE. 0) THEN
            RETURN
        END IF

        IF (this%kind .EQ. kstr .OR. this%kind .EQ. kident) THEN
            DEALLOCATE(this%str)
        END IF

        IF (this%kind .GT. kstr) THEN
            DO i=1, SIZE(this%ary)
                CALL this%ary(i)%free()
            END DO

            DEALLOCATE(this%ary)
        END IF
    END SUBROUTINE

    SUBROUTINE set_int(this, int)
        CLASS(value), INTENT(INOUT) :: this
        INTEGER :: int
        this%kind = kint
        this%int = int
    END SUBROUTINE

    SUBROUTINE set_str(this, str)
        CLASS(value), INTENT(INOUT) :: this
        CHARACTER(:), ALLOCATABLE :: str
        this%kind = kstr
        this%str = str
        this%int = 1
    END SUBROUTINE

    SUBROUTINE set_ary(this, ary)
        CLASS(value) :: this
        TYPE(value), DIMENSION(:), ALLOCATABLE :: ary
        this%kind = kary
        this%ary = ary
        this%int = 1
    END SUBROUTINE

    SUBROUTINE dump(this, fd)
        CLASS(value) :: this
        INTEGER :: fd, i

        SELECT CASE (this%kind)
        CASE (kundef)
            WRITE(fd, "(A)", ADVANCE="no") "<undef>"

        CASE (kint)
            WRITE(fd, "(I0)", ADVANCE="no") this%int

        CASE (kstr)
            WRITE(fd, "(A)", ADVANCE="no") '"'
            WRITE(fd, "(A)", ADVANCE="no") this%str
            WRITE(fd, "(A)", ADVANCE="no") '"'

        CASE (ktrue)
            WRITE(fd, "(A)", ADVANCE="no") "true"

        CASE (kfalse)
            WRITE(fd, "(A)", ADVANCE="no") "false"

        CASE (knull)
            WRITE(fd, "(A)", ADVANCE="no") "null"

        CASE (kary)
            WRITE(fd, "(A)", ADVANCE="no") "["
            do i=1, SIZE(this%ary)
                IF (i /= 1) then
                    WRITE(fd, "(A)", ADVANCE="no") ", "
                END IF
                CALL this%ary(i)%dump(fd)
            end do
            WRITE(fd, "(A)", ADVANCE="no") ']'

        CASE (kblock,kfunc,kident)
            CALL die("bad type: block")

        CASE DEFAULT
            CALL bug("bad type: ")
        END SELECT
    END SUBROUTINE

    RECURSIVE FUNCTION run(this) RESULT(val)
        CLASS(value), INTENT(INOUT) :: this
        TYPE(value) :: val

        SELECT CASE (this%kind)
            CASE (kint,ktrue,kfalse,knull)
                val = this

            CASE (kstr,kary)
                CALL this%clone()
                val = this

            CASE (kident)
                IF (SIZE(this%ary) .EQ. 0) THEN
                    CALL die("unknown variable " // this%str // " accessed")
                END IF
                val = this%ary(1)
                CALL val%clone()

            CASE (kblock,kfunc)
                CALL DIE("todo")

            CASE DEFAULT
                CALL bug("invalid type")
            END SELECT
    END FUNCTION

    RECURSIVE FUNCTION to_int(this) RESULT(int)
        CLASS(value), INTENT(IN) :: this
        INTEGER :: int

        SELECT CASE (this%kind)
        CASE (kint)
            int = this%int

        CASE (kstr)
            READ(this%str, *) int

        CASE (ktrue)
            int = 1

        CASE (kfalse, knull)
            int = 0

        CASE (kary)
            int = SIZE(this%ary)

        CASE DEFAULT
            CALL bug("invalid type")

        END SELECT
    END FUNCTION to_int

    RECURSIVE FUNCTION to_str(this) RESULT(val)
        CLASS(value), INTENT(IN) :: this
        TYPE(value) :: val

        val%kind = kstr
        SELECT CASE (this%kind)
        CASE (kint)
            ! int = this%int

        CASE (kstr)
            CALL this%clone()
            val = this

        CASE (ktrue)
            ALLOCATE(val%str)
            val%str = "true"

        CASE (kfalse, knull)
            int = 0

        CASE (kary)
            int = SIZE(this%ary)

        CASE DEFAULT
            CALL bug("invalid type")

        end select


        CHARACTER(:), ALLOCATABLE :: str
    str = "Hello"
        CALL die("todo")
    END FUNCTION

    SUBROUTINE reverse(ary)
        TYPE(value), DIMENSION(:), ALLOCATABLE :: ary
        TYPE(value) :: tmp
        INTEGER :: i

        DO i = 1, SIZE(ary) / 2
            tmp = ary(i)
            ary(i) = ary(SIZE(ary) - i + 1)
            ary(SIZE(ary) - i + 1) = tmp
        END DO
    END SUBROUTINE

    RECURSIVE FUNCTION to_ary(this) RESULT(val)
        CLASS(value), INTENT(IN) :: this
        TYPE(value), DIMENSION(:), ALLOCATABLE :: tmpary
        CHARACTER(:), ALLOCATABLE :: str
        TYPE(value) :: val
        INTEGER :: i, j

        val%kind = kary
        SELECT CASE (this%kind)
        CASE (kint)
            i = this%int
            ALLOCATE(val%ary(1))

            j = 1
            CALL val%ary(j)%set_int(MOD(i, 10))

            DO
                j = j + 1
                i = i / 10

                IF (i .EQ. 0) THEN
                    EXIT
                END IF

                IF (ALLOCATED(tmpary)) THEN 
                    DEALLOCATE(tmpary)
                END IF

                ALLOCATE(tmpary(SIZE(val%ary) + 1))
                tmpary(:) = val%ary(:)
                DEALLOCATE(val%ary)
                val%ary = tmpary

                CALL val%ary(j)%set_int(MOD(i, 10))
            END DO

            CALL reverse(val%ary)

        CASE (kstr)
            ALLOCATE(val%ary(LEN(this%str)))

            DO i = 1, LEN(this%str)
                IF (ALLOCATED(str)) THEN
                    DEALLOCATE(str)
                END IF
                ALLOCATE(CHARACTER(1) :: str)
                str = this%str(i : i)
                CALL val%ary(i)%set_str(str)
            END DO

        CASE (ktrue)
            ALLOCATE(val%ary(1))
            val%ary(1) = vtrue

        CASE (kfalse,knull)
            ALLOCATE(val%ary(0))
        CASE DEFAULT
            CALL bug("invalid type")

        END SELECT
    END FUNCTION

    RECURSIVE FUNCTION to_bool(this) RESULT(bool)
        CLASS(value), INTENT(IN) :: this
        LOGICAL :: bool

        SELECT CASE (this%kind)
        CASE (ktrue)
            bool = .TRUE.

        CASE (kfalse,knull)
            bool = .FALSE.

        CASE (kint)
            bool = this%int .NE. 0

        CASE (kstr)
            bool = LEN(this%str) .NE. 0

        CASE (kary)
            bool = SIZE(this%ary) .NE. 0

        CASE DEFAULT
            CALL bug("invalid type")

        END SELECT
    END FUNCTION
END MODULE knight_value


program main
use knight_value
implicit none
    TYPE(value) :: val, other
    ! CALL val%set_int(-0)
    ! other = val%to_ary()
    ! CALL other%dump(0)
    character(:), ALLOCATABLE :: str
    str = "Hello"

    CALL val%set_str(str)
    val = val%to_ary()
    CALL val%dump(0)
    PRINT *, VAL%to_bool()
end program
