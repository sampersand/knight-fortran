MODULE shared
IMPLICIT NONE
CONTAINS
    ! Exits with the given message.
    SUBROUTINE die(msg)
        CHARACTER(LEN=*), INTENT(IN) :: msg

        PRINT*, msg
        STOP 1
    END SUBROUTINE die

    ! Same as 'die', but indicates an internal problem.
    SUBROUTINE bug(msg)
        CHARACTER(LEN=*), INTENT(IN) :: msg

        CALL die("bug: " // msg)
    END SUBROUTINE bug

    ! IF 'cond' is false, exit with 'msg'.
    SUBROUTINE assert(cond, msg)
        LOGICAL, INTENT(IN) :: cond
        CHARACTER(LEN=*), INTENT(IN) :: msg

        IF (.NOT. cond) THEN
            CALL bug("assertion failed")
        END IF
    END SUBROUTINE assert
END MODULE shared
