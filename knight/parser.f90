INCLUDE 'shared.f90'

MODULE ctype
IMPLICIT NONE
    CHARACTER(1), PARAMETER :: newline = CHAR(10)
CONTAINS
    PURE FUNCTION isspace(c)
        CHARACTER, INTENT(IN) :: c
        LOGICAL :: isspace
        isspace = ((CHAR(9) .LE. c) .AND. (c .LE. CHAR(13))) .OR. (c .EQ. ' ')
    END FUNCTION

    PURE FUNCTION isdigit(c)
        CHARACTER, INTENT(IN) :: c
        LOGICAL :: isdigit
        isdigit = ('0' .LE. c) .AND. (c .LE. '9')
    END FUNCTION
END MODULE ctype

MODULE knight_parser
USE shared
IMPLICIT NONE
        
    TYPE parser
        INTEGER :: idx = 1
        CHARACTER(len=:), POINTER :: src
    CONTAINS
        PROCEDURE :: next, iseof
        PROCEDURE :: advance, peek
    END TYPE

CONTAINS
    PURE FUNCTION iseof(this)
        CLASS(parser), INTENT(IN) :: this
        LOGICAL :: iseof
        iseof = this%idx .GT. LEN(this%src)
    END FUNCTION

    FUNCTION peek(this) RESULT(chr)
        CLASS(parser), INTENT(IN) :: this
        CHARACTER :: chr
        CALL assert(.NOT. this%iseof(), "peeked at eof")
        chr = this%src(this%idx:)
    END FUNCTION

    SUBROUTINE advance(this)
        CLASS(parser) :: this
        CALL assert(.NOT. this%iseof(), "advanced at eof")
        this%idx = this%idx + 1
    END SUBROUTINE advance

    SUBROUTINE strip_whitespace_and_comments(this)
    USE ctype, ONLY: isspace, newline
        TYPE(parser) :: this
        CHARACTER :: c

        DO WHILE (.TRUE.)
            c = peek(this)

            IF (isspace(c)) THEN
                CALL this%advance()
            ELSE IF (c .EQ. '#') THEN
                DO WHILE (.NOT. this%iseof() .AND. this%peek() .NE. newline)
                    CALL this%advance()
                END DO
            ELSE
                RETURN
            END IF
        END DO
    END SUBROUTINE strip_whitespace_and_comments

    FUNCTION next(this) RESULT(out)
        CLASS(parser) :: this
        INTEGER :: out
        CHARACTER :: c

        CALL strip_whitespace_and_comments(this)
        IF (this%iseof()) THEN
            CALL die("called 'next' at EOF")
        END IF

        c = this%peek()

        ! IF (isdigit(c)) THEN
        !     OUT = this%next_int()
        !     RETURN
        ! END


        SELECT CASE (this%peek())
        CASE ("0":"9")
            PRINT*,'digit'
            ! TODO: parse digit
        CASE ("a":"z", "_")
            PRINT*,'lower'
        CASE DEFAULT
            PRINT*,'else'
            ! TODO: parse digit
        END SELECT
    END FUNCTION
END MODULE


program main
use knight_parser
implicit none
    type(parser) :: p
    character(256), target :: str = '#a123' // CHAR(10) // '456'
    integer ::i

    p%src => str

    i = p%next()
    print*,i
    i = p%next()
    print*,i
end program main
