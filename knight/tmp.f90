MODULE knight_parser
IMPLICIT NONE
        
    TYPE parser
        INTEGER :: i
    CONTAINS
        PROCEDURE :: getnext
    END TYPE

CONTAINS

    FUNCTION getnext(this) RESULT(out)
        CLASS(parser) :: this
        INTEGER :: out

        ! PRINT*,1
        out = 4
    END FUNCTION
END MODULE

program main
use knight_parser
implicit none
    type(parser) :: p
    PRINT *, getnext(p)
end program main
