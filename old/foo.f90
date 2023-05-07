MODULE hello
IMPLICIT NONE
  TYPE thing
    INTEGER :: refcount = 1
  CONTAINS
    PROCEDURE :: world
  END TYPE
CONTAINS
    SUBROUTINE world(this)
        CLASS(thing), INTENT(INOUT) :: this
        PRINT*, "1:", this%refcount
    END SUBROUTINE
END MODULE

MODULE lol
IMPLICIT NONE
  TYPE thing2
    INTEGER :: refcount = 1
  CONTAINS
    PROCEDURE :: world
  END TYPE
CONTAINS
    SUBROUTINE world(this)
        CLASS(thing2), INTENT(INOUT) :: this
        PRINT*, "2:", this%refcount
    END SUBROUTINE
END MODULE

PROGRAM hello1
    use hello!, only: thing
    use lol!, only: thing2

    TYPE(THING2) :: T
    TYPE(THING) :: T2
    CALL T%WORLD()
    CALL T2%WORLD()
end program
