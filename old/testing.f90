module mymodule
implicit none

type car
    character(len=25) :: maker
    integer :: wheels
end type

contains
    include 'printary.f90'

subroutine double(num)
    implicit none
    integer, pointer :: num
    num = num * 2
end subroutine double

end module

program main
    use mymodule
    implicit none
    type(car) :: c
    integer, pointer :: ptr
    integer, target :: num = 3

    1 format
    ptr=>num
    call double(ptr)
    print*, num
    c%maker = 'honda'
    c%wheels = 4
    print*, c
    ! call mymodule::printary(foo)
end program
