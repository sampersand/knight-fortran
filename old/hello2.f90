module fileutils
contains
    subroutine readfile(filename, result, fd)
        implicit none
        character(:), pointer :: filename
        character, allocatable, intent(out) :: result(:)
        character(256) :: errmsg, buf
        integer :: fd, err, length

        open(fd, file=filename, status="old", iomsg=errmsg, err=300)

        allocate(result(256), stat=err)
        length = 0
        if (err /= 0) then ; go to 300 ; end if

        do while (.true.)
            read(fd, "(1A256)", iomsg=errmsg, err=200, end=100) buf(:)
            ! result = buf
            print *,"'",buf,"'"
        end do
    100 return
    200 close(fd)
    300 write(*,*) trim(errmsg)
    end subroutine
end module

program foo
    use fileutils
implicit none

    character(len=50), target :: filename_ = "old.f90"
    character(:), pointer :: filename => filename_
    character, allocatable :: output(:)

    call readfile(filename, output, 10)
    print*, output
    ! call readfile("old.f90", output)
    ! print*, output
    ! deallocate(output)

    ! integer :: i
    ! character(len=100) :: input
    ! open(9, file="Makefile1", err=19, status="old")
    ! GOTO 20
    ! 19 print*, "err!"
    ! 20 i = 9
    !    ! read(9, )
end program
! program productDetails 
! implicit none 
!    100 format(7x,'Name:', 7x, 'Id:', 1x, 'Weight:')
!    200 format(1x, a, 2x, i3, 2x, f5.2) 

!    character (len = 15) :: name
!    integer :: id 
!    real :: weight

!    name = 'Ardupilot'
!    id = 1
!    weight = 0.08
    
!    print *,' The product details are' 
    
!    print 100
!    print 200, name, id, weight 
    
! end program productDetails
