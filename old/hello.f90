subroutine printary(ary)
implicit none
   integer :: ary(:,:), i, j

   do i = lbound(ary,1), ubound(ary,1)
      write(*,*) (ary(i,j), j = lbound(ary,2), ubound(ary,2))
   end do
end subroutine

program dynamic_array 
implicit none 
   interface
      subroutine printary(ary)
         integer :: ary(:,:)
      end subroutine
   end interface

   integer, dimension(5,5) :: foo
   integer :: i,j
   do i=1, 5
      do j=1, 5
         foo(i,j) = i*j
      end do
   end do

   call printary(foo)
   print*, "/"
   where (foo > 7) 
      foo = foo ** 2
   else where (foo > 9)
      foo = 100
   end where
   call printary(foo)

   ! integer :: bar(5,5)

   ! print*, rank(foo), size(foo)
   ! print*, rank(bar), size(bar)

   ! !rank is 2, but size not known   
   ! real, dimension (:,:), allocatable :: darray    
   ! integer :: s1, s2     
   ! integer :: i, j     
   
   ! print*, "Enter the size of the array:"     
   ! read*, s1, s2      
   
   ! ! allocate memory      
   ! allocate ( darray(s1,s2) )      
   
   ! do i = 1, s1           
   !    do j = 1, s2                
   !       darray(i,j) = i*j               
   !       print*, "darray(",i,",",j,") = ", darray(i,j)           
   !    end do      
   ! end do      
   
   ! deallocate (darray)  
end program dynamic_array
! subroutine fillit(ary)
!    integer, dimension (:), intent (out) :: ary
!    do i = 1, size(ary)
!       ary(i) = i ** 2
!    end do  
! end subroutine
! subroutine printit(ary)
!    integer, dimension (:) :: ary  
!    do i = 1, size(ary)
!       print *, ary(i)
!    end do
! end subroutine

! program main      
! implicit none      
!    interface
!       subroutine fillit(ary)
!          integer, dimension (:), intent (out) :: ary
!       end subroutine

!       subroutine printit(ary)
!          integer, dimension (:) :: ary
!       end subroutine
!    end interface

!    integer, dimension (7) :: ary
!    call fillit(ary)      
!    call printit(ary)
!    print, 0.0E+00
! end program

! ! !> show the usage of type-bound procedures (pass/nopass arguments)
! ! module test_m
! !    implicit none
! !    private
! !    public test_type
! !    type test_type
! !       integer :: i
! !    contains
! !       procedure, nopass :: print_hello
! !       procedure         :: print_int
! !    end type
! ! contains
! !    !> do not process type specific data => nopass
! !    subroutine print_hello
! !       print *, "hello"
! !    end subroutine

! !    !> process type specific data => first argument is "this" of type "class(test_type)"
! !    !! use class and not type below !!!!
! !    subroutine print_int(this)
! !       class(test_type), intent(in) :: this

! !       print *, "i", this%i
! !   end subroutine
! ! end module

! ! program main
! !    use test_m
! !    implicit none

! !    integer :: i, n, nfact
! !    integer, dimension(5) :: numbers
! !    nfact = 1
! !    print *, "hello" // "world"
! !    print *, kind(.true.)
! !    do i = 1,5
! !       numbers(i) = i * 2.0
! !    end do
! !    numbers = (/ 1, 2, 3, 4, 5 /)
! !    print*, numbers
! ! ! compute factorials
! ! do n = 1, 10
! !    nfact = nfact * n  
! !    ! printing the value of n and its factorial
! !    print*,  n, " ", nfact   
! ! end do
! !    ! type (test_type) :: obj

! !    ! obj%i = 1
! !    ! select case (3)
! !    ! case (0)
! !    ! case (1)
! !    !    print *, "one"
! !    ! case (2)
! !    ! case default
! !    !    print *, "two"
! !    ! end select

! !    ! if (obj%i .eq. 1) then
! !    !    print *, "i is 1"
! !    ! else
! !    !    print *, "i is not 1"
! !    ! endif
! !    ! call obj%print_hello
! !    ! call obj%print_int
! !    ! print *, obj%i == 5
! ! end program
