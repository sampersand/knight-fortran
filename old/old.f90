
!> show the usage of type-bound procedures (pass/nopass arguments)
module test_m
   implicit none
   private
   public test_type
   type test_type
      integer :: i
   contains
      procedure, nopass :: print_hello
      procedure         :: print_int
   end type
contains
   !> do not process type specific data => nopass
   subroutine print_hello
      print *, "hello"
   end subroutine

   !> process type specific data => first argument is "this" of type "class(test_type)"
   !! use class and not type below !!!!
   subroutine print_int(this)
      class(test_type), intent(in) :: this

      print *, "i", this%i
  end subroutine
end module

program main
   use test_m
   implicit none
   type (test_type) :: obj

   obj%i = 1
   select case (3)
   case (0)
   case (1)
      print *, "one"
   case (2)
   case default
      print *, "two"
   end select

   if (obj%i .eq. 1) then
      print *, "i is 1"
   else
      print *, "i is not 1"
   endif
   call obj%print_hello
   call obj%print_int
   print *, obj%i == 5
end program
