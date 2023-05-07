subroutine printary(ary)
implicit none
   integer :: ary(:,:), i, j

   do i = lbound(ary,1), ubound(ary,1)
      write(*,*) (ary(i,j), j = lbound(ary,2), ubound(ary,2))
   end do
end subroutine


SUBROUTINE print_matrix(ary)
IMPLICIT NONE
   integer :: ary(:,:), i, j

   DO i = lbound(ary,1), ubound(ary,1)
      write(*,*) (ary(i,j), j = lbound(ary,2), ubound(ary,2))
   END DO
END SUBROUTINE
