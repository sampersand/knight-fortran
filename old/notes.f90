
# annotate something as a parameter for it to be a constant
   ! gravitational acceleration
   real, parameter :: g = 9.81   

you use .eqv.  and .neqv. for boolean comparisons


you can use 'case (low:high)' for case testing

do var = start, stop [,step]    
   ! statement(s)
   …
end do

do while (logical expr) 
   statements
end do
The concatenation operator //, concatenates characters.
   numbers = (/ 1, 2, 3, 4, 5 /)

   character(len = 11)::hello
   hello = "Hello World"
   print*, hello(7:11)
   

program dataStatement
implicit none

   integer :: a(5), b(3,3), c(10),i, j
   data a /7,8,9,10,11/ 
   
   data b(1,:) /1,1,1/ 
   data b(2,:)/2,2,2/ 
   data b(3,:)/3,3,3/ 
   data (c(i),i = 1,10,2) /4,5,6,7,8/ 
   data (c(i),i = 2,10,2)/5*2/
   
