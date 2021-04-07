! File:   Newton Ralphson Root.f08
! Author: Aakash Gajjar
! Created on August 26, 2016, 9:01 PM
!
! Subject: Implementation of Newton Ralphson
!          root finding procedure
! This method explicitly requires to define function
! as well as it's first derivative
!

! The FUNCTION
function f(x)
 implicit none
 real :: f, x
 f = x * x - 25.0
end function

! The FUNCTION'S DERIVATIVE
function fD(x)
 implicit none
 real :: fD, x
 fD = 2.0 * x
end function fD

function NR(guess, tolerance)
 implicit none
 real :: NR, guess, tolerance
 real :: f, fD

 NR = guess
 ! Don't forget as we get close to a
 ! root the function value converges
 ! to zero(0).
 do while (abs(f(NR)) > tolerance)
  NR = NR - (f(NR)/fD(NR))
 end do

end function NR

! Note: Compare this result from BiSection Method
! You should see that this method gives root
! 5.0000 to be exact where BiSection does not!
!
program NewtonRalphson
 implicit none
 real :: NR
 print *, NR(1.0, 1.0e-7)
end program NewtonRalphson

! OOUPUT
! 5.00000000