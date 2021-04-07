! File:   Secant Method.f08
! Author: Aakash Gajjar
! Created on August 30, 2016, 6:48 PM
!
! Subject: Implementation of Secant Method
!          that removes difficulty of finding
!          derivative of function f instead it
!          approximates the derivative using
!          Secant's Approximation
!

! This is the function
! whose root we want to
! find!
function f(x)
 implicit none
 real :: f, x
 f = x*x-25
end function

! The Secant's Approximation
! of derivative of function f
function f1(x, lastX)
 implicit none
 real :: f1, x, lastX, f
 f1 = (f(lastX) - f(x))/(lastX - x)
 if (lastX == 0) then
  f1 = 1.0
 end if
end function f1

! The main Secant's Approximation
! procedure
! It requires an initial guess value
! plus lastX value i.e. x(n-1)
! guess = Initial Guess
! lastX = X(-1) value if starting from 0
! maxIter = Maximum iterations to perform
!           It stops after until maxIter is
!           reached or the value is below
!           a fixed tolerance of 1e-6
!
function SECANT(guess, lastX, maxIter)
 implicit none
 real :: SECANT
 real :: guess, lastX
 integer :: maxIter
 real :: f, f1
 integer :: i
 real :: last, temp

 ! It stores last value of x in
 ! variable last
 SECANT = guess
 last = lastX
 do i = 1, maxIter
  ! make decision based on tolerance
  ! whether to exit or continue
  if (abs(SECANT - last) > 1e-6) then
   temp = SECANT
   SECANT = SECANT - f(SECANT)/f1(SECANT, last)
   last = temp
  end if
 end do
end function SECANT

program SecantMethod
 implicit none
 real :: SECANT
 ! The root of this function is
 ! 0.56714329
 print *, SECANT(1.0, 0.0, 100)
end program SecantMethod

! OUTPUT
! 5.00000000