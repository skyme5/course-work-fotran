! File:   Simpson 1-3rd Rule.f08
! Author: Aakash Gajjar
! Created on August 26, 2016, 6:28 PM
!
! Subject: Implementation of Simpson's
!          One third integration rule
!          to integrate a given function
!          between specified interval
!

! This is a utility function that calculates
! X^Y, x to the power y
function pow(base, power)
 implicit none
 real :: pow, base
 integer :: power, i
 pow = 1.0
 do i = 1, power
  pow = pow * base
 end do
end function pow

! The function which we want to integrate
function f(x)
 implicit none
 real :: f, x, pow
 f = 0.2 + 25 * pow(x, 1) &
 -200 * pow(x, 2) &
 +675 * pow(x, 3) &
 -900 * pow(x, 4) &
 +400 * pow(x, 5)
end function

! The Simpson's One Third Rule
! Procedure
function S13(low, high, steps)
 implicit none
 real :: S13, low, high, f
 integer :: steps
 real :: stepSize, a, b, c
 integer :: i

 ! Steps should be even
 stepSize = (high - low)/(2 * steps)
 S13 = 0.0

 ! This loop calculates rectangles area
 ! as a,b,c and uses those to estimate
 ! area S13
 do i = 1, steps
  a = low + (2 * i - 2) * stepSize
  b = low + (2 * i - 1) * stepSize
  c = low + (2 * i - 0) * stepSize
  S13 = S13 + (f(a) + f(c) + 4.0 * f(b))
 end do

 S13 = S13 * stepSize/3.0

end function S13

! The main container program
program Simpson1_3Rule
 implicit none
 real :: S13
 !Exact value is 1.640533
 print *, S13(0.0, 0.8, 200)
end program Simpson1_3Rule

! OUTPUT
! 1.64053357