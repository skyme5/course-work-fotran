! File:   Trapezoidal Integration.f08
! Author: Aakash Gajjar
! Created on August 26, 2016, 6:15 PM
!
! Subject: Implementation of Trapezoidal
!          integration procedure
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

! The Trapezoidal Function
function trapezoidal(low, high, steps)
 implicit none
 real :: trapezoidal, low, high, f
 integer :: steps
 real :: stepSize
 real :: answer
 integer :: i

 stepSize = (high - low)/steps

 ! YOU should check definition for
 ! this method
 answer = f(low) + f(high)

 ! i is from 1 to less than steps
 do i = 1, steps - 1
  answer = answer + 2 * f(low + i * stepSize)
 end do

 trapezoidal = answer * stepSize/2
end function trapezoidal

program TrapezoidalLaw
 implicit none
 real :: trapezoidal
 !Exact value is 1.640533
 print *, trapezoidal(0.0, 0.8, 200)
end program TrapezoidalLaw

! OUTPUT
! 1.64046979