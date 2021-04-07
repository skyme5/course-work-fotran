! File:   Newton Forward Difference Interpolation.f08
! Author: Aakash Gajjar
! Created on August 27, 2016, 12:06 PM
!
! Subject: Implementation of Newton's Forward
!          Difference formula
! This program uses pascal triangle coefficients to
! calculate Nth difference

! The Function that calculates factorial
function factorial(n)
 implicit none
 integer :: factorial, n
 integer :: i
 factorial = 1
 do i = 1, n
  factorial = i * factorial
 end do
end function factorial

! The function that calculates factors in
! Newton's polynomial
function factor(diff, stepSize)
 implicit none
 real :: factor
 integer :: diff
 real :: stepSize
 integer :: i

 factor = 1.0
 do i = 0, diff - 1
  factor = factor * (stepSize - i)/(i + 1)
 end do

end function factor

! The function that calculates Nth Difference required
! in Newton's Polynomial
function nDifference(size, diff, f)
 implicit none
 real :: nDifference, factor
 integer :: factorial
 integer :: size, diff
 real, dimension(size) :: f
 integer :: i, sign

 nDifference = 0.0
 do i = 0, diff
  if (mod(i, 2) == 0) then
   sign = 1
  else
   sign = -1
  end if
  ! Since fortran arrays start from 1
  ! i have added 1 to function value
  nDifference = nDifference + sign * f(diff - i + 1)&
  *(factorial(diff))/(factorial(i) * factorial(diff - i))
 end do
end function nDifference

! The Function for Interpolation
! @args: x : array of x values
!        f : array of f values
!      size: length of array
!     point: interpolation point
function NFD(size, x, f, point)
 implicit none
 real :: NFD
 real :: factor, nDifference
 integer :: size, step
 real, dimension(size) :: x, f
 real :: point, stepSize
 integer :: diff

 step = x(2) - x(1)
 stepSize = (point - x(1))/step

 NFD = f(1)

 ! Again difference should start from 0
 do diff = 2, size
  NFD = NFD + factor(diff - 1, stepSize) * nDifference(size, diff - 1, f)
 end do
end function NFD

! The main program
program NewtonInterpolation
 implicit none
 integer :: size = 5
 real, dimension(5) :: x, f
 real :: NFD
 real :: point = 1.83

 f = (/0.0, 1.0986, 1.6094, 1.9459, 2.1972/)
 x = (/1.0, 3.0, 5.0, 7.0, 9.0/)

 print *, NFD(size, x, f, point)
end program NewtonInterpolation

! OUTPUT
!  Function data
!  f = 0.0, 1.0986, 1.6094, 1.9459, 2.1972
!  Variable X data
!  x = 1.0, 3.0, 5.0, 7.0, 9.0
!  x = 1.83
!  f = 0.567234695