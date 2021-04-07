! File:   Newton General Difference Formula.f08
! Author: Aakash Gajjar
! Created on August 27, 2016, 1:32 PM
!
! Subject: Implementation of Newton's General
!          Difference formula

! The function that calculates factors in
! Newton's polynomial
function factor(size, point, diff, x)
 implicit none
 real :: factor, point
 integer :: diff, size
 real, dimension(size) :: x
 integer :: i

 factor = 1.0
 if (diff > 1) then
  do i = 1, diff - 1
   factor = factor * (point - x(i))
  end do
 end if
end function factor

! The function that calculates Nth Difference required
! in Newton's Polynomial
recursive function nDifference(size, diff, index, x, f) result(answer)
 implicit none
 real :: factor
 real :: answer
 integer :: size, diff, index
 real, dimension(size) :: x, f
 integer :: i

 answer = 0.0
 if (diff > 2) then
  answer = (nDifference(size, diff - 1, index, x, f) - &
  nDifference(size, diff - 1, index - 1, x, f))/&
  (x(index) - x(index - diff + 1))
 else
  answer = (f(index) - f(index - 1))/(x(index) - x(index - 1))
 end if
end function nDifference

! The Function for Interpolation
! @args: x : array of x values
!        f : array of f values
!      size: length of array
!     point: interpolation point
function NGD(size, x, f, point)
 implicit none
 real :: NGD
 real :: factor, nDifference
 integer :: size
 real, dimension(size) :: x, f
 real :: point
 integer :: diff

 NGD = f(1)

 ! Again difference should start from 0
 do diff = 2, size
  NGD = NGD + factor(size, point, diff, x) * &
  nDifference(size, diff, diff, x, f)
 end do
end function NGD

! The main program
program NewtonInterpolation
 implicit none
 integer :: size = 5
 real, dimension(5) :: x, f
 real :: NGD
 real :: point = 1.86
 f = (/0.0, 1.0986, 1.6094, 1.9459, 2.1972/)
 x = (/1.0, 3.0, 5.0, 7.0, 9.0/)

 print *, NGD(size, x, f, point)
end program NewtonInterpolation

! OUTPUT
!  Function data
!  f = 0.0, 1.0986, 1.6094, 1.9459, 2.1972
!  Variable X data
!  x = 1.0, 3.0, 5.0, 7.0, 9.0
!  x = 1.83
!  f = 0.584319830