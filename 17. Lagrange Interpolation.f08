! File:   Lagrange Interpolation.f08
! Author: Aakash Gajjar
! Created on August 26, 2016, 11:12 PM
!
! Subject: Implementation of Lagrange
! Interpolation Formula
!
! Note: The Implementation should be self explaining

! This function finds factor for L's Polynomial
function factor(size, point, x, order)
 implicit none
 real :: factor, point
 integer :: size, order
 real, dimension(size) :: x
 integer :: i

 ! If YOU're still confuse check the formula first
 factor = 1.0
 do i = 1, size
  if (i /= order) then
   factor = factor * (point - x(i))/(x(order) - x(i))
  end if
 end do

end function factor

! The main function for Interpolation
function lagrange(size, point, x, f)
 implicit none
 integer :: size
 real :: lagrange, point, factor
 real, dimension(size) :: x, f
 integer :: i

 ! The lagrange polynomial
 ! Initialized with 0
 lagrange = 0.0
 do i = 1, size
  ! We want to sum with the factor of degree i
  ! into function i.e. lagrange
  lagrange = lagrange + factor(size, point, x, i) * f(i)
 end do

end function lagrange

! Main program container
program LagrangeInterpolation
 implicit none
 real :: lagrange, point = 1.83
 integer :: size = 5, i
 real, dimension(5) :: x, f
 ! Function data
 f = (/0.0, 1.0986, 1.6094, 1.9459, 2.1972/)
 ! The variable X data
 x = (/1.0, 3.0, 5.0, 7.0, 9.0/)
 ! Prints Interpolated Value
 print *, lagrange(size, point, x, f)
end program LagrangeInterpolation

! OUTPUT
!  Function data
!  f = 0.0, 1.0986, 1.6094, 1.9459, 2.1972
!  Variable X data
!  x = 1.0, 3.0, 5.0, 7.0, 9.0
!  x = 1.83
!  f = 0.567234635