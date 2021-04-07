! File:   Runge-Kutta Order 2.f08
! Author: Aakash Gajjar
! Created on September 2, 2016, 10:44 PM
!
! Subject: Implementation of Runge-Kutta Second
!          Order Method to solve ODE's of the
!          order 1.

! This is the function
! of kind y' = f(x,y)
function f(x, y)
 implicit none
 real :: f
 real :: x, y
 f = 3.0 * x + y/2.0
end function f

! RK2 Subroutine that takes following arguments
! steps: Number of steps
! low:   Low Bound of X value
! high:  Upper Bound of Y value
! x0:    Initial value of x
! y0:    Initial value of y
! y:     Output Value
subroutine RKTwo(steps, low, high, x0, y0, y)
 implicit none
 real :: f
 integer, intent(in) :: steps
 real, intent(in) :: low, high
 real, intent(in) :: x0, y0
 real(kind = 8), intent(out) :: y
 real :: lastX, lastY
 real :: stepSize
 real :: k1, k2
 integer :: i

 ! Calculates stepSize according to supplied
 ! number of steps
 stepSize = (high - low)/steps
 lastX = x0
 lastY = y0
 do i = 1, steps
  ! Increase the value since we already have
  ! initial value
  lastX = lastX + stepSize
  ! Coefficients
  k1 = f(lastX, lastY)
  k2 = f(lastX + (2.0/3.0) * stepSize, lastY &
  +(2.0/3.0) * stepSize * k1)
  ! Update lastY with new value
  lastY = lastY + stepSize * (k1/4.0 + 3.0 * k2/4)
  ! Just for debugging
  !print *, lastX, lastY
 end do
 ! Return value
 y = lastY
end subroutine RKTwo

! Got to have main
program RK2
 implicit none
 real(kind = 8) :: y = 0
 ! Why don't make a call
 call RKTwo(100, 0.0, 0.2, 0.0, 1.0, y)
 ! Don't you wanna know answer
 print *, y
 print *, "Exact value is: 1.16722193"
end program RK2

! OUTPUT
!    1.1684831380844116
!  Exact value is: 1.16722193