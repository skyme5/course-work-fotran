! File:   Range Kutta Order 4.f08
! Author: Aakash Gajjar
! Created on September 17, 2016, 10:34 PM
!
! Subject: Implementation of Runge-Kutta Fourth
!          Order method for solving ODE's of
!          order 1

! This is the function
! of kind y' = f(x,y)
function f(x, y)
 implicit none
 real :: f
 real :: x, y
 f = 3.0 * x + y/2.0
end function f

! RK4 Subroutine that takes following arguments
! steps: Number of steps
! low:   Low Bound of X value
! high:  Upper Bound of Y value
! x0:    Initial value of x
! y0:    Initial value of y
! y:     Output Value
subroutine RKFour(steps, low, high, x0, y0, y)
 implicit none
 real :: f
 integer, intent(in) :: steps
 real, intent(in) :: low, high
 real, intent(in) :: x0, y0
 real, intent(out) :: y
 real :: lastX, lastY
 real :: stepSize
 real :: k1, k2, k3, k4
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
  k1 = stepSize * f(lastX, lastY)
  k2 = stepSize * f(lastX + stepSize/2.0, lastY + k1/2.0)
  k3 = stepSize * f(lastX + stepSize/2.0, lastY + k2/2.0)
  k4 = stepSize * f(lastX + stepSize, lastY + k3)
  ! Update lastY with new value
  lastY = lastY + (k1 + 2 * k2 + 2 * k3 + k4)/6.0
  ! Just for debugging
  !print *, stepSize, lastX, lastY
 end do
 ! Return value
 y = lastY
end subroutine RKFour

! Got to have main
program RK4
 implicit none
 real :: y = 0.0
 ! Why don't make a call
 call RKFour(10, 0.0, 0.2, 0.0, 1.0, y)
 ! Don't you wanna know answer
 print *, y
 print *, "Exact value is: 1.16722193"
end program RK4

! OUTPUT
!   1.17984271
! Exact value is: 1.16722193