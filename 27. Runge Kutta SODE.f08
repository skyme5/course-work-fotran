! File:   Runge Kutta SODE.f08
! Author: Aakash Gajjar
! Created on September 18, 2016, 12:20 PM
!
! Subject: Implementation of Runge-Kutta Fourth
!          Order method for solving simultaneous
!          ODE's of  order 1
!

function f(x, y1, y2)
 implicit none
 real :: f
 real :: x, y1, y2
 f = -y1/2
end function f

function g(x, y1, y2)
 implicit none
 real :: g
 real :: x, y1, y2
 g = 4 - 0.3 * y2 - 0.1 * y1
end function g

subroutine RKFour(steps, low, high, x0, y1, y2)
 implicit none
 real :: f, g
 integer, intent(in) :: steps
 real, intent(in) :: low, high
 real, intent(in) :: x0
 real, intent(in out) :: y1, y2
 real :: lastX, lastY
 real :: stepSize
 real :: f1, f2, f3, f0
 real :: g1, g2, g3, g0
 integer :: i

 ! Calculates stepSize according to supplied
 ! number of steps
 stepSize = (high - low)/steps
 lastX = x0
 !write(*, '(f9.6Af9.6Af9.6)') lastX, 9, y1, 9, y2
 !write(*, '(f9.6Af9.6Af9.6)') lastX, ' ', y1, ' ', y2
 do i = 1, steps
  lastX = lastX + stepSize
  f0 = stepSize * f(lastX, y1, y2)
  g0 = stepSize * g(lastX, y1, y2)

  f1 = stepSize * f(lastX + stepSize/2, y1 + f0/2, y2 + g0/2)
  g1 = stepSize * g(lastX + stepSize/2.0, y1 + f0/2.0, y2 + g0/2)

  f2 = stepSize * f(lastX + stepSize/2, y1 + f1/2, y2 + g1/2)
  g2 = stepSize * g(lastX + stepSize/2, y1 + f1/2, y2 + g1/2)

  f3 = stepSize * f(lastX + stepSize, y1 + f2, y2 + g2)
  g3 = stepSize * g(lastX + stepSize, y1 + f2, y2 + g2)

  y1 = y1 + (f0 + 2 * f1 + 2 * f2 + f3)/6
  y2 = y2 + (g0 + 2 * g1 + 2 * g2 + g3)/6
  !write(*, '(f9.6Af9.6Af9.6)') lastX, ' ', y1, ' ', y2
 end do
end subroutine RKFour

! Got to have main
program RK4
 implicit none
 real :: x = 0.0, y1 = 4.0, y2 = 6.0
 call RKFour(4, x, 2.0, x, y1, y2)
 ! Don't you wanna know answer
 print *, y1, y2
end program RK4

! OUTPUT
!    1.47157681       8.94686508