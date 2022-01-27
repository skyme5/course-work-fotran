! File:   Schrodinger Equation.f08
! Author: Aakash Gajjar
! Created on September 30, 2016, 3:54 AM
!
! Subject: Numerical Solution of Harmonic Oscillator
!           using shooting method
!

! IMPORTANT NOTE HERE
! YOU NEED TO SET VALUE OF OMEGA IN FUNCTION G BELOW
function w()
 implicit none
 real(kind = 8) :: w
 w = 1.0
end function w

! Function f for RKFour
function f(x, y1, y2)
 implicit none
 real(kind = 8) :: f
 real(kind = 8) :: x, y1, y2
 f = y2
end function f

! Function g for RKFour
function g(x, y1, y2, E)
 implicit none
 real(kind = 8) :: g
 real(kind = 8) :: x, y1, y2, E, V, w
 real(kind = 8) :: m = 1.0, h = 1.0
 !real(kind=8) :: w = 1.4
 V = (m * w() * w() * x * x)/2.0
 g = y1 * (V - E)*(2.0 * m)/(h * h)
end function g

! Utility subroutine foe printing values
subroutine PrintVal(a, b)
 implicit none
 real(kind = 8) :: a, b
 print *, a, b
end subroutine PrintVal

! RKFour function to approximate value of yn
! @args  steps     : Number of steps to take
!        low       : low bound to x value
!        high      : high bound to x value, this is the value
!                    where we want to find yn
!        x0        : initial x value
!        y1_o,y2_o : initial y1, y2 values
!        E         : Gauss value of Energy
!        prnt  : Do we want to print y1, y2 values
function RKFour(steps, low, high, x0, y1_o, y2_o, E, prnt)
 implicit none
 real(kind = 8) :: RKFour, f, g
 integer, intent(in) :: steps
 real(kind = 8), intent(in) :: low, high
 real(kind = 8), intent(in) :: x0
 real(kind = 8), intent(in) :: y1_o, y2_o
 real(kind = 8), intent(in) :: E
 integer :: prnt
 real(kind = 8) :: lastX, y1, y2
 real(kind = 8) :: stepSize
 real(kind = 8) :: f1, f2, f3, f0
 real(kind = 8) :: g1, g2, g3, g0
 integer :: i

 ! Determine step size
 stepSize = (high - low)/steps
 ! Assign values to variable
 lastX = x0
 y1 = y1_o
 y2 = y2_o
 ! If we want to print values
 if (prnt == 1) then
  call PrintVal(lastX, y1)
  ! Uncomment following line to print Probability density
  !call PrintVal(lastX,y2*y2) !Probability density
 end if

 ! Loop that does the most of the work
 do i = 1, steps
  lastX = lastX + stepSize

  f0 = f(lastX, y1, y2)
  g0 = g(lastX, y1, y2, E)

  f1 = f(lastX + stepSize/2.0, y1 + f0 * stepSize/2.0, &
  y2 + g0 * stepSize/2.0)
  g1 = g(lastX + stepSize/2.0, y1 + f0 * stepSize/2.0, &
  y2 + g0 * stepSize/2.0, E)

  f2 = f(lastX + stepSize/2.0, y1 + f1 * stepSize/2.0, &
  y2 + g1 * stepSize/2.0)
  g2 = g(lastX + stepSize/2.0, y1 + f1 * stepSize/2.0, &
  y2 + g1 * stepSize/2.0, E)

  f3 = f(lastX + stepSize, y1 + f2 * stepSize, y2 + &
  g2 * stepSize)
  g3 = g(lastX + stepSize, y1 + f2 * stepSize, y2 + &
  g2 * stepSize, E)

  y1 = y1 + stepSize * (f0 + 2.0 * f1 + 2.0 * f2 + f3)/6.0
  y2 = y2 + stepSize * (g0 + 2.0 * g1 + 2.0 * g2 + g3)/6.0

  if (prnt == 1) then
   call PrintVal(lastX, y1)
   ! Uncomment following line to print Probability density
   !call PrintVal(lastX,y2*y2) !Probability density
  end if
 end do
 RKFour = y1
end function RKFour

! This subroutine does the shooting work
subroutine SolveSHO(steps, x0, y1, y2, energy_low, &
 energy_high, energy)
 real(kind = 8), intent(in) :: x0
 integer, intent(in) :: steps
 real(kind = 8), intent(in) :: y1, y2
 real(kind = 8), intent(inout) :: energy_low, energy_high
 real(kind = 8), intent(inout) :: energy
 real(kind = 8) :: RKFour, energy_mid
 real(kind = 8) :: low, high, middle
 real(kind = 8) :: tolerance = 1e-10
 integer :: printing = 0, i
 ! Find y_n at 3 values low,middle and high
 ! and determines in which interval energy falls
 do i = 0, 2000
  energy_mid = (energy_high + energy_low)/2.0
  low = RKFour(1000, -x0, x0, -x0, y1, y2, &
  energy_low, printing)
  high = RKFour(1000, -x0, x0, -x0, y1, y2, &
  energy_high, printing)
  middle = RKFour(1000, -x0, x0, -x0, y1, y2, &
  energy_mid, printing)
  ! Just for break
  if (abs(energy_high - energy_low) < tolerance) then
   goto 23
  endif
  if (middle * high > 0) then
   energy_high = (energy_low + energy_high)/2.0
  end if
  if (middle * low > 0) then
   energy_low = (energy_low + energy_high)/2.0
  end if
 end do
 23 energy = (energy_low + energy_high)/2.0
end subroutine SolveSHO

program Schrodinger
 implicit none
 real(kind = 8) :: x0 = 3.0
 integer :: steps = 10000
 real(kind = 8) :: y1 = 0.0, y2 = -1.0
 real(kind = 8) :: energy_low, energy_high
 real(kind = 8) :: energy = 0
 real(kind = 8) :: RKFour, w
 real(kind = 8) :: temporary
 integer :: printing, i
 do i = 0, 20
  energy_low = i + 0.15
  energy_high = i + 1.2
  !printing = 1
  call SolveSHO(steps, x0, y1, y2, energy_low, &
  energy_high, energy)
  !temporary = RKFour(steps*1000, -x0, x0, -x0, y1, y2, energy, printing)
  print "(a,i4,a,f8.4,a,f8.4,a,ES10.3)", "#State:",i," Energy:", energy, " Th: ", w()*(i + 0.5),&
  " %Error: ", 100 * (energy - w()*(i + 0.5))/energy
 end do
end program Schrodinger

! OUTPUT
! #State:   0 Energy:  0.5004 Th:   0.5000 %Error:  7.820E-02
! #State:   1 Energy:  1.5061 Th:   1.5000 %Error:  4.039E-01
! #State:   2 Energy:  2.5411 Th:   2.5000 %Error:  1.619E+00
! #State:   3 Energy:  3.6642 Th:   3.5000 %Error:  4.482E+00
! #State:   4 Energy:  4.9542 Th:   4.5000 %Error:  9.168E+00
! #State:   5 Energy:  5.5000 Th:   5.5000 %Error:  1.852E-10
! #State:   6 Energy:  6.4734 Th:   6.5000 %Error: -4.115E-01
! #State:   7 Energy:  7.5000 Th:   7.5000 %Error:  1.358E-10
! #State:   8 Energy:  8.2529 Th:   8.5000 %Error: -2.994E+00
! #State:   9 Energy:  9.5000 Th:   9.5000 %Error: -3.346E-06
! #State:  10 Energy: 10.3038 Th:  10.5000 %Error: -1.904E+00
! #State:  11 Energy: 11.5000 Th:  11.5000 %Error: -2.764E-06
! #State:  12 Energy: 12.6291 Th:  12.5000 %Error:  1.022E+00
! #State:  13 Energy: 13.5000 Th:  13.5000 %Error: -2.355E-06
! #State:  14 Energy: 14.5000 Th:  14.5000 %Error: -2.192E-06
! #State:  15 Energy: 15.2294 Th:  15.5000 %Error: -1.777E+00
! #State:  16 Energy: 16.5000 Th:  16.5000 %Error:  6.175E-11
! #State:  17 Energy: 18.1047 Th:  17.5000 %Error:  3.340E+00
! #State:  18 Energy: 18.5000 Th:  18.5000 %Error:  5.508E-11
! #State:  19 Energy: 19.5000 Th:  19.5000 %Error:  5.225E-11
! #State:  20 Energy: 20.5000 Th:  20.5000 %Error:  4.970E-11