! File:   Monte Carlo 2D.f08
! Author: Aakash Gajjar
! Created on August 07, 2016, 2:48 PM
!
! Subject: Implementing Monte Carlo 1D Integration
!           for simple function y = x from [0,1]
!

function func(point)
 implicit none
 real :: func, point
 func = point
end function

function MC_1D(samples, low, high)
 implicit none
 real :: low, high, func, answer = 0, MC_1D
 integer :: samples, i

 do i = 1, samples
  answer = answer + func(low + (high - low) * rand())
 enddo
 MC_1D = answer/samples;
end function

program main
 implicit none
 real :: answer, MC_1D
 integer :: samples
 print *, "In 1000 iteration", MC_1D(1000, 0.0, 1.0)
 print *, "Analytic answer is 0.5"
end program

! OUTPUT
!  In 1000 iteration  0.497961760
!  Analytic answer is 0.5