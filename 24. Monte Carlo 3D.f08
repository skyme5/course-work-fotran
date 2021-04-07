! File:   Monte Carlo 1D.f08
! Author: Aakash Gajjar
! Created on August 07, 2016, 2:45 PM
!
! Subject: Implementing Monte Carlo 3D Integration
!           for simple function xyz from [0,1],[0,1],[0,1]
!           Ans = 1/8 = 0.125
!

function func(x, y, z)
 implicit none
 real :: func, x, y, z
 func = x * y * z
end function

function MC_1D(samples, lowx, highx, lowy, highy, lowz, highz)
 implicit none
 real :: MC_1D, func
 real :: lowx, highx, lowy, highy, lowz, highz
 real :: answer = 0.0
 integer :: samples, i

 do i = 1, samples
  answer = answer + func(lowx + (highx - lowx) * rand(), &
  lowy + (highy - lowy) * rand(), lowz + (highz - lowz) * rand())
 enddo
 MC_1D = answer/samples
end function

program main
 implicit none
 real :: answer, MC_1D, lowx = 0.0, highx = 1
 real :: lowy = 0.0, highy = 1, lowz = 0, highz = 1
 integer :: samples = 1000000
 print *, "Analytical Value  :   0.125"
 print *, "Approx Value: ", MC_1D(samples, lowx, highx, lowy, &
 highy, lowy, highy), " after ", samples, " steps"
end program

! OUTPUT
!  Analytical Value  :   0.125
!  Approx Value:   0.125147223      after      1000000  steps