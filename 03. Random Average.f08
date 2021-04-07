! File:   Random Average.f08
! Author: Aakash Gajjar
! Created on July 19, 2016, 2:25 PM
!
! Subject: Finds average of 100 numbers
!

program randAVG
 implicit none
 real :: number, average = 0.0
 integer :: i,N
 call system("cls")
 print *,"Enter how many?"
 read *, N
 print *,"  Number            Average"
 do i = 1, N
  number = rand() * 1000
  average = average + number/i
  print *, number, average
 enddo
 call system("pause")
end program randAVG

! OUTPUT
! Enter how many?
! 3
! Number            Average
! 7.62939453E-03   7.62939453E-03
! 131.537674       65.7764664
! 755.605225       317.644867