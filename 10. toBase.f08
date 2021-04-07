! File:   toBase.f08
! Author: Aakash Gajjar
! Created on August 02, 2016, 2:39 PM
!
! Subject: This program converts a given
! decimal number into specified base system
!

program toBinary
 implicit none
 integer :: number = 17
 integer :: base = 16
 print *, "Enter number"
 read *, number
 ! Specify base here
 print *, "Enter base to convert in"
 read *,base
 print *,"read from right to left"
 do while (number >= base)
  write(*, '(i0)',advance='no') mod(number, base)
  number = number/base
 enddo
 write(*, '(i0)',advance='no') number
end program toBinary

! OUTPUT
! Enter number
! 2
! Enter base to convert in
! 2
! read from right to left
! 01