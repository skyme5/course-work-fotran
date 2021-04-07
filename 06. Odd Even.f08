! File:   Odd Even.f08
! Author: Aakash Gajjar
! Created on July 13, 2016, 2:19 PM
!
! Check whether a number is odd or even
! It determines by taking a modulo 2
! and checking a remainder returned
!

program evenODD
 implicit none
 integer :: inputNumber
 print *, "Enter Ctrl-C to exit"
 do while (.true.)
  print *, "Enter a number:"
  read *, inputNumber
  if (mod(inputNumber, 2) == 0) then
   print *, inputNumber, " is was EVEN!"
  else
   print *, inputNumber, " is was ODD!"
  endif
 enddo
end program evenODD

! OUTPUT
! Enter Ctrl-C to exit
! Enter a number:
! 3
! 3  is ODD!
! Enter a number:
! 6
! 6  is EVEN!