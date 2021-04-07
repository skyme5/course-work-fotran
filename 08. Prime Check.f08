! File:   Prime Check.f08
! Author: Aakash Gajjar
! Created on August 02, 2016, 2:37 PM
!
! Subject: Checks if two numbers are
! prime or not! using modulo
!

program checkPrime
 implicit none
 integer :: number, i, maxNum
 do while (.true.)
  print *, "Enter a number"
  read *, number
  maxNum = number/2
  i = 2
  do while (i <= maxNum)
   if (mod(number, i) == 0) then
    print *, number, "is Not a prime"
    exit
   endif
   i = i + 1
  enddo
  if (i > maxNum) then
   print *, number, "is a Prime number"
  endif
 enddo
end program checkPrime

! OUTPUT
! Enter a number
! 23
! 23 is a Prime number