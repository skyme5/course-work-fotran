! File:   CoPrime.f08
! Author: Aakash Gajjar
! Created on August 02, 2016, 2:38 PM
!
! Subject: This program checks whether two
! numbers are co-prime or not
! Two numbers are co-prime if their GCD is 1
!

program coPrime
 implicit none
 integer :: number1 = 12, number2 = 11, i, temp, isCoPrime = 1
 print *, "Enter Two Numbers"
 read *, number1, number2
 ! Swap Numbers if Number1 is less, Number2 is Bigger
 if (number1 > number2) then
  temp = number1
  number1 = number2
  number2 = temp
 endif
 ! Start the real thing
 i = 2
 do while (i < number1)
  if (mod(number1, i) == 0) then
   if (mod(number2, i) == 0) then
    isCoPrime = 0
   endif
  endif
  i = i + 1
 enddo
 if (isCoPrime == 1) then
  print *, "They are co-prime"
 else
  print *, "They are No a co-prime"
 endif
end program coPrime

! OUTPUT
! Enter Two Numbers
! 23 12
! They are co-prime
! Enter Two Numbers
! 4 12
! They are No a co-prime