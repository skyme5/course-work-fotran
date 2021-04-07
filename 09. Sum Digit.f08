! File:   Sum Digit.f08
! Author: Aakash Gajjar
! Created on July 26, 2016, 2:28 PM
!
! Subject: This programs sums individual
! digits composing that number
!

program sumDigits
 implicit none
 integer :: number, sum = 0
 print *, "Enter a number:"
 read *, number
 do while (number > 0)
  sum = sum + mod(number, 10)
  print *, number, mod(number, 10), sum
  number = number/10
 enddo
 call system("pause")
end program sumDigits