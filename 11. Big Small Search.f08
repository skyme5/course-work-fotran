! File:   Big Small Search.f08
! Author: Aakash Gajjar
! Created on July 27, 2016, 2:33 PM
!
! Subject: This program finds smallest and biggest
! numbers in a list of numbers
! and compares the result with fortran intrinsic
! functions
!

program smallBig
 implicit none
 integer :: length = 10
 integer, dimension(10) :: A
 integer :: row, smallest = 100000000, biggest = 0, temp

 ! initialize array
 do row = 1, length
  A(row) = 11 * rand()
 enddo

 ! find numbers
 do row = 1, length
  if (A(row) > biggest) then
   biggest = A(row)
  endif
  if (A(row) < smallest) then
   smallest = A(row)
  endif
 enddo

 ! print array
 do row = 1, length
  write (*, '(i3)', advance = 'no') A(row)
 enddo
 print *, ''
 write(*, fmt = *) "Smallest:", smallest, "Biggest:", biggest
 write(*, fmt = *) "Using inbuilt function Smallest:", &
 minval(A), "Biggest:", maxval(A)
 call system('pause')
end program smallBig

! OUTPUT
!  0  1  8  5  5  2  0  7  7 10
! Smallest: 0 Biggest: 10
! Using inbuilt functions Smallest:  0 Biggest:  10