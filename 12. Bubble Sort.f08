! File:   Bubble Sort.f08
! Author: Aakash Gajjar
! Created on July 27, 2016, 2:35 PM
!
! Subject: This program sorts given list of
! numbers using a very simple sorting algorithm
! known as "Bubble Sort"
! In bubble sort we swap two consecutive numbers
! until they are sorted in a ascending or
! descending order.
!

program bubbleSort
 implicit none
 integer :: length = 10
 integer, dimension(10) :: A
 integer :: row, col, temp

 ! initialize matrix
 do row = 1, length
  A(row) = 10 * rand()
 enddo

 ! print sorted array
 print *, "Unsorted: "
 do row = 1, length
  write(*, '(i4)', advance = 'no') A(row)
 enddo
 print *, ""

 ! bubble sorting algorithms
 do row = 1, length
  do col = row, length
   if (A(col) < A(row)) then
    temp = A(row)
    A(row) = A(col)
    A(col) = temp
   endif
  enddo
 enddo

 ! print sorted array
 print *, "Sorted: "
 do row = 1, length
  write(*, '(i4)', advance = 'no') A(row)
 enddo
 print *, ''
 call system('pause')
end program bubbleSort

! OUTPUT
! Unsorted:
!   0   1   7   4   5   2   0   6   6   9
! Sorted:
!   0   0   1   2   4   5   6   6   7   9