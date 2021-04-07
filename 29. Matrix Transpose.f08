! File:   Matrix Transpose.f08
! Author: Aakash Gajjar
! Created on July 27, 2016, 2:31 PM
!
! Subject: This program finds transpose
! of a given matrix and prints it
!

program transposeMatrix
 implicit none
 integer :: dim = 5, temp
 integer, dimension(5, 5) :: A
 integer :: row, col

 ! initialize MAtrix
 do col = 1, dim
  do row = 1, dim
   A(row, col) = rand() * 1000
  enddo
 enddo

 ! Do Transpose
 do col = 1, dim
  do row = 1, dim
   temp = A(row, col)
   A(row, col) = A(col, row)
   A(col, row) = temp
  enddo
 enddo

 ! print matrix
 do col = 1, dim
  do row = 1, dim
   write(*, '(i5)') A(row, col)
  enddo
 enddo
end program transposeMatrix