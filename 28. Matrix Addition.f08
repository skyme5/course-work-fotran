! File:   Matrix Addition.f08
! Author: Aakash Gajjar
! Created on July 27, 2016, 2:30 PM
!
! Subject: This program adds two matrix of
! same (rows,cols).
!

program addMatrix
 implicit none
 integer, dimension(5, 5) :: A, B
 integer :: row, col, dim = 5

 ! initialize MAtrix
 do col = 1, dim
  do row = 1, dim
   A(row, col) = rand() * 1000
   B(row, col) = rand() * 1000 * 2
  enddo
 enddo

 ! add MAtrix
 do col = 1, dim
  do row = 1, dim
   B(row, col) = B(row, col) + A(row, col)
  enddo
 enddo

 ! print MAtrix
 print *, "After adding two MAtrix"
 do row = 1, dim
  do col = 1, dim
   write (*, '(i5)', advance = 'no') B(row, col)
  enddo
 enddo
end program addMatrix