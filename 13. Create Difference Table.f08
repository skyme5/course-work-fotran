! File:   Create Difference Table.f08
! Author: Aakash Gajjar
! Created on August 26, 2016, 9:23 PM
!
! Subject: This program calculates
! difference table and checks whether it has
! any error in observation sample
!

! This is just a utility function that
! initializes a matrix with supplied value
function Init(N, x, val)
 implicit none
 integer :: Init, N
 real, dimension(N, N) :: x
 real :: val
 integer :: i, j
 do i = 1, N
  do j = 1, N
   x(i, j) = val
  end do
 end do
 Init = 1
end function Init

! This function checks whether error exists
! in data
function checkError(N, x)
 integer :: checkError, N
 real, dimension(N, N) :: x
 integer :: i

 checkError = 0
 do i = 1, N
  if (floor(x(i, N - 1)) /= 0) then
   checkError = 1
   return
  endif
 end do

end function checkError

! The main function that calculates difference
! table
! The implementation is straight forward using
! matrix as data container
function makeDiffTable(N, x)
 implicit none
 integer :: makeDiffTable, N, checkError
 real, dimension(N, N) :: x
 integer :: i, j

 do j = 2, N
  do i = 1, N - j + 1
   x(i, j) = x(i + 1, j - 1) - x(i, j - 1)
  end do
 end do

 makeDiffTable = 0
 if (checkError(N, x) == 1) then
  makeDiffTable = 1
  return
 endif

end function makeDiffTable

! Main program data container
program DifferenceTable
 implicit none
 integer :: makeDiffTable, N = 6, Init
 real, dimension(6, 6) :: x
 integer :: i, j

 i = Init(N, x, 0.0)

 ! Set the X values
 x(1, 1) = 0
 x(2, 1) = 4
 x(3, 1) = 9
 x(4, 1) = 12
 x(5, 1) = 16
 x(6, 1) = 20

 if (makeDiffTable(N, x) == 1) then
  print *, "Error Exist"
 else
  print *, "Wow No Error!"
 endif

 do i = 1, N
  do j = 1, N
   write (*, '(f8.2) ', advance = 'no') x(i, j)
  end do
  print *, ''
 end do

end program DifferenceTable

! OUTPUT
! 1.
!  Wow No Error!
!     0.00    4.00    0.00    0.00    0.00    0.00
!     4.00    4.00    0.00    0.00    0.00    0.00
!     8.00    4.00    0.00    0.00    0.00    0.00
!    12.00    4.00    0.00    0.00    0.00    0.00
!    16.00    4.00    0.00    0.00    0.00    0.00
!    20.00    0.00    0.00    0.00    0.00    0.00
! 2.
!  Error Exist
!     0.00    4.00    1.00   -3.00    6.00  -10.00
!     4.00    5.00   -2.00    3.00   -4.00    0.00
!     9.00    3.00    1.00   -1.00    0.00    0.00
!    12.00    4.00    0.00    0.00    0.00    0.00
!    16.00    4.00    0.00    0.00    0.00    0.00
!    20.00    0.00    0.00    0.00    0.00    0.00