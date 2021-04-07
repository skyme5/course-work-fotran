! File:   Gauss Jordan Elimination.f08
! Author: Aakash Gajjar
! Created on August 31, 2016, 8:21 PM
!
! Subject: Implementation of Gauss Jordan
!          Elimination Method for Solving
!          Systems of Equation

! In this program a matrix stores value of
! RHS of equations in last column

! Main subroutine is GaussJordan which takes
! number of rows, => rows
! number of cols, => cols
! Matrix a containing x coefficients => a(rows,cols)
! as it's arguments

! This subroutine Normalizes a given row in matrix by
! it's first non-zero element
subroutine Normalize(rows, cols, a, row)
 implicit none
 integer :: rows, cols
 real :: a(rows, cols)
 integer :: row
 integer :: i
 real :: pivot
 pivot = a(row, row)
 do i = 1, cols
  a(row, i) = a(row, i)/pivot
 end do
end subroutine Normalize

! This subroutine does the elimination part of
! Gauss Jordan method
subroutine Eliminate(rows, cols, a, row)
 implicit none
 integer :: rows, cols
 real :: a(rows, cols)
 integer :: row
 integer :: i, j
 integer :: times
 real :: pivot

 do i = 1, rows
  if (i /= row) then
   pivot = a(i, row)
   do j = 1, cols
    a(i, j) = a(i, j) - pivot * a(row, j)
   end do
  end if
 end do
end subroutine Eliminate

! Main subroutine call for Gauss Jordan Method
subroutine GaussJordan(rows, cols, a)
 implicit none
 integer :: rows, cols
 real :: a(rows, cols)
 integer :: row, col

 ! It starts from first row upto last
 ! and calls required subroutine to
 ! do work
 do row = 1, rows
  call normalize(rows, cols, a, row)
  call eliminate(rows, cols, a, row)
 end do
end subroutine GaussJordan

! This is just a small utility subroutine
! for displaying line before and after a
! matrix is printed
subroutine DrawLine(cols)
 implicit none
 integer :: cols, j
 print *, ''
 do j = 1, cols
  write(*, "(A)", advance = "no") "========"
 end do
 print *, ''
end subroutine DrawLine

! This subroutine prints a matrix
subroutine PrintA(rows, cols, a)
 implicit none
 integer :: rows, cols
 real :: a(rows, cols)
 integer :: i, j
 call DrawLine(cols)
 do i = 1, rows
  do j = 1, cols
   write(*, "(f8.3)", advance = "no") a(i, j)
  end do
  print *, ''
 end do
 call DrawLine(cols)
end subroutine PrintA

! This subroutine prints a matrix
subroutine ReadA(rows, cols, a)
 implicit none
 integer :: rows, cols
 real :: a(rows, cols)
 integer :: i, j
 write(*, '(Ai1A1i1)') "Enter a matrix of ", &
 rows, "x", cols
 do i = 1, rows
  do j = 1, cols
   read *, a(i, j)
  end do
 end do
end subroutine ReadA

! This subroutine reads dimensions of matrix
subroutine ReadDimension(rows, cols)
 implicit none
 integer :: rows, cols
 write(*, "(A)", advance = "no") "Enter number of rows: "
 read *, rows
 cols = rows + 1
end subroutine ReadDimension

! This subroutine prints a Info
subroutine PrintInfo
 implicit none
 print *, "Gauss Jordan Elimination - SkyME5"
 print *, "This program stores RHS of equations "
 print *, "in last column of matrix so enter "
 print *, "dimensions accordingly"
end subroutine PrintInfo

! The main program which defines matrix and it's
! component and calls requires procedures
program GaussJordanElimination
 implicit none
 integer :: rows = 3, cols = 4
 real, allocatable, dimension(:,:) :: a
 call PrintInfo
 call ReadDimension(rows, cols)
 allocate(a(rows, cols))
 call ReadA(rows, cols, a)
 call PrintA(rows, cols, a)
 call GaussJordan(rows, cols, a)
 print *, "Answer: "
 call PrintA(rows, cols, a)
 call system("pause")
end program GaussJordanElimination

! OUTPUT
!  Gauss Jordan Elimination - SkyME5
!  This program stores RHS of equations
!  in last column of matrix so enter
!  dimensions accordingly
! Enter number of rows: 3
! Enter a matrix of 3x4
! 2 4 3 13 1 3 2 9 3 6 1 16
! ================================
!    2.000   4.000   3.000  13.000
!    1.000   3.000   2.000   9.000
!    3.000   6.000   1.000  16.000
! ================================
!  Answer:
! ================================
!    1.000   0.000   0.000   1.000
!    0.000   1.000   0.000   2.000
!   -0.000  -0.000   1.000   1.000