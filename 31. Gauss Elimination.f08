! File:   Gauss Elimination.f08
! Author: Aakash Gajjar
! Created on August 29, 2016, 10:22 PM
!
! Subject: Implementation of Naive Gauss Elimination
!          Method
!

! The main subroutine that does the
! Gauss Elimination
subroutine GaussElimination(a, n, m)
 implicit none
 integer, intent(in) :: n, m
 real, intent(in out) :: a(n, m)
 real :: first, second
 integer :: row, col
 integer :: i, j
 do i = 2, n
  ! The first column element of Selected row
  ! which we want to eliminate from all other
  ! rows
  first = a(i - 1, i - 1)
  do row = i, n
   ! This is the element which we will multiply
   ! to make that [row,row] element zero
   second = a(row, i - 1)
   ! Main loop that does the elimination work
   do col = i - 1, m
    ! Check the Gauss Elimination Method
    a(row, col) = a(row, col) - &
    second * a(i - 1, col)/first
   end do
  end do
 end do
 return
end subroutine GaussElimination

! This subroutine solves the equations
! using back substitution
! It starts from the last row and goes to first
subroutine SolveX(a, n, m, x)
 implicit none
 integer :: n, m
 real :: a(n, m)
 real :: x(n)
 integer :: row, col
 real :: sum

 ! We want to sum the multiplication of x value and it's
 ! coefficient and then subtract it from the coefficient C
 ! and divide that with the coefficient of x for which we
 ! want to solve
 do row = n, 0, -1
  sum = 0
  ! Multiply x and coefficient and compute it's sum
  do col = row + 1, m - 1
   sum = sum + a(row, col) * x(col)
  end do
  ! Subtract sum from coefficient C and divide with
  ! the coefficient of X
  x(row) = (a(row, m) - sum)/a(row, row)
 end do
end subroutine SolveX

! This is just a utility subroutine that prints
! a matrix supplied with appropriate dimensions
subroutine PrintMatrix(a, m, n)
 implicit none
 integer :: m, n
 real, intent(in out) :: a(m, n)
 integer :: i, j
 do i = 1, m
  do j = 1, n
   write(*, '(f18.2)', advance = "no") a(i, j)
  end do
  print *, ''
 end do
end subroutine PrintMatrix

! This is another utility function that prints
! Array of supplied length
subroutine PrintArray(a, m)
 implicit none
 integer :: m
 real, intent(in out) :: a(m)
 integer :: i
 print *, ''
 do i = 1, m
  write(*, '(f18.2) ', advance = "no") a(i)
 end do
 print *, ''
end subroutine PrintArray

! The main program container
program SolveEquations
 implicit none
 real, dimension(3, 4) :: a = &
 reshape((/2, 1, 3, 4, 3, 6, 3, 2, 1, 13, 9, 16/), (/3, 4/))
 real, dimension(3) :: x
 integer :: rows = 3, cols = 4
 print *, "Input System of equations:"
 call PrintMatrix(a, rows, cols)
 call GaussElimination(a, rows, cols)
 print *, "Output:"
 call PrintMatrix(a, rows, cols)
 call SolveX(a, rows, cols, x)
 print *, "Solution:"
 call PrintArray(x, rows)
end program SolveEquations

! OUTPUT
!  Input System of equations:
!               2.00              4.00              3.00             13.00
!               1.00              3.00              2.00              9.00
!               3.00              6.00              1.00             16.00
!  Output:
!               2.00              4.00              3.00             13.00
!               0.00              1.00              0.50              2.50
!               0.00              0.00             -3.50             -3.50
!  Solution:
!               1.00              2.00              1.00