! File:   Matrix Multiplication.f08
! Author: Aakash Gajjar
! Created on August 30, 2016, 8:29 PM
!
! Subject: Implementation of Matrix
!          Multiplication using
!          Fortran Subroutines
!

! Multiplies two matrix
! Matrix-1: a of dim: n,m in
! Matrix-2: b of dim: m,l in
! Matrix-3: b of dim: n,l out
subroutine MatrixAB(a, b, c, n, m, l)
 integer :: n, m, l
 real, intent(in out), dimension(n, m) :: a
 real, intent(in out), dimension(m, l) :: b
 real, intent(in out), dimension(n, l) :: c
 integer :: i, j, k
 real :: sum
 ! You should know how to multiply two matrix
 do i = 1, n
  do j = 1, l
   sum = 0.0
   do k = 1, m
    sum = sum + a(i, k) * b(k, j)
   end do
   c(i, j) = sum
  end do
 end do
end subroutine MatrixAB

! This subroutine reads a matrix of
! dimension nXm
subroutine ReadMatrix(a, n, m)
 implicit none
 integer :: n, m
 real, intent(in out), dimension(n, m) :: a
 real :: buffer
 integer :: i, j
 write(*, '(Ai1A1i1)') "Enter a matrix of ", n, "x", m
 do i = 1, n
  do j = 1, m
   read *, buffer
   a(i, j) = real(buffer)
  end do
 end do
end subroutine ReadMatrix

! This subroutine prints a matrix of
! dimension nXm
subroutine PrintMatrix(a, n, m)
 implicit none
 integer :: n, m
 real, intent(in out), dimension(n, m) :: a
 integer :: i, j
 write(*, '(Ai1A1i1)') "Printing a matrix of ", n, "x", m
 do i = 1, n
  do j = 1, m
   write (*, '(f16.2)', advance = 'no') a(i, j)
  end do
  print *, ''
 end do
end subroutine PrintMatrix

! This main program container
program MatrixMultiplication
 implicit none
 integer :: n = 3, m = 3, l = 3
 real, dimension(3, 3) :: a, b, c
 print *, "This program multiplies 2 matrix and displays result"
 call ReadMatrix(a, n, m)
 call ReadMatrix(b, m, l)
 call MatrixAB(a, b, c, n, m, l)
 call PrintMatrix(c, n, l)
end program MatrixMultiplication