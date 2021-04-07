! File:   Matrix Eigen Value.f08
! Author: Aakash Gajjar
! Created on August 21, 2016, 2:53 PM
!
! Subject: This program implements a
! Power method that is used to find
! Eigen values of the system of equation
!

program PowerMethod
 implicit none

 integer, parameter :: Row = 3, Col = 3
 real, dimension(Row, Col) :: matrixA, matrixB, matrixC
 real :: eigenValue, oldEigenValue
 integer :: i, j, k, m

 print *, "Enter Matrix Row-Wise"

 ! Read matrix matrixA and initialize
 ! matrixB
 do i = 1, Row
  print *, "Enter row no", i
  do j = 1, Col
   read *, matrixA(i, j)
   matrixB(i, j) = 0.0
  end do
 end do

 ! Start Process
 matrixB(1, 1) = 1.0
 oldEigenValue = 0.0

 do m = 0, 100

  !Multiply matrix
  do i = 1, Row
   do j = 1, Col
    matrixC(i, j) = 0.0
    do k = 1, Col
     matrixC(i, j) = &
     matrixC(i, j) + matrixA(i, k) * matrixB(k, j)
    end do
   end do
  end do

  !Initialize eigenValue with first element of matrixC
  eigenValue = matrixC(1, 1)
  do i = 1, Row
   do j = 1, Col
    if (matrixC(i, j) >= eigenValue) then
     eigenValue = matrixC(i, j)
    end if
   end do
  end do

  !Check if new value is equal to old value
  ! if it is we terminate process here
  if (eigenValue == oldEigenValue) then
   exit
  end if

  !Assign new to old
  oldEigenValue = eigenValue

  !Normalize matrixC
  !Assign matrixC TO matrixB
  do i = 1, Row
   do j = 1, Col
    matrixC(i, j) = matrixC(i, j)/eigenValue
    matrixB(i, j) = matrixC(i, j)
   end do
  end do

 end do

 print *, "Eigen Value = ", eigenValue

 !Print matrixC
 do i = 1, Row
  do j = 1, Col
   print *, matrixC(i, j)
  end do
 end do

end program PowerMethod

! OUTPUT
!  Enter Matrix Row-Wise
!  Enter row no           1
! 2
! 4
! 3
!  Enter row no           2
! 1
! 3
! 2
!  Enter row no           3
! 3
! 6
! 1
!  Eigen Value =    7.70009804
!    7.46673203
!    0.00000000
!    0.00000000
!    4.86520243
!    0.00000000
!    0.00000000
!    7.70009804
!    0.00000000
!    0.00000000
