! File:   Quadratic Equation.f08
! Author: Aakash Gajjar
! Created on October 13, 2016, 2:15 PM
!
! Subject: Finds solution of quadratic
! equation

function solve2D(A, B, C, root1, root2)
 implicit none
 real :: solve2D, delta, uRoot
 real, intent(out) :: root1, root2
 integer, intent(in) :: A, B, C
 uRoot = B * B - 4 * A * C
 if (uRoot < 0) then
  delta = sqrt((-1.) * uRoot)
 else
  delta = sqrt((1.) * uRoot)
 endif
 root1 = ((-1) * B + delta)/(2 * A)
 root2 = ((-1) * B - delta)/(2 * A)
 solve2D = 0
end function solve2D

program quadratic
 implicit none
 integer :: A, B, C, out
 real :: solve2D, root1 = 0., root2 = 0.
 print *, "Enter Coefficients of X^2, X and Constant"
 read *, A, B, C
 out = solve2D(A, B, C, root1, root2)
 print "(a,i1,a,i1,a,i1,a,f9.5,a,f9.5)", "Equation: ", &
 A, "x^2+", B, "x+", C, " has two roots ", root1, " &", root2
end program quadratic

! OUTPUT
! Enter Coefficients of X^2, X and Constant
!  1 2 3
! Equation: 1x^2+2x+3 has two roots   0.41421 & -2.41421