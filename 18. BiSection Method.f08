! File:   BiSection Method.f08
! Author: Aakash Gajjar
! Created on August 26, 2016, 8:33 PM
!
! Subject: Implementation BiSection Method
!          to find root of 1D equation in
!          a given interval
!

! The FUNCTION whose root we want
! to find
function f(x)
 implicit none
 real :: f,x
 f = x*x-25.0
end function f

! See the Reference given
function BiSection(a,b)
 implicit none
 real :: BiSection,a,b
 real :: f
 real :: low,high,middle
 ! Maximum Tolerance
 real :: tolerance = 1.0e-6

 ! Choose low and high interval
 ! value
 low = a
 high = b
 do while(abs(high-low)>tolerance)
  ! Find mid point and compare
  middle = low + (high-low)/2
  ! Swap whoever is more
  if (f(low)*f(middle)>0) then
   low = middle
  else
   high = middle
  endif
 end do

 ! Increases Significance just by
 ! taking mid for one more time
 BiSection = low + (high-low)/2

end function BiSection

! The main program container
program BiSectionMethod
 implicit none
 real :: BiSection
 print *,BiSection(0.0,10.0)
end program BiSectionMethod

! OUTPUT
! 4.99999952