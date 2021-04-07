! File:   Check Class.f08
! Author: Aakash Gajjar
! Created on July 13, 2016, 2:21 PM
!
! Subject: Calculate Percentage and outputs
! the corresponding class
! First Class: >=60
! Second Class: 40<=x<60
! Fail: <40
! Allowed No of Subject: 3
!

program calculatePercentage
 implicit none
 integer :: sub1, sub2, sub3
 real :: percentage
 do while (.true.)
  print *, "Enter 3 Subjects Marks (out of 100)"
  read *, sub1, sub2, sub3
  percentage = (sub1 + sub2 + sub3)/3
  if (percentage >= 60) then
   print *, "First Class"
  else if (percentage >= 40 .and. percentage < 60) then
   print *, "Second Class"
  else
   print *, "Failed"
  endif
 enddo
end program calculatePercentage

! OUTPUT
! Enter 3 Subjects Marks (out of 100)
! 50 50 50
! Second Class
