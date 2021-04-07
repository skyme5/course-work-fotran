! File:   Average From File.f08
! Author: Aakash Gajjar
! Created on October 13, 2016, 2:26 PM
!
! Subject: This program writes 100 random float
! numbers to a file and then closes the file.
! Then again it opens the same file and reads those
! numbers and prints them.
! THE FILE NAME is 'average'

program calculateAverage
 implicit none
 integer :: error, i
 integer :: length
 real :: j
 length = 100
 ! This line opens a new file
 open(23, file = "average")
 do i = 1, length
  write(23,*) rand()*100
 end do
 rewind(23)
 do i = 1, length
  read(23,*) j
  print *, j
 end do
 close(23)
end program calculateAverage