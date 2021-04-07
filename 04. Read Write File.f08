! File:   Read Write File.f08
! Author: Aakash Gajjar
! Created on July 19, 2016, 2:24 PM
!
! Subject: Write into a file and reads the same
!

program rwfile
 implicit none
 integer :: i
 open (unit = 13, file = "input", &
 status = "new", action = "write", iostat = i)
 if (i /= 0) then
  print *, "Error Opening file"
  stop
 endif
 do i = 1, 10
  write (unit = 13, fmt = *), i, i * i
 enddo
 close(unit = 13)
end program rwfile