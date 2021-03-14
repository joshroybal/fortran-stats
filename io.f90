! input data sets x and y from file
subroutine rdfile(x, y, n, infile)
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, dimension(n), intent(out) :: x, y
   character (len=*), intent(in) :: infile
   ! local data
   integer :: i
   ! processing
   open (7,file=infile,status='old')
   read (7,*) (x(i),i=1,n)
   read (7,*) (y(i),i=1,n)
   close (7)
end subroutine rdfile

! dump data sets x and y to file
subroutine wtfile(x, y, n, ofile)
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, dimension(n), intent(in) :: x, y
   character (len=*), intent(in) :: ofile
   ! local data
   integer :: i
   ! processing
   open (8,file=ofile)
   do i = 1, n
      write (8,*) x(i)
   end do
   do i = 1, n
      write (8,*) y(i)
   end do
   close (8)
end subroutine
