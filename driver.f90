program driver
   use stats
   implicit none
   interface
      subroutine rdfile(x, y, n, fname)
         integer, intent(in) :: n
         real, dimension(n), intent(out) :: x, y
         character (len=*), intent(in) :: fname
      end subroutine rdfile
      subroutine wtfile(x, y, n, fname)
         integer, intent(in) :: n
         real, dimension(n), intent(in) :: x, y
         character (len=*), intent(in) :: fname
      end subroutine
      subroutine report(x, n)
         integer, intent(in) :: n
         real, dimension(n), intent(in) :: x
      end subroutine report
   end interface
   ! variable and array declarations
   integer, parameter :: N = 1000
   integer :: i
   real, dimension(N) :: x, y
   ! processing
   call srand(time())
   ! create and dump data sets
   call uniform(x, N)
   call uniform(y, N)
   call wtfile(x, y, N, 'uniform.dat')
   call randnorm(x, N)
   call randnorm(y, N)
   call mergesort(x, N)
   call mergesort(y, N)
   call wtfile(x, y, N, 'normal.dat')
   ! uniform tests
   call rdfile(x, y, N, 'uniform.dat')
   write (*,*) 'random uniform distributions'
   write (*,*) 'x'
   call report(x, N)
   write (*,*) 'y'
   call report(y, N)
   write (*,*) 'x y'
   write (*,*) 'covariance = ', covar(x, y, N)
   write (*,*) 'correlation coefficient = ', correlation_coefficient(x, y, N)
   ! randnorm tests
   call rdfile(x, y, N, 'normal.dat')
   write (*,*) 'random normal distributions'
   write (*,*) 'x'
   call report(x, N)
   write (*,*) 'y'
   call report(y, N)
   write (*,*) 'x y'
   write (*,*) 'covariance = ', covar(x, y, N)
   write (*,*) 'correlation coefficient = ', correlation_coefficient(x, y, N)
end program driver
