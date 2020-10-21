subroutine report(x, n)
   use stats
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, dimension(n), intent(in) :: x
   ! processing
   write (*,*) 'n = ', n
   write (*,*) 'minimum = ', minval(x, 1)
   write (*,*) 'maximum = ', maxval(x, 1)
   write (*,*) 'mean = ', mean(x, n)
   write (*,*) 'median = ', median(x, n)
   write (*,*) 'population variance = ', popvar(x, n)
   write (*,*) 'sample variance = ', samvar(x, n)
   write (*,*) 'population standard deviation = ', popstd(x, n)
   write (*,*) 'sample standard deviation = ', samstd(x, n)
   write (*,*) 'median deviation = ', median_deviation(x, n)
   write (*,*) 'mean deviation = ', mean_deviation(x, n)
   write (*,*) 'skewness = ', skewness(x, n)
   write (*,*) 'nonparametric skew = ', nonparametric_skew(x, n)
end subroutine report
