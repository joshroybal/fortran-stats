! Modern Fortran statistical computations module
module stats
contains
   ! function returns real mean of all array elements
   function mean(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function result location
      real :: s
      ! local variables
      integer :: i
      ! processing
      s = 0.
      do i = 1, n
         s = ((i-1)*s+x(i))/i
      end do
   end function mean

   ! function returns real population variance of x
   function popvar(x, n, m) result(var)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), optional :: m
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: var
      ! local variables
      integer :: i
      real :: mu
      ! processing
      if (present(m)) then
         mu = m
      else
         mu = mean(x, n)
      end if
      var = 0.
      do i = 1, n
         var = (((i-1)*var)+(x(i)-mu)**2)/i
      end do   
   end function popvar

   ! function returns real sample variance of x
   function samvar(x, n) result(var)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: var
      ! local variables
      integer :: i
      real :: m
      ! processing
      m = mean(x, n)
      var = 0.
      do i = 1, n
         var = var + (x(i) - m)**2
      end do
      var = var / (n - 1)
   end function samvar

   ! function returns real population standard deviation of x
   function popstd(x, n) result(std)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: std
      ! processing
      std = sqrt(popvar(x, n))
   end function popstd

   ! function returns real sample standard deviation of x
   function samstd(x, n) result(std)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: std
      ! processing
      std = sqrt(samvar(x, n))
   end function samstd

   ! functions returns index of kth element of x
   ! C. A. R. Hoare's algorithm
   ! implementation works on index array only - preserves data set array
   function quickselect(k, n, x) result(v)
      implicit none
      ! dummy arguments
      integer, intent(in) :: k, n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: v
      ! local variables
      integer :: i, j, left, right, tmp
      integer, dimension(n) :: idx
      real :: pivot
      ! processing
      do i=1,n
         idx(i)=i
      end do
      left=1
      right=n
      do while (left < right)
         pivot=x(idx(k))
         i=left
         j=right
         do
            do while (x(idx(i)) < pivot)
               i=i+1
            end do
            do while (pivot < x(idx(j)))
               j=j-1
            end do
            if (i <= j) then
               tmp=idx(i)
               idx(i)=idx(j)
               idx(j)=tmp
               i=i+1
               j=j-1
            end if
            if (i > j) exit
         end do
         if (j < k) left=i
         if (k < i) right=j
      end do
      v=x(idx(k))
   end function quickselect

   ! function returns median of x
   function median(x, n) result(mdn)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: mdn
      ! processing
      if (mod(n, 2) == 1) then
         mdn=quickselect(n/2+1, n, x)
      else
         mdn=(quickselect(n/2, n, x) + quickselect(n/2+1, n, x))/2
      end if
   end function median

   !funcition returns median absolute deviation of x
   function median_deviation(x, n) result(mad)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: mad
      ! local variables
      integer :: i
      real :: mdn
      real, dimension(n) :: y
      ! processing
      mdn = median(x, n)
      do i = 1, n
         y(i) = abs(mdn - x(i))
      end do
      mad = median(y, n)
   end function median_deviation

   ! function computes the mean absolute deviation of x
   function mean_deviation(x, n) result(aad)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: aad
      ! local variables
      integer :: i
      real :: m
      ! processing
      m = mean(x, n)
      aad = 0.
      do i=1,n
         aad = aad + abs(x(i) - m)
      end do
      aad = aad / n
   end function mean_deviation

   ! function computes the skewness of x (expected value 1/N)
   function skewness(x, n) result(skw)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: skw
      ! local variables
      integer :: i
      real :: m, s
      ! processing
      m = mean(x, n)
      s = 0.
      do i = 1, n
         s = s + ((x(i) - m)**3) / n
      end do
      skw = s / samstd(x, n)**3
   end function skewness

   ! nonparametric skew
   function nonparametric_skew(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(in) :: x
      ! function result location
      real :: s
      ! processing
      s = (mean(x, n) - median(x, n)) / popstd(x, n)
   end function nonparametric_skew

   ! function computes the covariance of data sets x and y
   function covar(x, y, n) result(cov)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(in) :: x, y
      ! function return location
      real :: cov
      ! local variables
      integer :: i
      real mx, my, dx, dy
      ! processing
      mx = mean(x, n)
      my = mean(y, n)
      cov = 0.
      do i = 1, n
         dx = x(i) - mx
         dy = y(i) - my
         cov = cov + dx * dy
      end do
      cov = cov / n
   end function covar

   function correlation_coefficient(x, y, n) result(rho)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(in) :: x, y
      ! function return location
      real :: rho
      ! processing
      rho = covar(x, y, n) / (samstd(x, n) * samstd(y, n))
   end function correlation_coefficient

   ! iterative merge sort algorithm (fastest stable iterative?)
   subroutine mergesort(x, n)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(inout) :: x
      ! local variables and arrays
      integer :: i, j, l, m, r
      real, dimension(n) :: work
      ! processing
      j = 1
      do
         if (j >= n) exit
         do i = 1, n, 2*j
            l = i
            m = min(i + j - 1, n)
            r = min(i + 2*j - 1, n)
            if ( l >= n .or. m >= n .or. r > n) exit
            call mergearr(x(l:m), x(m+1:r), work(l:r), j, r-m)
            x(l:r) = work(l:r)
         end do
         j = 2 * j
      end do
      x = work
   end subroutine mergesort

   ! subroutine merges two sorted arrays into a third sorted array
   subroutine mergearr(A, B, C, na, nb)
      implicit none
      ! dummy arguments
      integer, intent(in) :: na, nb
      real, dimension(na), intent(in) :: A
      real, dimension(nb), intent(in) :: B
      real, dimension(na+nb), intent(out) :: C
      ! local variables
      integer :: i, j, k
      ! processing
      i = 1
      j = 1
      k = 1
      do
         if (i > na .or. j > nb) exit
         if (A(i) < B(j)) then
            C(k) = A(i)
            i = i + 1
         else
            C(k) = B(j)
            j = j + 1
         end if
         k = k + 1
      end do
      do
         if (i > na) exit
         C(k) = A(i)
         k = k + 1
         i = i + 1
      end do
      do
         if (j > nb) exit
         C(k) = B(j)
         k = k + 1
         j = j + 1
      end do
   end subroutine mergearr

   ! subroutine fills array with uniformly distributed rand
   subroutine uniform(x, n)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(out) :: x
      ! local variables
      integer :: i
      ! processing
      do i = 1, n
         x(i) = rand()
      end do
   end subroutine uniform

   ! subroutine fills array with standard normal distribution
   subroutine normal(x, n)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(out) :: x
      ! local variables
      integer :: i
      real, parameter :: pi = 4.*atan(1.)
      real :: mu, sd
      ! processing
      mu = n/2.
      sd = n/6.018
      do i = 1, n
         x(i) = sd*(1./(sd*sqrt(2.*pi)))*exp(-.5*((i-mu)/sd)**2)
      end do
   end subroutine normal

   ! box-muller normally distributed mapping of rands
   subroutine box_muller(x, n)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(out) :: x
      ! local variables
      real, parameter :: pi = 4.*atan(1.)
      integer :: i
      ! processing
      do i = 1, n
         if (mod(i,2) == 0) then
            x(i) = sqrt(-2.*log(rand()))*sin(2.*pi*rand())
         else
            x(i) = sqrt(-2.*log(rand()))*cos(2.*pi*rand())
         end if
      end do
   end subroutine box_muller

   ! subroutine randnorm is normally distributed analog of
   subroutine randnorm(x, n)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, dimension(n), intent(out) :: x
      ! local variables
      real :: a, b, c, d, r
      ! processing
      call box_muller(x, n)
      a = minval(x, 1)
      b = maxval(x, 1)
      c = max(-a, b)
      r = 2*c
      d = (r-(b-a))/2.
      x = (x-a+d)/r
   end subroutine
end module stats
