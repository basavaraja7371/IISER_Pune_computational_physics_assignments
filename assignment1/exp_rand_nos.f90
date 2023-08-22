program exp_rand_nos
      implicit none

      ! Variables to generate random numbers
      
      integer, allocatable :: seed(:)
      integer:: dt_vals(8)
      integer :: seed_size, i
      real *8 :: r

      ! Other variables

      real *8 :: y
      integer :: n, bin, m
      real *8, allocatable :: rand(:)

      real *8 :: width, maxs, mins
      integer, allocatable :: bins(:)

      ! Generating random numbers using date and time as seed
      
      call random_seed(size=seed_size)
      allocate(seed(seed_size))

      call random_seed(get=seed)
      call date_and_time(values=dt_vals)

      do i=1,8
        seed(i)=dt_vals(i)
      end do

      call random_seed(put=seed)
      deallocate(seed)

      ! Generating exponentially distributed random numbers.
      ! y(x)=-ln(1-x)

      open(unit=16, file='exp_rand_nos.dat', action='write')

      print *, 'How many exponentially distributed random numbers needed'
      read *, n

      allocate(rand(n))

      do i=1,n
        call random_number(r)
        rand(i) = y(r)
        write(16,*) y(r)
      end do


      ! Creating bins to plot histogram

      print *, "Enter how many bins should be created."
      read *, m
      maxs = maxval(rand)
      mins = minval(rand)
      width = (maxs-mins)/real(m)


      ! Allocating dimension of bins from mins/width to max/width,
      ! so that there are n bins between min and max.

      allocate(bins(int(mins/width):int(maxs/width)))

      bins = 0

      ! Distributing the sums into specific bin based on the
      ! integer part of the value.
      ! Increasing value of bin by 1 if a value encounters the bin

      do i=1,n
        bin = floor(rand(i)/width)
        bins(bin) = bins(bin) + 1
      enddo


      ! Writing bins to a file
      open(unit=17, file='bins.dat', action='write')

      do i=int(mins/width), int(maxs/width)
        write(17,*) i*width, bins(i)
      end do

end program exp_rand_nos

! Defining Function
real *8 function y(xk) result(k)

    implicit none
    real *8 :: xk

    k = -log(1-xk)

end function y
