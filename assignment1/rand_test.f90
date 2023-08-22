program rand_test
      implicit none

      ! Variables to generate random numbers

      integer, allocatable :: seed(:)
      integer:: dt_vals(8)
      integer :: seed_size, i
      real *8 :: r

      ! Other variables
      real *8, allocatable :: rand_nos(:)
      integer :: k, n, bin, m
      real *8 :: width, maxs, mins
      integer, allocatable :: bins(:)
      real *8 :: mean, var, sd, ck, s


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

      print *, "How many random numbers to be generated?"
      read *, m

      open(unit=16, file='rand_nos.dat', action='write')

      allocate(rand_nos(1:m))

      do i=1,m
         call random_number(r)
         rand_nos(i) = r
         write(16,*) r
      end do

      ! Creating bins to plot histogram

      print *, "Enter how many bins should be created."
      read *, n

      maxs = maxval(rand_nos)
      mins = minval(rand_nos)
      width = (maxs-mins)/real(n)

      ! Allocating the size of array bins Distributing the random numbers
      ! into specific bin based on the integer part of the value.(Bucket sort)
      ! increasing value of bin by 1 if a value encounters the bin.

      allocate(bins(int(mins/width):int(maxs/width)))
      bins = 0

      do i=1,10000
        bin = floor(rand_nos(i)/width)
        bins(bin) = bins(bin) + 1
      enddo

      open(unit=17, file='bins.dat', action='write')

      do i=int(mins/width), int(maxs/width)
        write(17,*) i*width, bins(i)
      end do

      ! For scatter plot writing x_i and x_(i+1) in a file

      open(unit=18, file='scatter.dat', action='write')

      do i=1, m-1
        write(18,*) rand_nos(i), rand_nos(i+1)
      end do

      ! Calculating mean, sd and correlation function.

      mean = sum(rand_nos)/real(m)

      var = (sum(rand_nos**2)/real(m))-mean**2

      sd = sqrt(var)
      print *, 'Mean               =', mean
      print *, 'Standard deviation =', sd

      ! Opening file to store ck values

      open(unit=19, file='correlation.dat', action='write')

      do k=0,m-1
        s = 0
        do i=1,m-k
            s = s + (rand_nos(i)*rand_nos(i+k))
        end do
        s=s/real(m-k)
        ck = (s-mean**2)/var
        write(19,*) k, ck
      end do

       



end program rand_test
