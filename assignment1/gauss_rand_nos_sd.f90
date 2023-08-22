program gauss_rand_nos
      implicit none

      ! Variables to generate random numbers
      
      integer, allocatable :: seed(:)
      integer:: dt_vals(8)
      integer :: seed_size, i
      real *8 :: r, s

      ! Other variables

      real *8 :: A, p, q, p1, q1
      integer :: n, bin1, bin2, m
      real *8, allocatable :: rand1(:), rand2(:)

      real *8 :: width_1, width_2, maxs_1, mins_1, maxs_2, mins_2
      integer, allocatable :: bins1(:), bins2(:)
      real *8 :: sd, mean, var, st, me
      integer :: len1, len2

      !-----------------------------------------------------------------------
      
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
     
      !-----------------------------------------------------------------------

      ! Generating normal distributed random numbers.
      
      open(unit=16, file='gauss_rand_nos.dat', action='write')
      
      print *, ""
      
      print *, 'How many normal distributed random numbers needed'
      read *, n
 
      print *, ""
      
      print *, 'Enter mean for the distribution'
      read *, me

      print *, ""
      
      print *, 'Enter the standard deviation'
      read *, st
      
      allocate(rand1(1:n))
      allocate(rand2(1:n))
      
      ! Box-Muller Method
      ! Taken uniformly distributed random numbers belonging to [-1,1]
      ! We get Normal distributed random numbers x's with mean 0 and sd 1
      ! To get distribution of numbers z's with mean m and sd s, we change 
      ! random numbers to z = x*s + m

      do i=1,n
        call random_number(p1)
        call random_number(q1)
        p = 2.0*p1-1.0
        q = 2.0*q1-1.0
        r=p*p + q*q
        if (r<1 .and. r>0.0) then
            A = sqrt(-2*log(r))
            rand1(i) = A*p/sqrt(r)*st + me
            rand2(i) = A*q/sqrt(r)*st + me
            write(16,*) rand1(i), rand2(i)
        end if
      end do

      !--------------------------------------------------------------------

      ! To find the mean and standard deviation
      ! The arrays include zeros when there was no value produced due
      ! to the above if statement. So extracting the actual length of
      ! the array. That is the number of non zero values to calculate
      ! mean and standard deviation.

      len1 = 0
      len2 = 0
      
      do i=1, size(rand1)
        if (rand1(i) .ne. 0) then
            len1 = len1 + 1
        end if
      end do
      
      do i=1, size(rand2)
        if (rand2(i) .ne. 0) then
            len2 = len2 + 1
        end if
      end do

      mean = sum(rand1)/real(len1)
      var = sum(rand1**2)/real(len1) - mean**2
      sd = sqrt(var)
      
      print *, "Mean               =", mean
      print *, "Standard deviation =", sd
 
      !-------------------------------------------------------------------

      ! Creating bins to plot histogram
      print *, ""
      print *, "Enter how many bins should be created."
      read *, m
      maxs_1 = maxval(rand1)
      mins_1 = minval(rand1)
      width_1 = (maxs_1-mins_1)/real(m)
      
      maxs_2 = maxval(rand2)
      mins_2 = minval(rand2)
      width_2 = (maxs_2-mins_2)/real(m)

      ! Allocating dimension of bins from mins/width to max/width,
      ! so that there are n bins between min and max.

      allocate(bins1(int(mins_1/width_1):int(maxs_1/width_1)))
      allocate(bins2(int(mins_2/width_2):int(maxs_2/width_2)))

      bins1 = 0
      bins2 = 0

      ! Distributing the sums into specific bin based on the
      ! integer part of the value.
      ! Increasing value of bin by 1 if a value encounters the bin
      ! There is an if statement below for the same reason mentioned 
      ! while calculating mean and sd

      do i=1,n
        if (rand1(i) .ne. 0.0) then
            bin1 = floor(rand1(i)/width_1)
            bins1(bin1) = bins1(bin1) + 1
        end if
      enddo
      
      do i=1,n
        if (rand2(i) .ne. 0.0) then
            bin2 = floor(rand2(i)/width_2)
            bins2(bin2) = bins2(bin2) + 1
        end if
      enddo

      ! Writing bins to a file
      
      open(unit=17, file='bins1.dat', action='write')
      open(unit=18, file='bins2.dat', action='write')
      
      do i=int(mins_1/width_1), int(maxs_1/width_1)
        write(17,*) i*width_1, bins1(i)
      end do
     
      do i=int(mins_2/width_2), int(maxs_2/width_2)
        write(18,*) i*width_2, bins2(i)
      end do
  
end program gauss_rand_nos


