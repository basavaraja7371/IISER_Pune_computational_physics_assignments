program random_sums
      implicit none

      ! Variables to generate random numbers
      
      integer, allocatable :: seed(:)
      integer:: dt_vals(8)
      integer :: seed_size
      real *8 :: r

      ! Other variables

      real *8, allocatable :: sums(:)
      integer :: i, j, ni, nj, n, bin
      real *8 :: width, prob,sum_r, maxs, mins 
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

      print *, "Enter number of random numbers to be summed."
      read *, nj

      print *, "Enter how many times to sum the random numbers."
      read *, ni

      allocate(sums(1:ni))

      do i=1,ni
        sum_r = 0
        do j=1,nj
            call random_number(r)
            sum_r = sum_r + r
        end do
        sums(i) = sum_r
      end do
      

      !Opening files to store sums

      open(unit=16, file='random_sums.dat', action='write')

      do i=1,ni
        write(16,*) sums(i)
      end do

      ! Creating bins to plot histogram

      print *, "Enter how many bins should be created."
      read *, n
      maxs = maxval(sums)
      mins = minval(sums)
      width = (maxs-mins)/real(n)
      
      
      ! Allocating dimension of bins from mins/width to max/width,
      ! so that there are n bins between min and max.

      allocate(bins(int(mins/width):int(maxs/width)))      
      
      bins = 0
      
      ! Distributing the sums into specific bin based on the 
      ! integer part of the value. 
      ! Increasing value of bin by 1 if a value encounters the bin
      
      do i=1,ni                     
        bin = floor(sums(i)/width)  
        bins(bin) = bins(bin) + 1    
      enddo                          
      
      
      ! Writing bins to a file
      open(unit=17, file='bins.dat', action='write')
      open(unit=18, file='norm_bins.dat', action='write')

      do i=int(mins/width), int(maxs/width)
        write(17,*) i*width, bins(i) 
        write(18,*) i*width, bins(i)/real(ni)
      end do
           
    
end program random_sums
