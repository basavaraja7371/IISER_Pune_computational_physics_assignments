program random_walk
      implicit none

      ! Variables to generate random numbers
      
      integer, allocatable :: seed(:)
      integer:: dt_vals(8)
      integer :: seed_size
      real *8 :: r

      ! Other variables

      integer, allocatable :: sums(:)
      integer :: sum_r, a, i, j, ni, nj
      integer :: n, maxs, mins, bin, xi, x_dxi
      real *8 :: width, prob, x, x_dx, mean, var, sd
      real *8, allocatable :: bins(:)

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
            if (r<0.5) then
                a = 1
            else
                a = -1
            end if
            sum_r = sum_r + a
        end do
        sums(i) = sum_r
      end do

      ! Opening file to store sum values 

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

      ! Allocating the size of array bins Distributing the sums into specific
      ! bin based on the integer part of the value.
      ! increasing value of bin by 1 if a value encounters the bin.

      allocate(bins(floor(mins/width):floor(maxs/width)))
      bins = 0

      do i=1,ni                      
        bin = nint(sums(i)/width)   
        bins(bin) = bins(bin) + 1    
      enddo                         

      ! Writing bins to a file
      open(unit=17, file='bins.dat', action='write')
      open(unit=18, file='norm_bins.dat', action='write')

      ! Normalizing the values by dividing by ni before writing it to file.
      ! First column writes the range of sum in terms of width.

      do i=floor(mins/width), floor(maxs/width)
        write(18,*) i*width, bins(i)/real(ni)
        write(17,*) i*width, bins(i)
      end do

      ! Fitting the curve
      ! Finding mean, variance and standard deviation
      
      print *, ""
      mean = sum(sums)/real(ni)
      print *, 'Mean               = ' , mean

      var = sum((sums)**2)/real(ni) - mean**2
      print *, 'Variance           = ', var
      print *, 'Standard deviation = ', sqrt(var)




      ! To get rough estimate of the probability    
      
      print *, ""
      print *,"Enter the intervals; x and x+dx for probability"   
      read *, x, x_dx


      ! Converting that values to the bins
      
      xi = floor(x/width)                         
      x_dxi = floor(x_dx/width)              
      
      ! Now adding the values of probability from x to x+dx, i.e., bin value divided by ni

      prob = 0
      do i=xi, x_dxi                              
        prob = prob + (bins(i)/real(ni))             
      end do                                       
      print *,""
      print *, 'Probability(x to x+dx) = ', prob             


end program random_walk

