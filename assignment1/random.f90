program random
      implicit none
      real :: r, sum, mean, var, sd
      real, allocatable :: rand(:)
      integer :: i, n

      print*, "Number of random numbers"
      read *, n

      allocate(rand(1:n))
      do i=1,n
        call random_number(r)
        rand(i)=r
      end do

      mean = sum(rand)/real(n)

      var = (sum(rand**2)/real(n)) - mean**2
      sd = sqrt(var)

      print *, mean, sd

end program random
