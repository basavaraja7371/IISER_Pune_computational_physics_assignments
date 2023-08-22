program random_numbers
    implicit none

    integer, allocatable :: seed(:)
    integer :: seed_size
    real *8 :: r

    real *8 :: sums, sum1, avg, error
    integer :: i, j

    ! Q1a. Printing 10 random numbers

    do i=1,10
        call random_number(r)
        print *, r
    end do

    !Q1b. Printing 10 random numbers in a file

    open(unit=16, file='test_ran.dat', action='write')

    do i=1,10
        call random_number(r)
        write(16,*) r
    end do

   ! Q1c. Writing the comment

    write(16,*) "" ! A blanck line
    write(16,*) "Changing seed and generating 10 new random numbers"

    ! Q1d. Changing the seed and generating new set of random numbers.

    call random_seed(size = seed_size) ! knowing seed size
    allocate(seed(seed_size))          ! allocating that size to seed
    seed = 30101996                    ! new seed set to 16021997(it's an array)
    call random_seed(put=seed) 

    ! printing another 10 random numbers with seed above
    
    write (16,*) "" ! A blank line

    do i=1,10
        call random_number(r)
        write (16,*) r
    end do


    ! Q1e. Writing the comment and calculating average

    write(16,*) "" ! blank line
    write(16,*) "Now calculating average of 10 random numbers"

    sums = 0

    do i=1,10
        call random_number(r)
        sums = sums + r
    end do

    avg = sums/10.0

    write(16,*) ""
    write(16,*) "The average of 10 random numbers is", avg
    
    ! Q1f. Calculating average of 100,10000,1000000 random numbers
    ! Q1g. Calculating difference between 0.50 and averages above 
    !      calculated

    write(16,*) ""
    write(16,*) "Now calculationg the averages of 100,10000, &
        1000000 random numbers"
    
    j=100
2   sum1 = 0
    
    do i=1,j
        call random_number(r)
        sum1=sum1+r
    end do

    avg = sum1/real(j)
    error = abs(0.50d0 - avg)

    write(16,*) ""
    write(16,*) "The average of ",j, "random numbers is", avg
    write(16,*) "And the error in the average value is", error
    write(16,*) ""
    write(16,*) ""

    j=j*100
    if (j<1000001) goto 2

    write(16,*) ""
    write(16,*) "We can easily see the as we take more random &
        numbers, the average of them goes nearer to 0.50."
    write(16,*) "Also the error in the average decrease and &
        approach zero "




end program random_numbers
