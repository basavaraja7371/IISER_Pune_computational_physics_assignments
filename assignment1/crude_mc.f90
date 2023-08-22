program mc_integration
      implicit none

       ! Variables to generate random numbers

       integer, allocatable :: seed(:)
       integer:: dt_vals(8)
       integer :: seed_size, i
       real *8 :: r, s

       ! Other variables

       real *8 :: crude_mc, sigma, p, x, y1
       integer :: n 


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
       
       print *, "How many random points shoud be used?"
       read *, n

       crude_mc = 0
       sigma = 0

       do i = 1, n
         call random_number(p)
         x = 3.0d0 * p

         crude_mc = crude_mc + y1(x)
         sigma = sigma + (y1(x) * y1(x))

       end do

       crude_mc = (crude_mc/real(n))*3.0d0
       sigma = sigma/real(n)

       print *, "I = ", crude_mc


end program mc_integration

real *8 function y1(x) result (k)
    real *8 :: x
    k = exp(x)

end function y1
