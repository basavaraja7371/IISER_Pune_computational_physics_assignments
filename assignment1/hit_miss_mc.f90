program hit_miss
      implicit none

      ! Variables to generate random numbers

       integer, allocatable :: seed(:)
       integer:: dt_vals(8)
       integer :: seed_size, i
       real *8 :: r, s

       ! Other variables

       real *8 :: hit_miss_mc, p, x, y, f, ratio, q
       integer :: n, point 


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

       !-------------------------------------------------------------------------

       print *, "Number of iterations:"
       read *, n
       point = 0

       do i=1,n
         call random_number(p)
         x = p * 3.0d0
         print *, f(x)
         call random_number(q)
         y = q * exp(3.0d0)
         print *, y 
         if (y > f(x)) then
             point = point + 1
         end if
       end do
       print *, point
       ratio = real(point)/real(n)

       hit_miss_mc = 1.0 * exp(3.0d0) * ratio

       print *, "I = ", hit_miss_mc

end program hit_miss

real *8 function f(x)
    implicit none

    real *8 :: x

    f(x) = exp(x)

end function f


    





