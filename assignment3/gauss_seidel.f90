program gauss_seidel
      implicit none

      ! Variables----------------------------------------------------------------------

      real *8, parameter :: xi=0.0d0, xf=0.9d0   ! interaval values
      
      ! Value of dx and number of points n---------------------------------------------

      real *8, parameter :: dx = 0.001d0
      integer, parameter :: n = int((xf-xi)/dx)

      ! Arrays of x, y and y_old values------------------------------------------------

      real *8 :: x(n), y(n), yo(n)
      
      ! Condition, count, limit and other variables for loops--------------------------

      integer :: cond, l, i, j
      real *8 :: limit

      ! Parameters and constants-------------------------------------------------------

      real *8, parameter :: c=1.0d0/(2.0d0-10.0d0*dx*dx)
      real *8, parameter :: d1=(1.0d0-2.50d0*dx), d2=(1.0d0+2.50d0*dx), d3=10.0d0*dx*dx

      !---------------------------------------------------------------------------------
      ! Initialization and Boundary condition

      l = 0
      limit = 0.000001
      cond = 0

      x(1) = xi
      x(n) = xf
      y(1) = 0.0d0
      y(n) = 50.0d0
      
      !---------------------------------------------------------------------------------
 
      open(unit=16, file='dx_001_lim_000001.dat', action='write')

      do i=2,n-1

        x(i) = x(i-1) + dx

        y(i) = (y(n)-y(1))*x(i)/(x(n)-x(1))

      enddo

      do 
        
        l=l+1

        if(cond .eq. 1) exit
        yo=y

        do j=2,n-1
            y(j) = c*(d1*yo(j+1) + d2*y(j-1) + d3*x(j))
        enddo

        cond=1
        do j=2,n-1
            if(abs(yo(j)-y(j)) .ge. limit) cond=0
        enddo

      enddo
      
      print *, ""
      print *, 'Number of iteration required for convergence:', l

      do i=1,n
        write(16,*) x(i), y(i)
      enddo

      close(16)

      print *, " "
      print *, 'The values of y at x=0.8 is ', y(800)

end program gauss_seidel
