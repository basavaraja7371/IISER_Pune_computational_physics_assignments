
! Program to calculate 6D integral

program gaussian_rand_nos
    implicit none

    real *8 :: x(6), gauss, f
    real *8 :: sd, y, var, st_dev
    integer :: i, n, j
    real *8, parameter :: pi = 2*asin(1.0d0)
    
    sd = 1.0d0/sqrt(2.0d0)
    
    print *, "Enter number of iterations"
    read *, n
    
    open(unit=16, file="imp_samp.dat", action='write')


9   y=0.0d0
    var = 0.0d0

    do i=1,n
    
        do j=1,6
            x(j) = gauss()*sd
        end do
    
        y = y+f(x)
        var = var + f(x)*f(x)
    
    end do

    y = y/dfloat(n)
    var = (var/dfloat(n)) - (y*y)
    st_dev = sqrt(var)
   
    write(16,*) n,  (pi**3) * y, (pi**3) * st_dev/sqrt(real(n))
    
    n=n*10
    if (n < 100000001) goto 9




end program gaussian_rand_nos

!----------------------------------------------------------------------------

! Defining function which is calculated after multiplying by distn. function.

real *8 function f(x)
    implicit none
    real *8 :: x(6), a, xy

    a = 0.50d0

    xy = (x(1)-x(4))**2 + (x(2)-x(5))**2 +(x(3)-x(6))**2
    f = exp(-a*xy)

end function f

!------------------------------------------------------------------------

! Defining a function which returns a gaussian distributed random number.
! Box-Muller method

real *8 function gauss()
    implicit none
    real *8 :: x, y, s, p, q
8   call random_number(x)
    call random_number(y)

    p = 2.0d0*x -1.0d0
    q = 2.0d0*y -1.0d0
    s = p*p + q*q
    if (s .ge. 1.0d0 .or. s .eq. 0.0d0) goto 8

    gauss = sqrt(-2*log(s))*(p/sqrt(s))

end function gauss 


    
