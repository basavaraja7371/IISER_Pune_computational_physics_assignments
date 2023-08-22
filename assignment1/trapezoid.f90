program trapezoid
	implicit none
    
    real *16 :: func
    real *16 :: a, b, fa, fb, h, x
    integer :: n, i, j

    ! func is the function defined
    ! a is lower limit
    ! b is upper limit
    ! fa and fb is value of function at a and b
    ! h is bin size
    ! n is number of iterartions
    
    real *16 :: hi, pi, xi, q=1.0d0, error  ! For error calculations
    integer :: ni

    
    print *, 'Enter the number of iterartions'
    read *, n

    print *, 'Enter the lower limit'
    read *, a

    print *, 'Enter the upper limit'
    read *, b
    
    h = (b-a)/dfloat(n)

    fa = func(a)
    fb = func(b)

    x = (fa + fb)/2.0

    ! (Look at trapezoid rule.) I have defined x as the first two
    ! terms in the formula which contains fa and fb. (I'll multiply h later)

    do i=1,n-1
    	x = x + func(a+(h*i))
    enddo
    
    print *, 'I = ', h*x

    ! Calculating the error to see n^-2 relationship
    
    open(unit=16, file='error.dat', action='write')

    pi=2.0d0*asin(q) ! Where q is in real*16
    ni = 1
 1  ni = ni*100      ! In the goto loop hi is multiplied by 0.5
                     ! Which is same as ni multilied by 2 everytime.

    hi = (b-a)/dfloat(ni)        

    xi = (fa + fb)/2.0d0    ! Same procedure as above

    do i=1,ni-1
         xi = xi + func(a+(hi*i))
    enddo
    
       error = abs(pi-(hi*xi))       ! error for 4/(1+x^2)
    
    ! Writing log(ni) and log(error)

    write(16,*) log10(dfloat(ni)) , log10(error)  
    
    if (ni<10000001) goto 1
    
end program trapezoid



! Defining Function
real *16 function func(xk) result(k)

    implicit none
    real *16 :: xk

    k = 4.0d0/(1.0d0 + xk**2.0d0)

end function func
