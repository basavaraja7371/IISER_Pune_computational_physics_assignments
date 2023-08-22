program gauss_and_sin
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

     !print *, "Integration of the function f(x) = 1/(sqrt(2 \pi)) * exp[- (x^2)/2]"
     
     !print *, "Integration of the function f(x) = sin(x)" 
     
     print *, "Integration of the function f(x) = (cos(x))^2" 
     
     print *, ""    
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
         x = x + func(a+h*i)
     enddo

     print *, 'I = ', h*x

end program gauss_and_sin


real *16 function func(xk) result(k)

     implicit none
     real *16 :: xk
     real *16, parameter :: pi=2*asin(1.0)
      
     !k = exp(-(xk**2.0d0)/2.0d0)/sqrt(2.0d0*pi)

     !k = sin(xk)

     k = cos(xk)**2 

 end function func
