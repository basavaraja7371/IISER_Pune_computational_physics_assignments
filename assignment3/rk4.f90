program euler
      implicit none

      integer :: i, n
      real *8 :: x0, y0, f0, x1, y1, f1, x2, y2, f2, x3, y3, f3 
      real *8 :: xn, h, f 

! Inputs----------------------------------------
      
      print *, 'Give'

      print *, 'x_0:'
      read *, x0

      print *, 'y_0'
      read *, y0
      
      print *, 'End point:'
      read *, xn
      
      print *, 'The interval length h:'
      read *, h

! Opening files ---------------------------------------------------

      open(unit=19, file='rk4.dat', action='write')

! Main program-----------------------------------------------------
  
      n = int((xn-x0)/h)
      write(19,*) x0, y0
      do i=1,n
        
        ! RK4  method
        f0 = f(x0, y0)
        
        x1 = x0 + 0.50d0*h
        y1 = y0 + 0.50*h*f0
        f1 = f(x1, y1)

        x2 = x0 + 0.50d0*h
        y2 = y0 + 0.50d0*h*f1
        f2 = f(x2, y2)

        x3 = x0 + h
        y3 = y0 + h*f2
        f3 = f(x3, y3)

        y0 = y0 + h*(f0 + 2*f1 + 2*f2 + f3)/6.0d0
        x0 = x0 + h

        write(19,*) x0, y0


     end do
 
     print *, ""
     print *, '         x                        y_RK4           '
     print *, x0, y0 
     
     print *, ""
     print *, 'Error in y(1.55)'
     print *, tan(1.550d0)-y0




end program euler

real *8 function f(xk,yk) result(k)
 
     implicit none
     real *8 :: xk, yk
 
     k =1.0d0 + yk**2.0d0
 
 end function f
