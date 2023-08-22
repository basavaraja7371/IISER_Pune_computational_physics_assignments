program euler
      implicit none

      integer :: i, n
      real *8 :: x0, y0, h, f, k1, x, y, xn

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

      open(unit=18, file='imp_euler.dat', action='write')

! Main program-----------------------------------------------------
  
      n = floor((xn-x0)/h)
      write(18,*) x0, y0
      do i=1,n
        
        ! Improved Euler method

        x = x0 + h
        k1 = h*f(x0, y0)
        y = y0 + 0.50d0*(k1+h*f(x, y0+k1))
      
        x0 = x  
        y0 = y
        write(18, *) x , y

     end do
 
     print *, ""
     print *, '         x                       y_IE           '
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
