program second_order
      implicit none
      
      real *8 :: t0, tn, x0, v0, h
      integer :: n, i 

      real *8 :: k1, k2, k3, k4, l1, l2, l3, l4
      real *8 :: f, g

      ! Inputs-------------------------------------------------------

      print *, ''

      print *, 'Initial position x_0:'
      read *, x0
      print *, ''

      print *, 'Initial velocity v_0:'
      read *, v0
      print *, ''

      print *, 'The length of the time interval dt:'
      read *, h
      print *, ''

      print *, 'The final value of time t_n:'
      read *, tn
      print *, ''
      
      !----------------------------------------------------------------

      open(unit=16, file='sec-order.dat', action='write')

      !----------------------------------------------------------------

      t0 = 0.0d0
      n=int((tn-t0)/h)

      write(16,*) t0, x0, v0

      do i=1,n
        
        k1 = h*f(t0, x0, v0)
        l1 = h*g(t0, x0, v0)

        k2 = h*f(t0+(0.50d0*h), x0+(k1*0.50d0), v0+(l1*0.50d0))
        l2 = h*g(t0+(0.50d0*h), x0+(k1*0.50d0), v0+(l1*0.50d0))

        k3 = h*f(t0+(0.50d0*h), x0+(k2*0.50d0), v0+(l2*0.50d0))
        l3 = h*g(t0+(0.50d0*h), x0+(k2*0.50d0), v0+(l2*0.50d0))

        k4 = h*f(t0+h, x0+k3, v0+l3)
        l4 = h*g(t0+h, x0+k3, v0+l3)

        t0 = t0 + h
        x0 = x0 + (k1 + 2*k2 + 2*k3 + k4)/6.0d0
        v0 = v0 + (l1 + 2*l2 + 2*l3 + l4)/6.0d0

        write(16,*) t0, x0, v0

    end do

    print *, '           t_n                    x_n                     v_n             '
    print *, t0, x0, v0 

end program second_order






! function f(t,x,v)----------------------------------------------------

real *8 function f(t, x, v) result(z)
      implicit none
      real *8 :: t, x, v

      z = v

end function f

! function g(t, x, v)--------------------------------------------------

real *8 function g(t,x,v) result(z)
      implicit none
      real *8 :: t, x, v 

      z = -sin(x)

end function g















