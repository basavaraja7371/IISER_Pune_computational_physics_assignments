program atoms
      implicit none

      !Parameters------------------------------------------------

      integer, parameter :: nop = 50
      real *8, parameter :: k=1.0d0
      
      !----------------------------------------------------------

      real *8 :: y0(nop), v0(nop)     ! initial values of y and v
      real *8 :: t0, tn               ! Start and end points of t
      
      real *8 :: fy0(nop), fy1(nop), fy2(nop), fy3(nop)
      real *8 :: fv0(nop), fv1(nop), fv2(nop), fv3(nop)

      real *8 :: y1(nop), y2(nop), y3(nop)
      real *8 :: v1(nop), v2(nop), v3(nop)

      integer :: i, j, n, l, r, ni        ! l-left, r-right
      real *8 :: h, hby2                  ! hby2 = h/2
      character(len=30) :: filename

      ! variables for jmol---------------------------------------
      
      real *8 :: theta(nop), rr=5.0d0
      real *8, parameter :: two_pi = 4.0d0*asin(1.0d0)

      ! Values of parameters -------------------------------------

      tn = 40.0d0
      t0 = 0.0d0
      h = 0.020d0
      hby2 = h*0.50d0
      n = int((tn-t0)/h)
      
      ! initial conditions----------------------------------------

      y0 = 0.0d0
      v0 = 0.0d0

      y0(1) = 0.80d0
      y0(26) = 0.80d0
      
      ! circle for jmol------------------------------------------

      do i=1, nop
      theta(i) = dfloat(i)*two_pi/dfloat(nop)
      enddo
      
      
      ! The main program------------------------------------------

      
      open(unit=16, file='file_at_n_0000.dat', action='write')
      write(16,*) '50'
      write(16,*) ''
      do i=1,nop
        write(16,*) 'O', rr*cos(theta(i)), rr*sin(theta(i)), y0(i)
      enddo
      close(16)
      
      !--------------------------------------------------------------

      do i=1, n
        
        do j=1,nop
            
            ! Neighbours and PBC
            
            l = j-1
            r = j+1
            if (j .eq. 1) l=nop
            if (j .eq. nop) r=1

            fy0(j) = v0(j)                         ! dx/dt=v
            fv0(j) = k*(y0(r) + y0(l) - 2.0d0*y0(j))  ! dv/dt
        
        end do

        do j=1,nop
            y1(j) = y0(j) + hby2 * fy0(j)
            v1(j) = v0(j) + hby2 * fv0(j)
        end do
        
        do j=1,nop
            
            ! Neighbours and PBC
            
            l = j-1
            r = j+1
            if (j .eq. 1) l=nop
            if (j .eq. nop) r=1

            fy1(j) = v1(j)                            ! dx/dt=v
            fv1(j) = k*(y1(r) + y1(l) - 2.0d0*y1(j))  ! dv/dt
        
        end do

        do j=1,nop
            y2(j) = y0(j) + hby2 * fy1(j)
            v2(j) = v0(j) + hby2 * fv1(j)
        end do

        do j=1,nop
            
            ! Neighbours and PBC
            
            l = j-1
            r = j+1
            if (j .eq. 1) l=nop
            if (j .eq. nop) r=1

            fy2(j) = v2(j)                            ! dx/dt=v
            fv2(j) = k*(y2(r) + y2(l) - 2.0d0*y2(j))  ! dv/dt
        
        end do

        do j=1,nop
            y3(j) = y0(j) + h * fy2(j)
            v3(j) = v0(j) + h * fv2(j)
        end do
       
        do j=1,nop
            
            ! Neighbours and PBC
            
            l = j-1
            r = j+1
            if (j .eq. 1) l=nop
            if (j .eq. nop) r=1

            fy3(j) = v3(j)                            ! dx/dt=v
            fv3(j) = k*(y3(r) + y3(l) - 2.0d0*y3(j))  ! dv/dt
        
        end do

        y0 = y0 + h*(fy0 + 2*fy1 + 2*fy2 + fy3)/6.0d0
        v0 = v0 + h*(fv0 + 2*fv1 + 2*fv2 + fv3)/6.0d0
        t0 = t0 + h

        

        if (mod(i,10) .eq. 0) then

             ni =10
             write(filename,'("file_at_n_",i4.4,".dat")') i
             open(newunit=ni, file=filename)
             write(ni, *) '50'
             write(ni, *) ""
             do j=1,nop
                 write(ni,*) 'O', rr*cos(theta(j)), rr*sin(theta(j)), y0(j)
             end do
            
             close(ni)
        end if

    end do
    print *,"The position of the first particle after 2000 iterations:", y0(1)

end program atoms






        



