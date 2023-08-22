program pde_neuman
      implicit none

      integer :: lx=34, ly=34, counter, cond
      real *8 :: T_old(1:34, 1:34), T(1:34, 1:34), dy, dx, limit, shift
      
      integer :: i, j, ii, jj
      real *8 :: A(34), B(34), C(34), D(34)


      A=-70.d0
      B=-40.d0
      C= 20.d0
      D=-10.d0
      
      T_old=0.d0

      dx=1.0d0
      dy=dx


      counter = 0
      limit = 0.00001d0

      ! Loop over iteration
      ! T=T_old
      print *, ""

      do 

          counter=counter+1
          cond=0

          ! updating boundaries, leaving the corners: Neuman

          do j=2, ly-1
              T(1,j)=0.25d0*(2.0d0*T_old(2,j)-2.0d0*dx*A(j)+T_old(1,j+1)+T_old(1,j-1))
              T(lx,j)=0.25d0*(2.0d0*T_old(lx-1,j)+2.0d0*dx*B(j)+T_old(lx,j+1)+T_old(lx,j-1))
          enddo

          do i=2, lx-1
              T(i,1) =0.25d0*(T_old(i+1,1) +T_old(i-1,1) +2.0d0*T_old(i,2)-2.0d0*dx*C(i))
              T(i,ly)=0.25d0*(T_old(i+1,ly)+T_old(i-1,ly)+2.0d0*T_old(i,ly-1)+2.0d0*dx*D(i))
          enddo

          ! corners---------------------------------------------------------

          T(1,1)  = 0.5d0*(T_old(1,2) - dx*C(1) + T_old(2,1) - dx*A(1))
          T(1,ly) = 0.5d0*(T_old(1,ly-1) + dx*D(1) + T_old(2,ly)   - dx*A(ly))
          T(lx,1) = 0.5d0*(T_old(lx-1,1) + dx*B(1) + T_old(lx,2)   - dx*C(lx))
          T(lx,ly)= 0.5d0*(T_old(lx-1,ly)+ dx*B(ly)+ T_old(lx,ly-1)+ dx*D(lx))

          ! Inside----------------------------------------------------------

          do j=2,ly-1
          do i=2,lx-1

          T(i,j) = 0.25 * (T_old(i+1,j) + T_old(i-1,j) + T_old(i,j+1) + T_old(i,j-1))

          enddo
          enddo

          !----------------------------------------------------------------

          do ii=1,lx
          do jj=1,ly

          if (abs(T_old(ii,jj)-T(ii,jj)) .gt. limit) cond=1

          enddo
          enddo

          if (cond .eq. 0) exit
          !if (counter .eq. 100000) exit
          shift=2000-T(1,1)
          T=T+shift
          T_old = T

    enddo

    
    print *, 'The number of iterations =', counter

    open(unit=17, file='T-lim00001_Neu.dat', action='write')

    do i=1,lx
    do j=1,ly

    write(17,*) i, j, T(i,j)

    enddo
    enddo
    print *, ""
    print *, "The value of T(10,10) is ",T(10,10)

end program pde_neuman

