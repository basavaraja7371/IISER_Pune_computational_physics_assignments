program pde_dirichlet
      implicit none

      integer :: lx=34, ly=34, counter, cond
      real  :: T_old(1:34, 1:34), T(1:34, 1:34), m, limit
      
      integer :: i, j, ii, jj

      ! Boundary conditions------------------------------------------
      
      ! For linear decrease, T as a function of x T = m*x
      ! Finding m in this case as (T(l)-T(1))/(xl-x1) = 3.3/33 = -0.1

      T_old=0
      m=0.10d0

      do i=1,lx
        T_old(i,1) = dfloat(ly)*m - dfloat(i)*m + 0.4
        T_old(i,ly) = dfloat(ly)*m - dfloat(i)*m + 0.4
      enddo
      

      do j=2,ly-1
        T_old(1,j) = 3.7d0
        T_old(lx,j) = 0.4d0
      enddo

      
      
      T = T_old
      !-----------------------------------------------------------------
      
      open(unit=16, file='T_old.dat', action='write')

      do i=1,lx
      do j=1,ly

      write(16,*) i, j, T_old(i,j)

      enddo
      enddo

      !print *, T_old

      !------------------------------------------------------------------
      
      counter = 0
      limit = 0.0001

      do
      counter = counter + 1
      cond = 0 

      do j=2,ly-1
      do i=2,lx-1

      T(i,j) = 0.25 * (T_old(i+1,j) + T_old(i-1,j) + T_old(i,j+1) + T_old(i,j-1))

      enddo
      enddo


      do ii=2,lx-1
      do jj=2,ly-1

      if (abs(T_old(ii,jj)-T(ii,jj)) .gt. limit) cond=1

      enddo
      enddo

      if (cond .eq. 0) exit

      T_old = T

      enddo

      !---------------------------------------------------------------------------------

      print *, 'The number of iterations =', counter

      open(unit=17, file='T-lim0001_test.dat', action='write')

      do i=1,lx,2
      do j=1,ly,2

      write(17,*) i, j, T(i,j)

      enddo
      enddo



end program pde_dirichlet


