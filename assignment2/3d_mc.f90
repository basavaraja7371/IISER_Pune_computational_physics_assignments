program monte_carlo_3d
      implicit none
      
      ! Variavle Declaration------------------------------------------------
      ! Variables for random number
      
      integer :: idum=-1283960977
      real *8 :: r, ran1

      !------------------------------------------------
      ! Variables for Monto-Carlo initialization

      integer, allocatable :: spin(:,:,:)
      integer :: i, j, k, a, b, c, u, v, w, L, M, N 
      real *8 ::  E, J_is=1, T
      
      !------------------------------------------------
      ! Variables for Monte-Carlo steps (Equilibriation)

      integer :: nn, mm, pp, niter, times
      real *8 :: Ei, Ef, prob, dE  
      
      !--------------------------------------------------------
      ! Variables to calculate Thermodynamic averages

      integer :: absM_2, absM, N_eq, T_val, N_stat, No_of_samps
      real *8 :: cv, chi, En_av, En_2_av, Mn_av, Mn_2_av, Mn_4_av, ul, uu
      real *8 :: M_av, E_av, M_av_2, E_av_2, var_E, var_M, abs_M_av
      
      !------------------------------------------------
      ! Opening file

      ! open(unit=17, file='T10_L10_1Lmcs.dat', action='write')
      open(unit=18, file='L7_dt_0-02_1L.dat', action='write')
      
      !------------------------------------------------------------------
      ! Reading inputs

      print *, "Give the length of the array:"
      read *, L

      print *, "Give the number of Monte-Carlo steps:"
      read *, niter
      
      !----------------------------------------------

      N_eq = 10000     ! Discarded values. Before eqm
      N_stat = 50         ! interval of values skipped
     
      No_of_samps = (niter-N_eq)/N_stat  ! Number values sampled
      
      M= 0
      E = 0.0d0
      
      !-----------------------------------------------
      ! Initializing the lattice

      allocate(spin(L,L,L))
      N = L*L*L

      do i=1,L
        do j=1,L
          do k=1,L     
            
            ! Setting random spin values

             r=ran1(idum)
             
             if (r .le. 0.50d0) then
                 spin(k,j,i) = 1
             else
                 spin(k,j,i) = -1
             end if
         
            ! Setting spin value to 1 or -1

             !spin(i,j,k) = 1
            ! spin(k,j,i) =-1

          end do
        end do
      end do
      
      !--------------------------------------------------------------------------------------
      ! Calculation of energy and magnetization of initial state 

      do i=1,L
        do j=1,L
         do k=1,L
            
            ! Neighbours

            a=i-1
            b=j-1
            c=k-1
            u=i+1
            v=j+1
            w=k+1

            ! Periodic Boundary conditions

            if (i==1) a=L
            if (j==1) b=L
            if (k==1) c=L
            if (i==L) u=1
            if (j==L) v=1
            if (k==L) w=1

            ! Magnetization and Energy calculation for this micro state

            M = M + spin(i,j,k)
            E = E - J_is * float((spin(i,j,k))*&
                (spin(a,j,k)+spin(i,b,k)+spin(i,j,c)+spin(u,j,k)+spin(i,v,k)+spin(i,j,w)))
         
            end do
        end do
    end do
    print *, ""
    E = E * 0.50d0
    print *, ""
    print *, "Total magnetization:", M
    print *, "Moment per spin    :", M/real(N)
    print *, ""
    print *, "Total energy   :", E
    print *, "Energy per spin:", E/real(N)
    print *, ""
    
    !---------------------------------------------------

    ! Evolving states for equilibrium
    
    do T_val=470,390,-2
        
        T = T_val/real(100)

        !M_av = 0.0d0
        !abs_M_av = 0.0d0
        !M_av_2 = 0.0d0
        !E_av = 0.0d0
        !E_av_2 = 0.0d0
        !var_E = 0.0d0
        !var_M = 0.0d0
        cv = 0.0d0
        Mn_av = 0.0d0
        Mn_2_av = 0.0d0
        En_av = 0.0d0
        En_2_av = 0.0d0
        Mn_4_av =0.0d0
        ul = 0.0d0
    do times=1, niter

          do mm=1,L
                do nn=1,L
                      do pp=1,L

                            ! Choose a random Lattice site
                            r=ran1(idum) 
                            i = int(r*float(L)) + 1

                            r=ran1(idum)
                            j = int(r*float(L)) + 1
 
                            r=ran1(idum)
                            k = int(r*float(L)) + 1

                            !Neighbours of spin(k,j,i)

                            a=i-1
                            b=j-1
                            c=k-1
                            u=i+1
                            v=j+1
                            w=k+1

                            ! PBC

                            if (i==1) a=L
                            if (j==1) b=L
                            if (k==1) c=L
                            if (i==L) u=1
                            if (j==L) v=1
                            if (k==L) w=1
            
                            Ei =  - J_is * float((spin(i,j,k)*(spin(a,j,k)+spin(i,b,k)+&
                                spin(i,j,c)+spin(u,j,k)+spin(i,v,k)+spin(i,j,w))))

                            !Flip
                            spin(i,j,k) = -spin(i,j,k)

                            Ef =  - J_is * float((spin(i,j,k)*(spin(a,j,k)+spin(i,b,k)+&
                                spin(i,j,c)+spin(u,j,k)+spin(i,v,k)+spin(i,j,w))))
                            dE = Ef-Ei

                            if(dE .le. 0) then
                                E = E + dE
                                M = M + (2.0d0 * float(spin(i,j,k)))
                            else
                                ! Accepting with probability    exp(-dE/kT)
                                
                                prob = exp(-dE/real(T))
                                
                                ! Generate random number, check if less than
                                ! exp(-de/kT), if true accept o.w. reject

                                r=ran1(idum)
                                !call random_number(r)
                                if (r .lt. prob) then
                                    E = E + dE
                                    M = M + (2.0d0 * float(spin(i,j,k)))
                                else
                                    spin(i,j,k) = -spin(i,j,k)
                                end if
                            end if
                        end do
                    end do
              end do
              if (times .gt. N_eq) then
                  
                  if (mod(times,N_stat) .eq. 0) then
                      
                      !----------------------------------------------------
                      !Averages per spin
                    
                      !E_av = E_av + (E/float(N))
                      !abs_M_av = abs_M_av + (abs(M)/float(N))
                      !M_av =M_av + M/float(N) 
                      
                      !E_av_2 = E_av_2 + (E/float(N))*(E/float(N))
                      !M_av_2 = M_av_2 + (abs(M)/float(N))*(abs(M)/float(N))
                      
                      !---------------------------------------------------
                      !Averages for whole lattice

                      En_av = En_av + E
                      En_2_av = En_2_av + (E*E)
                      
                      Mn_av = Mn_av + abs(M)
                      Mn_2_av = Mn_2_av + M**2.0d0
                                            
                      Mn_4_av = Mn_4_av + M**4.0d0
                      
                      !-----------------------------------------------------

                  end if
              end if
          !    write(17,*) times, M/float(N), E/float(N)
        end do
        
        !----------------------------------------------------------------------------
        ! dividing by number of samples choosen to get the average

        !E_av_2 = E_av_2/(float(No_of_samps));         E_av = E_av/(float(No_of_samps))
        !abs_M_av = abs_M_av/(float(No_of_samps));     M_av_2 = M_av_2/(float(No_of_samps))    
        !M_av = M_av/(float(No_of_samps));             
        
        En_av = En_av/(float(No_of_samps));           En_2_av = En_2_av/(float(No_of_samps))
        Mn_av = Mn_av/(float(No_of_samps));           Mn_2_av = Mn_2_av/(float(No_of_samps))
        Mn_4_av = Mn_4_av/(float(No_of_samps))
        
        !----------------------------------------------------------------------------
        !var_E = E_av_2 - (E_av*E_av)
        !var_M = M_av_2-(M_av*M_av)
        
        cv = (En_2_av - En_av**2)/(T**2)
        chi= (Mn_2_av - Mn_av**2)/(T)
        
        uu=  Mn_4_av/(3.0d0*Mn_2_av**2) 

        ul = 1.0d0 - uu
        
        !-------------------------------------------------------------------------------

        write(18,*) T, cv, chi, ul 
        
        !------------------------------------------------------------------------------
        
        !print *, ""
        !print *, "The average <E/N>: ", E_av/(dfloat(niter)-N_eq)
        !print *, "The average <M/N>: ", M_av/(dfloat(niter)-N_eq)
        !print *, ""
        !print *, "The variance in energy:       ", varE 
        !print *, "The variance in magnetization:", varM 
        
        !------------------------------------------------------------------------------
    end do 

end program monte_carlo_3d


!ran1 function
!--------------------------------------------------------------------------------
FUNCTION ran1(IDUM)
 
    implicit none

    !  RAN1 returns a unifom random deviate on the interval [0,1]

    INTEGER :: IDUM
    REAL*8 :: RAN2,ran1
    integer,parameter :: IM1=2147483563,IM2=2147483399
    integer,parameter :: IMM1=IM1-1,                            &
        IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791, &
        NTAB=32
    integer,parameter :: NDIV=1+IMM1/NTAB  
    real*8,parameter :: EPS=1.2e-7,RNMX=1.-EPS,AM=1./IM1
    INTEGER :: IDUM2,J,K,IV(NTAB),IY
    DATA IDUM2/123456789/, iv/NTAB*0/, iy/0/
         IF (IDUM.LE.0) THEN
           IDUM=MAX(-IDUM,1)
           IDUM2=IDUM
           DO  J=NTAB+8,1,-1
              K=IDUM/IQ1
              IDUM=IA1*(IDUM-K*IQ1)-K*IR1
              IF (IDUM.LT.0) IDUM=IDUM+IM1
              IF (J.LE.NTAB) IV(J)=IDUM
           end do
           IY=IV(1)
         ENDIF
         K=IDUM/IQ1
         IDUM=IA1*(IDUM-K*IQ1)-K*IR1
         IF (IDUM.LT.0) IDUM=IDUM+IM1
         K=IDUM2/IQ2
         IDUM2=IA2*(IDUM2-K*IQ2)-K*IR2
         IF (IDUM2.LT.0) IDUM2=IDUM2+IM2
         J=1+IY/NDIV
         IY=IV(J)-IDUM2
         IV(J)=IDUM
         IF(IY.LT.1)IY=IY+IMM1
         RAN1=MIN(AM*IY,RNMX)

END function ran1
