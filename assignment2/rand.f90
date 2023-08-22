program rand
    implicit none
    real *8 :: r
    integer :: i
    
    do i=1,100
        
        call random_number(r)
        print *, r
    end do
end program rand
