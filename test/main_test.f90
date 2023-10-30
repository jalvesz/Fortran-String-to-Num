program main_test
    !
       use, intrinsic :: iso_fortran_env, only: dp => real64, i8 => int64
       use mod_st_to_dp
       use str2num_m
    !
       implicit none 
    !  
       integer,parameter :: n = 1000000 !! number of values 
    !  
       character(len=30),dimension(:),allocatable,target :: strs
       character(:),allocatable,target :: strs_seq !> Copy of integers but in a sequential manner
       real(dp),dimension(:),allocatable :: rval, rref 
    !
       integer :: i, i0 
       integer :: ierr
       real(dp) :: r
       integer(i8) :: start, finish, count_rate
       real(dp),parameter :: eps = epsilon(1.0_dp) 
    !
    !
    !  create a list of values to parse
    !
       allocate( strs(n), rval(n), rref(n) )
       allocate( character(30*n) :: strs_seq  )
    !  Initializing RNG: with a dummy i
       r = util_unirand (i)
    
       i0 = int(n*r) + 1 
    
       write(*,*) 'N =', n, ', i0 =', i0
    
       do i = 1, n
          rref(i) = util_unirand()
       enddo
    
       call system_clock(start, count_rate)
       do i = 1, n
          write(strs(i), '(E30.16)') rref(i)
          strs_seq( 30*(i-1)+1:30*i ) = strs(i)
       enddo
       call system_clock(finish)
       write(*,'(A30,1X,F7.4,1X,A)') 'Write: time consumed =', &
          (finish-start)/real(count_rate,dp), ' seconds'
    
       
       blk_io: block
          write(*,11) "BLOCK 1: formatted read toward string to double"
    !
          call system_clock(start, count_rate)
          do i = 1, n
             read(strs(i),fmt=*,iostat=ierr) rval(i)
          enddo
          call system_clock(finish)
          
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (serial)'
             
          call system_clock(start, count_rate)
          read(strs(:),fmt=*,iostat=ierr) rval(:)
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (array)'
    
          call show_err ( maxval(abs(rval-rref)), eps ) 
    !
       end block blk_io
    
    
       blk_c: block
          write(*,11) "BLOCK 2: C st_to_r8"
    !
          call system_clock(start, count_rate)
          do i = 1, n
             call c_st_to_r8 ( strs(i), rval(i) )
          enddo
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (serial)'
    
          call system_clock(start, count_rate)
          call c_st_to_r8 ( strs, rval )
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (array)'
    !
          call show_err ( maxval(abs(rval-rref)), eps ) 
    !
       end block blk_c
    
    
       blk_f: block
          write(*,11) "BLOCK 3: F st_to_r8"
    !
          call system_clock (start, count_rate)
          do i = 1, n
             call f_st_to_r8 ( strs(i), rval(i) )
          enddo
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (serial)'
    
          call system_clock(start, count_rate)
          call f_st_to_r8 ( strs, rval )
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (array)'
    !
          call show_err ( maxval(abs(rval-rref)), eps ) 
    !
       end block blk_f
    
    
       blk_4: block
          write(*,11) "BLOCK 4: F str2real "
    !
          call system_clock (start, count_rate)
          do i = 1, n
             rval(i) = str2real ( strs(i) )
          enddo
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (serial)'
    !
          call system_clock(start, count_rate)
          rval = str2real ( strs  )
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (array)'
    !
          call show_err ( maxval(abs(rval-rref)), eps ) 
       end block blk_4
       
       blk_5: block
          character(len=:), pointer :: pc
          write(*,11) "BLOCK 5: F str2real_p "
    !
          call system_clock (start, count_rate)
          do i = 1, n
            pc => strs(i)(1:)
            rval(i) =  str2real_p( pc )
          enddo
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (serial)'
        
          rval(:) = 0.d0
          call system_clock(start, count_rate)
          pc => strs_seq(1:)
          do i = 1, n
            rval(i) = str2real_p ( pc )
          enddo
          call system_clock(finish)
          write(*,'(A30,1X,F7.4,1X,A)') 'Read: time consumed =', &
             (finish-start)/real(count_rate,dp), ' seconds (stream)'
    !
          call show_err ( maxval(abs(rval-rref)), eps ) 
          pc => null()
       end block blk_5
    
    !
    10 format ( 'i =', i9, ', string(i) =', a, ', zval(i) =', e25.17  )
    11 format ( /, 80('-'), /, a )
    !
       deallocate ( strs, rval, rref , strs_seq ) 
    !
       contains 
    !======================================================================
    !
    !     This is the ran0 in Numerical Rrecipes in Fortran.
    !     To initialize, 
    !        call system_clock( idummy )
    !     Do not alter idummy between successive deviates.
    !
          function util_unirand ( idum )    result( ran0 )
    !
          implicit none 
          integer,parameter :: rk = kind(1.0D0), ik = 4
    !
    !     ARGUMENTS:
    !
          real(rk) :: ran0
          integer(ik),intent(in),optional :: idum 
    !
    !     local constants and variable
    !
          integer(ik) :: k
          integer(ik),save :: idummy 
          integer(ik),parameter :: &
             ia=16807, im=2147483647, iq=127773, ir=2836, mask=123459876
          real(rk),parameter :: am = 1.0_rk / im
    !
    !
          if ( present(idum) ) call system_clock( idummy )
    !
          idummy = ieor( idummy, mask )
          k      = idummy / iq
          idummy = ia*( idummy - k*iq ) - ir*k
          if ( idummy .lt. 0 ) idummy = idummy + im
          ran0 = am*idummy
          idummy = ieor( idummy, mask )
          return
          end function
    !
    !=====
          subroutine show_err ( er, ep ) 
          real(dp),intent(in) :: er, ep 
          real(dp) :: tmp 
    !
          tmp = ep - er
          write(*,10)  er, ep, tmp 
          if ( tmp .lt. 0.0_dp ) write(*,11) er/ep 
          return 
     10   format( 'A = Max |rval(:)-rref(:)| =', 1pe10.3,',', 3x, &
                  'B = epsilon(1.0d0) =', 1pe10.3,',', 3x, &
                  'B-A =', 1pe11.3 )   
     11   format( '*** WARNING: A > B, A/B =', 1pe11.3 )
          end subroutine 
    !
    !======================================================================
end program