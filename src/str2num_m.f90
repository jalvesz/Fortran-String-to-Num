module str2num_m
    use, intrinsic :: ieee_arithmetic, only: IEEE_Value, IEEE_QUIET_NAN
    implicit none
    private
    public :: str2real, str2real_p, str2real_ch
    
    integer, parameter :: wp    = kind(1.0d0)
    integer, parameter :: ikind = selected_int_kind(2)
    integer(kind=ikind), parameter :: digit_0    = ichar('0')
    integer(kind=ikind), parameter :: period     = ichar('.') - digit_0
    integer(kind=ikind), parameter :: minus_sign = ichar('-') - digit_0
    integer(kind=ikind), parameter :: plus_sign = ichar('+') - digit_0
    integer(kind=ikind), parameter :: LF = 10, CR = 13, WS = 32
    
    real(wp), parameter :: whole_number_base(16) =                    &
            [1d15,  1d14,  1d13,  1d12,  1d11,  1d10,  1d9,   1d8,   &
             1d7,   1d6,   1d5,   1d4,   1d3,   1d2,   1d1,   1d0]
    real(wp), parameter :: fractional_base(24)   =                    &
            [1d-1,  1d-2,  1d-3,  1d-4,  1d-5,  1d-6,  1d-7,  1d-8,  &
             1d-9,  1d-10, 1d-11, 1d-12, 1d-13, 1d-14, 1d-15, 1d-16, &
             1d-17, 1d-18, 1d-19, 1d-20, 1d-21, 1d-22, 1d-23, 1d-24]
    real(wp), parameter :: period_skip = 0d0
    real(wp), parameter :: base(41) = &
            [whole_number_base, period_skip, fractional_base]
    real(wp), parameter :: expbase(40) = [whole_number_base, fractional_base]
    integer, parameter  :: ibase(10) = [1e9,1e8,1e7,1e6,1e5,1e4,1e3,1e2,1e1,1e0]
    integer(kind=ikind),parameter :: le=iachar('e')-digit_0
    integer(kind=ikind),parameter :: BE=iachar('E')-digit_0
    integer(kind=ikind),parameter :: oWS = WS-digit_0
    integer(kind=ikind),parameter :: oLF = LF-digit_0
    integer(kind=ikind),parameter :: oCR = CR-digit_0
    contains
    
    elemental function str2real_ch(s) result(r)
        !< Fonction by @Carltoffel and further modified by @hkvzjal
        character(*), intent(in) :: s
        real(wp) :: r
        integer, parameter :: N = 32
        character(N) :: factors_char
        integer(kind=ikind) :: factors(N)
        integer      :: period_loc
        integer      :: exponent_loc
        intrinsic    :: findloc, scan
        integer      :: findloc, scan
        integer(1)   :: sig_coef, sig_exp
        integer      :: i_exponent, val
        logical      :: not_found
        
        equivalence(factors, factors_char)
        
        factors_char = s
        factors = factors - digit_0
        
        exponent_loc = scan(s, 'eE', back=.true.)
        if(exponent_loc == 0) exponent_loc = len(s) + 1
        period_loc   = findloc(factors, period, 1)
        if(period_loc   == 0) period_loc   = exponent_loc
        
        sig_exp = 1
        if(factors(exponent_loc+1) == minus_sign) sig_exp = -1
        
        sig_coef = 1
        if(factors(1)== minus_sign) sig_coef = -1
        
        val = str2int( s(1:period_loc-1) )
        r = sum( factors(period_loc+1:exponent_loc-1)*fractional_base(1:exponent_loc-period_loc-1) )
        r = sig_coef*(r+val)
        
        i_exponent = str2int( s(exponent_loc+1:len(s)) )
        if(i_exponent.ne.0) r = r * expbase(16-sig_exp*i_exponent)
    end function
    
    elemental function str2int(s) result(int)
        character(*), intent(in) :: s
        integer :: int
        integer(1) :: i, val
        
        int = 0
        do i = 1, len(s)
            val = iachar(s(i:i))-digit_0
            if( val >= 0 .and. val <= 9 ) int = int*10 + val
        end do
        
    end function

    function str2int_p(s) result(int)
        character(len=:), pointer :: s
        integer :: int
        integer(1) :: p !< position within the number
        integer(1) :: val
        !----------------------------------------------
        ! Find first non white space
        p = mvs2nwsp(s)
        
        int = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                int = int*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        
        p = min( p , len(s) )
        s => s(p:)
        
    end function
    
    elemental function str2real(s) result(r)
        character(*), intent(in) :: s !< input string
        real(wp) :: r !< Output real value
        integer(1) :: p !< position within the number
        integer(1)  :: sign, sige !< sign of integer number and exponential
        integer(1)  :: pP, i_exp, val 
        integer     :: int_4 !< integer to capture whole number part
        integer(wp) :: int_wp !< logn integer to capture fractional part
        !----------------------------------------------
        ! Find first non white space
        p = mvs2nwsp(s)
        !----------------------------------------------
        ! Verify leading negative
        sign = 1
        if( iachar(s(p:p)) == minus_sign+digit_0 ) then
            sign = -1 ; p = p + 1
        end if
        if( iachar(s(p:p)) == iachar('I') ) then
            r = sign*huge(1.0); return
        end if
        if( iachar(s(p:p)) == iachar('N') ) then
            r = IEEE_VALUE(1.d0, IEEE_QUIET_NAN); return
        end if
        !----------------------------------------------
        ! read leading whole number
        int_4 = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                int_4 = int_4*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        if( p>=len(s)) then
            r = sign*int_4; return
        end if
        !----------------------------------------------
        ! Verify period
        pP = p
        if( iachar(s(p:p)) == period+digit_0 ) then
            pP = p ; p = p + 1
        end if
        !----------------------------------------------
        ! read fractional number
        int_wp = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                int_wp = int_wp*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        r = 0
        if(p>pP+1) r = int_wp*fractional_base(p-pP-1)
        r = sign*(r+int_4)
        !----------------------------------------------
        ! Get exponential
        if( p>=len(s)) then
            return
        else if( iachar(s(p:p)) == le+digit_0 .or. iachar(s(p:p)) == BE+digit_0 ) then
            p = p + 1
        end if
        
        sige = 1
        if( iachar(s(p:p)) == minus_sign+digit_0 ) then
            sige = -1
            p = p + 1
        else if( iachar(s(p:p)) == plus_sign+digit_0 ) then
            p = p + 1
        end if
        
        i_exp = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                i_exp = i_exp*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        
        if(i_exp.ne.0) r = r * expbase(16-sige*i_exp)
    end function
    
    function str2real_p(s) result(r)
        character(len=:), pointer :: s
        real(wp) :: r
        integer(1) :: p !< position within the number
        integer(1)  :: sign, sige !< sing of integer number and exponential
        integer(1)  :: pP, i_exp, val
        integer     :: int
        integer(wp)  :: int_wp
        !----------------------------------------------
        ! Find first non white space
        p = mvs2nwsp(s)
        !----------------------------------------------
        ! Verify leading negative
        sign = 1
        if( iachar(s(p:p)) == minus_sign+digit_0 ) then
            sign = -1 ; p = p + 1
        end if
        if( iachar(s(p:p)) == iachar('I') ) then
            r = sign*huge(1.0); goto 10
        end if
        if( iachar(s(p:p)) == iachar('N') ) then
            r = IEEE_VALUE(1.d0, IEEE_QUIET_NAN); goto 10
        end if
        !----------------------------------------------
        ! read leading whole number
        int = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                int = int*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        if( p>=len(s)) then
            r = sign*int; goto 10
        end if
        !----------------------------------------------
        ! Verify period
        pP = p
        if( iachar(s(p:p)) == period+digit_0 ) then
            pP = p ; p = p + 1
        end if
        !----------------------------------------------
        ! read fractional number
        int_wp = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                int_wp = int_wp*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        r = 0
        if(p>pP+1) r = int_wp*fractional_base(p-pP-1)
        r = sign*(r+int)
        !----------------------------------------------
        ! Get exponential
        if( p>=len(s)) then
            goto 10
        else if( iachar(s(p:p)) == le+digit_0 .or. iachar(s(p:p)) == BE+digit_0 ) then
            p = p + 1
        end if
        
        sige = 1
        if( iachar(s(p:p)) == minus_sign+digit_0 ) then
            sige = -1 
            p = p + 1
        elseif( iachar(s(p:p)) == plus_sign+digit_0 )then
            p = p + 1
        end if
        
    10  i_exp = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                i_exp = i_exp*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        
        if(i_exp.ne.0) r = r * expbase(16-sige*i_exp)
        p = min( p , len(s) )
        s => s(p:)
    end function
    
    elemental function mvs2nwsp(s) result(p)
        !< get position of the next non white space character
        character(*),intent(in) :: s !< character chain
        integer(1) :: p !< position
        !----------------------------------------------
        p = 1
        do while( p<len(s) .and. (iachar(s(p:p))==WS.or.iachar(s(p:p))==LF.or.iachar(s(p:p))==CR) ) 
            p = p + 1
        end do
    end function
    
    elemental function mvs2wsp(s) result(p)
        !< get position of the next white space character
        character(*),intent(in) :: s !< character chain
        integer(1) :: p !< position
        !----------------------------------------------
        p = 1
        do while( p<len(s) .and. .not.(iachar(s(p:p))==WS.or.iachar(s(p:p))==LF.or.iachar(s(p:p))==CR) ) 
            p = p + 1
        end do
    end function
    
end module str2num_m

module mod_st_to_dp
    !
       use, intrinsic :: iso_c_binding,   only: c_double, c_char, c_ptr, &
                                                c_null_ptr, c_long
    ! 
       use, intrinsic :: iso_fortran_env, only: dp => real64, i8 => int64
    !
       implicit none 
    !
       private
    ! 
       public :: f_st_to_r8
       public :: c_st_to_r8
    !
    !
       interface
          pure & 
          function c_strtod ( str, endptr ) result(d) bind(C, name="strtod")
             import :: c_char, c_ptr, c_double
             character(kind=c_char,len=1), intent(in) :: str(*)
             type(c_ptr), intent(in) :: endptr
             real(c_double) :: d 
          end function 
       end interface
    !
    !
    !  integer, parameter :: dp = selected_real_kind( p=12 )
    !  FP constants
       real(dp), parameter :: ten = 10.0_dp
    !  Integer constants
       integer, parameter :: ascii_negative = 45
       integer, parameter :: ascii_period = 46
       integer, parameter :: ascii_0 = 48
       integer, parameter :: ascii_9 = 57
       integer(i8), parameter :: ten_int = 10_i8
    !
       type(c_ptr),parameter :: endptr = c_null_ptr
    !
    !------------------------------------------------------------------------------
       contains
    !------------------------------------------------------------------------------
       elemental   &
       subroutine  c_st_to_r8 ( str, r )
          character(len=*), intent(in) :: str
          real(dp), intent(out) :: r
    !
          r = c_strtod ( str, endptr )
          return 
       end subroutine 
    !------------------------------------------------------------------------------
    !
       elemental   &
       subroutine  f_st_to_r8 ( str, r )
          character(len=*), intent(in) :: str
          real(dp), intent(out) :: r
    !     local variables
          integer(i8) :: n, n_exp
          integer :: lens, pos_exp, expnt
    !
          r = 0.0_dp
          n_exp = 0
          lens = len_trim( str )
          pos_exp = index( str, "E", back=.true. )     ! <-- find the capital 'E'
          if ( pos_exp == 0 ) then
             pos_exp = index( str, "e", back=.true. )
          endif
          if ( pos_exp > 0 ) then
             call str2dec( str(pos_exp+1:lens), n_exp ) 
          else
             pos_exp = lens + 1
          endif
          call str2dec( str(1:pos_exp-1), n, expnt )
          r = real( n, kind=dp ) * ( ten**(n_exp + expnt) ) 
          return
       end subroutine
    !
       elemental   &
       subroutine  str2dec( s, n, expnt )
          character(len=*), intent(in)   :: s
          integer(i8), intent(out)       :: n 
          integer, intent(out), optional :: expnt
          integer :: ipos, ic, pos_period, iexponent
    !
          n = 0_i8
          pos_period = 0
          iexponent = -1
          if ( present(expnt) ) iexponent = 0 
          do ipos = len(s), 1, -1
             ic = ichar( s(ipos:ipos) )
             if ( present(expnt) ) then 
                if ( ic == ascii_period ) then
                   pos_period = ipos
                   cycle
                endif
             endif 
             if ( ic == ascii_negative ) then
                n = -n
                exit
             endif 
             if ( (ic < ascii_0) .or. (ic > ascii_9) ) exit
             iexponent = iexponent + 1 
             n = n + (ten_int**iexponent)*(ic - 48)
          enddo
          if ( present(expnt) ) expnt = -len(s) + pos_period - 1
          return
       end subroutine 
    
end module