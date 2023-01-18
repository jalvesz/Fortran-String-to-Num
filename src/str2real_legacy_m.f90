! Legacy module containing some of the implemantations discussed in the fortran forum
module str2real_m
        implicit none
        private
        public :: str2real, str2real_c
        
        integer, parameter :: wp    = kind(1.0d0)
        integer, parameter :: ikind = selected_int_kind(2)
        
        integer(kind=ikind), parameter :: zero       = 0_ikind
        integer(kind=ikind), parameter :: one        = 1_ikind
        integer(kind=ikind), parameter :: nine       = 9_ikind
        integer(kind=ikind), parameter :: digit_0    = ichar('0',kind=ikind)
        integer(kind=ikind), parameter :: period     = ichar('.',kind=ikind) - digit_0
        integer(kind=ikind), parameter :: minus_sign = ichar('-',kind=ikind) - digit_0
        
        real(wp), parameter :: whole_number_base(16) =                   &
                [1d15,  1d14,  1d13,  1d12,  1d11,  1d10,  1d9,   1d8,   &
                 1d7,   1d6,   1d5,   1d4,   1d3,   1d2,   1d1,   1d0]
        real(wp), parameter :: fractional_base(24)   =                   &
                [1d-1,  1d-2,  1d-3,  1d-4,  1d-5,  1d-6,  1d-7,  1d-8,  &
                 1d-9,  1d-10, 1d-11, 1d-12, 1d-13, 1d-14, 1d-15, 1d-16, &
                 1d-17, 1d-18, 1d-19, 1d-20, 1d-21, 1d-22, 1d-23, 1d-24]
        real(wp), parameter :: period_skip = 0d0
        real(wp), parameter :: base(41)    = [whole_number_base, period_skip, fractional_base]
        real(wp), parameter :: expbase(40) = [whole_number_base, fractional_base]
        ! Integer digits mask
        !integer, private :: i
        !integer(kind=ikind), parameter :: is_digit(-128:127) = &
        !        [(merge(one,zero,i>=0 .and. i<=9),i=-128,127)] !< ifort 19 error #6211: A symbol must be a defined parameter in this context.   [I]
        
        contains
        
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
        
        elemental function str2real(s) result(r)
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
        
        function str2real_c(s) result(r)
            !< Original fonction by @Carltoffel 
            character(*), intent(in) :: s
            real(wp) :: r
        
            real(wp) :: r_coefficient
            real(wp) :: r_exponent
        
            integer(kind=ikind), parameter :: N = 32_ikind
            character(N) :: factors_char
            integer(kind=ikind)    :: factors(N)
            integer(kind=ikind)    :: mask(N)
            integer(kind=ikind)    :: period_loc
            integer(kind=ikind)    :: exponent_loc
            integer(kind=ikind)    :: mask_from
            integer(kind=ikind)    :: mask_till
            integer(kind=ikind)    :: ls
        
            equivalence(factors, factors_char)
            factors_char = s
            factors = factors - digit_0
        
            ls = len(s,kind=ikind)
        
            period_loc   = findloc(factors, period, dim=1, kind=ikind)
            exponent_loc = scan(s, 'eE', back=.true., kind=ikind)
            if(exponent_loc == 0) exponent_loc = ls + one
            if(period_loc   == 0) period_loc   = exponent_loc
        
            ! mask      = is_digit(factors)
            where (0 <= factors .and. factors <= 9)
                mask = 1
            elsewhere
                mask = 0
            end where
        
            mask_from = 18_ikind - period_loc
            mask_till = mask_from + exponent_loc - 2_ikind
        
            r_coefficient = sum( &
                    factors(:exponent_loc - one)  * &
                    base(mask_from:mask_till) * &
                    mask(:exponent_loc - one))
            r_exponent = sum( &
                    factors(exponent_loc+one:ls) * &
                    mask(exponent_loc+one:ls)  * &
                    base(17_ikind-(ls-exponent_loc):16_ikind))
            if(factors(exponent_loc+one) == minus_sign) r_exponent    = -r_exponent
            if(factors(one)              == minus_sign) r_coefficient = -r_coefficient
            r = r_coefficient * 10 ** r_exponent
        end function str2real_c
        
        end module str2real_m
        
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