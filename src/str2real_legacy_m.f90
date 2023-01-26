! Legacy module containing some of the implemantations discussed in the fortran forum
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