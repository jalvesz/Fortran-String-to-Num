module str2num_m
    use, intrinsic :: ieee_arithmetic, only: IEEE_Value, IEEE_QUIET_NAN
    implicit none
    private
    public :: str2real, str2real_p, str2int, str2int_p
    
    integer, parameter :: wp    = kind(1.0d0)
    integer, parameter :: ikind = selected_int_kind(2)
    integer(kind=ikind), parameter :: digit_0    = ichar('0',kind=ikind)
    integer(kind=ikind), parameter :: period     = ichar('.',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: minus_sign = ichar('-',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: plus_sign  = ichar('+',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: Inf        = ichar('I',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: NaN        = ichar('N',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: le         = ichar('e',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: BE         = ichar('E',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: LF = 10, CR = 13, WS = 32
    
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
    
    contains
    
    elemental subroutine str2uint32_base(s,int,p,stat)
        !> Return an unsigned 32-bit integer
        ! -- In/out Variables
        character(*), intent(in) :: s !< input string
        integer, intent(inout)  :: int !< Output real value
        integer(1), intent(out)  :: p !< position within the number
        integer(1), intent(out)  :: stat !< status upon succes of failure to read
        ! -- Internal Variables
        integer(1)  :: val 
        !----------------------------------------------
        stat = 23 !> initialize error status with any number > 0
        !----------------------------------------------
        ! Find first non white space
        p = mvs2nwsp(s)
        !----------------------------------------------
        int = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                int = int*10 + val ; p = p + 1
            else
                exit
            end if
        end do
        stat = 0
    end subroutine
    
    elemental function str2int(s) result(int)
        ! -- In/out Variables
        character(*), intent(in) :: s !< input string
        integer :: int !< Output integer 32 value
        ! -- Internal Variables
        integer(1) :: p !< position within the number
        integer(1)  :: stat ! error status
        !----------------------------------------------
        call str2uint32_base(s,int,p,stat)
    end function
    
    function str2int_p(s,stat) result(int)
        ! -- In/out Variables
        character(len=:), pointer :: s !< input string
        integer :: int !< Output integer 32 value
        integer(1),intent(inout), optional :: stat
        ! -- Internal Variables
        integer(1) :: p !< position within the number
        integer(1) :: err
        !----------------------------------------------
        call str2uint32_base(s,int,p,err)
        p = min( p , len(s) )
        s => s(p:)
        if(present(stat)) stat = err
    end function
    
    elemental subroutine str2real_base(s,r,p,stat)
        ! -- In/out Variables
        character(*), intent(in) :: s !< input string
        real(wp), intent(inout)  :: r !< Output real value
        integer(1), intent(out)  :: p !< position within the number
        integer(1), intent(out)  :: stat !< status upon succes of failure to read
        ! -- Internal Variables
        integer(1)  :: sign, sige !< sign of integer number and exponential
        integer     :: int_4  !< integer to capture whole number part
        integer(wp) :: int_wp !< long integer to capture fractional part
        integer(1)  :: pP, i_exp, val 
        !----------------------------------------------
        stat = 23 !> initialize error status with any number > 0
        !----------------------------------------------
        ! Find first non white space
        p = mvs2nwsp(s)
        !----------------------------------------------
        ! Verify leading negative
        sign = 1
        if( iachar(s(p:p)) == minus_sign+digit_0 ) then
            sign = -1 ; p = p + 1
        end if
        if( iachar(s(p:p)) == Inf ) then
            r = sign*huge(1.0); return
        else if( iachar(s(p:p)) == NaN ) then
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
            r = sign*int_4; goto 10
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
            goto 10
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
        stat = 0
    end subroutine
    
    elemental function str2real(s) result(r)
        ! -- In/out Variables
        character(*), intent(in) :: s !< input string
        real(wp) :: r !< Output real value
        ! -- Internal Variables
        integer(1) :: p !< position within the number
        integer(1)  :: stat ! error status
        !----------------------------------------------
        call str2real_base(s,r,p,stat)
    end function
    
    function str2real_p(s,stat) result(r)
        ! -- In/out Variables
        character(len=:), pointer :: s !< input string
        real(wp) :: r                  !< Output real value
        integer(1),intent(inout), optional :: stat
        ! -- Internal Variables
        integer(1) :: p !< position within the number
        integer(1) :: err
        !----------------------------------------------
        call str2real_base(s,r,p,err)
        p = min( p , len(s) )
        s => s(p:)
        if(present(stat)) stat = err
    end function
    
    !---------------------------------------------
    ! Utility functions
    !---------------------------------------------
    
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