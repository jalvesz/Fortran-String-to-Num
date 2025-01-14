module str2num_m
    use iso_c_binding 
    implicit none
    private
    public :: str2int, str2int_p
    public :: str2real, str2real_p
    
    integer, parameter :: wp    = kind(1.0d0)
    integer, parameter :: ikind = selected_int_kind(2)
    integer(kind=ikind), parameter :: zero       = 0_ikind
    integer(kind=ikind), parameter :: one        = 1_ikind
    integer(kind=ikind), parameter :: nine       = 9_ikind
    integer(kind=ikind), parameter :: digit_0    = ichar('0',kind=ikind)
    integer(kind=ikind), parameter :: period     = ichar('.',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: comma      = ichar(',',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: minus_sign = ichar('-',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: plus_sign  = ichar('+',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: Inf        = ichar('I',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: NaN        = ichar('N',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: le         = ichar('e',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: BE         = ichar('E',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: ld         = ichar('d',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: BD         = ichar('D',kind=ikind) - digit_0
    integer(kind=ikind), parameter :: LF = 10, CR = 13, WS = 32
    
    real(c_double), parameter :: rNaN = TRANSFER(9218868437227405313_c_int64_t, 1._c_double)
    
    integer(kind=ikind), parameter :: nwnb = 40 !> number of whole number factors
    integer(kind=ikind), parameter :: nfnb = 40 !> number of fractional number factors
    real(wp), parameter :: whole_number_base(nwnb) =             &
        [1d39,  1d38,  1d37,  1d36,  1d35,  1d34, 1d33,  1d32,   & 
         1d31,  1d30,  1d29,  1d28,  1d27,  1d26, 1d25,  1d24,   &
         1d23,  1d22,  1d21,  1d20,  1d19,  1d18, 1d17,  1d16,   &
         1d15,  1d14,  1d13,  1d12,  1d11,  1d10,  1d9,   1d8,   &
         1d7,   1d6,   1d5,   1d4,   1d3,   1d2,   1d1,   1d0]
    real(wp), parameter :: fractional_base(nfnb)   =                 &
        [1d-1,  1d-2,  1d-3,  1d-4,  1d-5,  1d-6,  1d-7,  1d-8,  &
         1d-9,  1d-10, 1d-11, 1d-12, 1d-13, 1d-14, 1d-15, 1d-16, &
         1d-17, 1d-18, 1d-19, 1d-20, 1d-21, 1d-22, 1d-23, 1d-24, &
         1d-25, 1d-26, 1d-27, 1d-28, 1d-29, 1d-30, 1d-31, 1d-32, &
         1d-33, 1d-34, 1d-35, 1d-36, 1d-37, 1d-38, 1d-39, 1d-40 ]
    real(wp), parameter :: period_skip = 0d0
    real(wp), parameter :: expbase(nwnb+nfnb) = [whole_number_base, fractional_base]

    contains
    
    !---------------------------------------------
    ! String To Integer implementations
    !---------------------------------------------
    elemental function str2int(s) result(int)
        ! -- In/out Variables
        character(*), intent(in) :: s !> input string
        integer :: int !> output integer value
        ! -- Internal Variables
        integer(1) :: p !> position within the number
        integer(1)  :: stat ! error status
        !----------------------------------------------
        call str2int32(s,int,p,stat)
    end function
    
    function str2int_p(s,stat) result(int)
        ! -- In/out Variables
        character(len=:), pointer :: s !> input string
        integer :: int !> output integer value
        integer(1),intent(inout), optional :: stat
        ! -- Internal Variables
        integer(1) :: p !> position within the number
        integer(1) :: err
        !----------------------------------------------
        call str2int32(s,int,p,err)
        p = min( p , len(s) )
        s => s(p:)
        if(present(stat)) stat = err
    end function

    elemental subroutine str2int32(s,int,p,stat)
        !> Return an unsigned 32-bit integer
        ! -- In/out Variables
        character(*), intent(in) :: s !> input string
        integer, intent(inout)  :: int !> output integer value
        integer(1), intent(out)  :: p !> position within the number
        integer(1), intent(out)  :: stat !> status upon succes of failure to read
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

    !---------------------------------------------
    ! String To Real function interfaces
    !---------------------------------------------
    
    elemental function str2real(s) result(r)
        ! -- In/out Variables
        character(*), intent(in) :: s !> input string
        real(wp) :: r !> Output real value
        ! -- Internal Variables
        integer(1) :: p !> position within the number
        integer(1) :: stat ! error status
        !----------------------------------------------
        call str2real_dp(s,r,p,stat)
    end function
    
    function str2real_p(s,stat) result(r)
        ! -- In/out Variables
        character(len=:), pointer :: s !> input string
        real(wp) :: r                  !> Output real value
        integer(1),intent(inout), optional :: stat
        ! -- Internal Variables
        integer(1) :: p !> position within the number
        integer(1) :: err
        !----------------------------------------------
        call str2real_dp(s,r,p,err)
        p = min( p , len(s) )
        s => s(p:)
        if(present(stat)) stat = err
    end function
    
    !---------------------------------------------
    ! String To Real implementations
    !---------------------------------------------
    
    elemental subroutine str2real_dp(s,r,p,stat)
        !> Sequentially unroll the character and get the sub integers composing the whole number, fraction and exponent
        ! -- In/out Variables
        character(*), intent(in) :: s !> input string
        real(wp), intent(inout)  :: r !> Output real value
        integer(1), intent(out)  :: p !> last position within the string
        integer(1), intent(out)  :: stat !> status upon success or failure to read
        ! -- Internal Variables
        integer(1)  :: sign, sige !> sign of integer number and exponential
        integer(wp) :: int_wp !> long integer to capture fractional part
        integer     :: i_exp !> integer to capture whole number part
        integer(1)  :: i, pP, pE, val , resp
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
            r = sign*huge(1_wp); return
        else if( iachar(s(p:p)) == NaN ) then
            r = rNaN; return
        end if
        !----------------------------------------------
        ! read whole and fractional number in a single integer
        pP = 127
        int_wp = 0
        do i = p, min(19+p-1,len(s))
            val = iachar(s(i:i))-digit_0
            if( val >= 0 .and. val <= 9 ) then
                int_wp = int_wp*10 + val
            else if( val == period ) then
                pP = i
            else
                exit
            end if
        end do
        pE = i ! Fix the exponent position
        do while( i<=len(s) )
           val = iachar(s(i:i))-digit_0
           if( val < 0 .or. val > 9 ) exit
           i = i + 1
        end do
        p = i
        resp = pE-min(pP,p) ! If no decimal indicator found it is taken as being in the current p position
        if( resp <= 0 ) resp = resp+1 
        !----------------------------------------------
        ! Get exponential
        sige = 1
        if( p<len(s) ) then
            if( any([le,BE,ld,BD]+digit_0==iachar(s(p:p))) ) p = p + 1
            if( iachar(s(p:p)) == minus_sign+digit_0 ) then
                sige = -1
                p = p + 1
            else if( iachar(s(p:p)) == plus_sign+digit_0 ) then
                p = p + 1
            end if
        end if
        
        i_exp = 0
        do while( p<=len(s) )
            val = iachar(s(p:p))-digit_0
            if( val >= 0 .and. val <= 9) then
                i_exp = i_exp*10_ikind + val ; p = p + 1
            else
                exit
            end if
        end do

        r = sign*int_wp*expbase(nwnb-1+resp-sige*max(0,i_exp))
        stat = 0
    end subroutine

    !---------------------------------------------
    ! Utility functions
    !---------------------------------------------
    
    elemental function mvs2nwsp(s) result(p)
        !> get position of the next non white space character
        character(*),intent(in) :: s !> character chain
        integer(1) :: p !> position
        !----------------------------------------------
        p = 1
        do while( p<len(s) .and. (iachar(s(p:p))==WS.or.iachar(s(p:p))==LF.or.iachar(s(p:p))==CR) ) 
            p = p + 1
        end do
    end function
    
    elemental function mvs2wsp(s) result(p)
        !> get position of the next white space character
        character(*),intent(in) :: s !> character chain
        integer(1) :: p !> position
        !----------------------------------------------
        p = 1
        do while( p<len(s) .and. .not.(iachar(s(p:p))==WS.or.iachar(s(p:p))==LF.or.iachar(s(p:p))==CR) ) 
            p = p + 1
        end do
    end function
    
    end module str2num_m