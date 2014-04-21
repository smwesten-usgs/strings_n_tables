module strings2

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use exceptions
  use types
  implicit none

  private

  public :: operator(+)
!  public :: assignment(=)
!  public :: operator(==)
!   public :: dquote, clean
!   public :: asCharacter

  interface operator(+)
    procedure :: concatenate_char_char_fn
     procedure :: concatenate_char_int_fn
     procedure :: concatenate_char_float_fn
!     procedure :: concatenate_char_double_fn
  end interface operator(+)

!   interface assignment(=)
!     procedure :: integer_to_char_sub
!     procedure :: float_to_char_sub
!     procedure :: double_to_char_sub
!   end interface assignment(=)

!   interface operator(==)
!     procedure :: is_char_equal_to_char_fn
!     procedure :: is_logical4_equal_to_logical1_fn
!     procedure :: is_logical1_equal_to_logical4_fn
!   end interface operator(==)


!   interface isNumeric
!     module procedure is_char_numeric_fn
!   end interface isNumeric

!   interface isInteger
!     module procedure is_char_an_integer_fn
!   end interface isInteger

   interface asCharacter
     procedure :: int_to_char_fn
     procedure :: float_to_char_fn
!     procedure :: double_to_char_fn
!     procedure :: logical_to_char_fn
   end interface asCharacter

!   interface asInt   
!     procedure :: char_to_int_fn
!   end interface asInt
 
!   interface asFloat
!   	procedure :: char_to_float_fn
!   end interface asFloat

!   interface asDouble
!   	procedure :: char_to_double_fn
!   end interface asDouble



!   interface dquote
!     procedure :: dquote_char_fn
!   end interface dquote

contains

  function concatenate_char_char_fn(sChar1, sChar2)   result(sChar)

    character (len=*), intent(in)      :: sChar1
    character (len=*), intent(in)      :: sChar2    
    character (len=:), allocatable     :: sChar

    sChar = sChar1 // sChar2

  end function concatenate_char_char_fn

!--------------------------------------------------------------------------------------------------

  function concatenate_char_int_fn(sChar1, iValue1)   result(sChar)

    character (len=*), intent(in)        :: sChar1
    integer (kind=c_int), intent(in)     :: iValue1    
    character (len=:), allocatable       :: sChar

    sChar = sChar1 // asCharacter( iValue1 )

  end function concatenate_char_int_fn

!--------------------------------------------------------------------------------------------------

  function concatenate_char_float_fn(sChar1, fValue1)   result(sChar)

    character (len=*), intent(in)        :: sChar1
    real (kind=c_float), intent(in)      :: fValue1    
    character (len=:), allocatable       :: sChar

    sChar = sChar1 // asCharacter( fValue1 )

  end function concatenate_char_float_fn

!--------------------------------------------------------------------------------------------------

  function int_to_char_fn(iValue)    result(sChar)

    integer (kind=c_int), intent(in)  :: iValue
    character (len=:), allocatable    :: sChar

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sBuf

    write(sBuf, fmt=*, iostat=iStat)  iValue

    if (iStat==0) then
      sChar = trim( adjustl(sBuf) )
    else
      sChar = "NA"
    endif  

  end function int_to_char_fn

!--------------------------------------------------------------------------------------------------

  function float_to_char_fn(fValue, iNumdigits)    result(sChar)

    real (kind=c_float), intent(in)             :: fValue
    integer (kind=c_int), intent(in), optional  :: iNumdigits
    character (len=:), allocatable              :: sChar

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=:), allocatable :: sFmt
    character (len=32)   :: sBuf

    if (present(iNumDigits) ) then
      sFmt = "(G0." // asCharacter(iNumdigits)//")"
    else
      sFmt = "(G0.4)"
    endif    

    write(sBuf, fmt=sFmt, iostat=iStat)  fValue

    if (iStat==0) then
      sChar = trim( adjustl(sBuf) )
    else
      sChar = "NA"
    endif  

  end function float_to_char_fn



end module strings2