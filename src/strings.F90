module strings

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use exceptions
  use types
  implicit none

  private

  public :: operator(+)
!  public :: assignment(=)
!  public :: operator(==)
   public :: dquote
   public :: clean
   public :: chomp
   public :: replace
   public :: remove_repeats
   public :: asCharacter

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

    interface chomp
      procedure :: split_and_return_text_sub
    end interface chomp

   interface dquote
     procedure :: dquote_char_fn
   end interface dquote

   interface replace
      procedure :: replace_character_sub
   end interface replace

contains

  function concatenate_char_char_fn(sText1, sText2)   result(sText)

    character (len=*), intent(in)      :: sText1
    character (len=*), intent(in)      :: sText2    
    character (len=:), allocatable     :: sText

    sText = sText1 // sText2

  end function concatenate_char_char_fn

!--------------------------------------------------------------------------------------------------

  function concatenate_char_int_fn(sText1, iValue1)   result(sText)

    character (len=*), intent(in)        :: sText1
    integer (kind=c_int), intent(in)     :: iValue1    
    character (len=:), allocatable       :: sText

    sText = sText1 // asCharacter( iValue1 )

  end function concatenate_char_int_fn

!--------------------------------------------------------------------------------------------------

  function concatenate_char_float_fn(sText1, fValue1)   result(sText)

    character (len=*), intent(in)        :: sText1
    real (kind=c_float), intent(in)      :: fValue1    
    character (len=:), allocatable       :: sText

    sText = sText1 // asCharacter( fValue1 )

  end function concatenate_char_float_fn

!--------------------------------------------------------------------------------------------------

  function int_to_char_fn(iValue)    result(sText)

    integer (kind=c_int), intent(in)  :: iValue
    character (len=:), allocatable    :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=32)   :: sBuf

    write(sBuf, fmt=*, iostat=iStat)  iValue

    if (iStat==0) then
      sText = trim( adjustl(sBuf) )
    else
      sText = "NA"
    endif  

  end function int_to_char_fn

!--------------------------------------------------------------------------------------------------

  function float_to_char_fn(fValue, iNumdigits)    result(sText)

    real (kind=c_float), intent(in)             :: fValue
    integer (kind=c_int), intent(in), optional  :: iNumdigits
    character (len=:), allocatable              :: sText

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=:), allocatable :: sFmt
    character (len=32)   :: sBuf

    if (present(iNumDigits) ) then
      write(sFmt, fmt="('(G0.',i0,')')") iNumdigits
    else
      sFmt = "(G0.4)"
    endif    

    write(sBuf, fmt=sFmt, iostat=iStat)  fValue

    if (iStat==0) then
      sText = trim( adjustl(sBuf) )
    else
      sText = "NA"
    endif  

  end function float_to_char_fn

!--------------------------------------------------------------------------------------------------

  function dquote_char_fn(sText1)    result(sText)

    character (len=*), intent(in)         :: sText1
    character (len=len_trim(sText1)+2)    :: sText

    sText = '"'//trim(sText1)//'"'

  end function dquote_char_fn

!--------------------------------------------------------------------------------------------------

  !> Strip delimiters from a text string.
  !!
  !! Remove delimiting characters from a test string. The delimiters may optionally be supplied.
  !! @param[in] sTextIn
  function clean(sText1, sDelimiters)            result(sText)

    ! ARGUMENTS
    character (len=*), intent(inout)           :: sText1
    character (len=*), intent(in), optional    :: sDelimiters
    character (len=:), allocatable :: sText

    ! LOCALS
    character (len=256)            :: sBuf
    integer (kind=c_int)           :: iR                 ! Index in sRecord
    integer (kind=c_int)           :: iIndex1, iIndex2
    character (len=:), allocatable :: sDelimiters_

    ! eliminate any leading spaces
    sText1 = adjustl(sText1)
    sBuf = ""
    iIndex2 = 0

    if (present(sDelimiters) ) then
      sDelimiters_ = sDelimiters
    else  
      sDelimiters_ = ":/;,"
    endif

    do iIndex1 = 1,len_trim(sText1)

      iR = SCAN(sText1(iIndex1:iIndex1), sDelimiters_)
  
      if(iR==0) then
        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
      end if

    enddo

    sText = trim(sBuf)

  end function clean

!--------------------------------------------------------------------------------------------------

  !> Strip repeated characters from string.
  !!
  !! Remove repeated characters from a string. By default the function looks for repeated spaces and eliminates them.
  !! @param[in] sTextIn
  function remove_repeats(sText1, sChar)            result(sText)

    ! ARGUMENTS
    character (len=*), intent(inout)           :: sText1
    character (len=*), intent(in), optional    :: sChar
    character (len=:), allocatable :: sText

    ! LOCALS
    character (len=256)            :: sBuf
    integer (kind=c_int)           :: iR                 ! Index in sRecord
    integer (kind=c_int)           :: iIndex1, iIndex2
    character (len=1)              :: sChar_
    logical (kind=c_bool)          :: lPreviouslyFound

    ! eliminate any leading spaces
    sText1 = adjustl(sText1)
    sBuf = ""
    iIndex2 = 0
    lPreviouslyFound = lFALSE

    if (present(sChar) ) then
      sChar_ = sChar
    else  
      sChar_ = " "
    endif

    do iIndex1 = 1,len_trim(sText1)

      iR = SCAN(sText1(iIndex1:iIndex1), sChar_)
  
      if(iR==0) then
        ! sChar_ was not found
        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
        lPreviouslyFound = lFALSE

      elseif( lPreviouslyFound ) then
        ! sChar_ was found, and was also found in the position preceding this one

        ! No OP

      else
        ! sChar_ was found, but was *not* found in the preceding position

        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sText1(iIndex1:iIndex1)
        lPreviouslyFound = lTRUE

      end if

    enddo

    sText = trim(sBuf)

  end function remove_repeats

!--------------------------------------------------------------------------------------------------

  subroutine split_and_return_text_sub(sText1, sText2, sDelimiters)

    character (len=*), intent(inout)                     :: sText1
    character ( len=len_trim( sText1 ) ), intent(out)    :: sText2
    character (len=*), intent(in), optional              :: sDelimiters

    ! [ LOCALS ]
    character (len=:), allocatable :: sDelimiters_
    integer (kind=c_int) :: iIndex

    if ( present(sDelimiters) ) then
      sDelimiters_ = sDelimiters
    else
      sDelimiters_ = sWHITESPACE
    endif

    iIndex = scan( string = sText1, &
                   set = sDelimiters_ )

    if (iIndex == 0) then
      ! no delimiters found; return string as was supplied originally
      sText2 = sText1
      sText1 = ""
    else
      ! delimiters were found; split and return the chunks of text
      sText2 = trim(adjustl( sText1(1:iIndex-1) ) )
      sText1 = trim(adjustl(sText1(iIndex + 1:) ) )
    endif

  end subroutine split_and_return_text_sub

!--------------------------------------------------------------------------------------------------

   subroutine replace_character_sub(sText1, sFind, sReplace)

    character (len=*), intent(inout)    :: sText1
    character (len=1), intent(in)       :: sFind
    character (len=1), intent(in)       :: sReplace

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    if ( len(sText1) > 0 ) then

      do iIndex = 1, len(sText1)

        if ( sText1(iIndex:iIndex) .eq. sFind)    sText1(iIndex:iIndex) = sReplace

      enddo

    endif

  end subroutine replace_character_sub
    
end module strings