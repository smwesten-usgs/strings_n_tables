module strings

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  implicit none

  private


  character (len=52), parameter :: sALPHA = &
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  character (len=26), parameter :: sUPPERCASE = &
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  

  character (len=15), parameter :: sNUMERIC = &
    "0123456789.eE+-"  

  character (len=10), parameter :: sINTEGER = &
    "0123456789"

  character (len=2), parameter :: sWHITESPACE = achar(9)//" "  

  logical (kind=c_bool), parameter :: lFALSE = .false.
  logical (kind=c_bool), parameter :: lTRUE = .true.


  type, public :: T_STRING 
    character (len=1), private, dimension(:), allocatable :: sChars

  contains

    procedure :: is_string_numeric_fn
    generic, public :: isNumeric => is_string_numeric_fn

    procedure :: is_string_an_integer_fn
    generic, public :: isInteger => is_string_an_integer_fn 

    procedure :: convert_to_int_fn
    generic, public :: asInt => convert_to_int_fn

    procedure :: convert_to_float_fn
    generic, public :: asFloat => convert_to_float_fn

    procedure :: convert_to_double_fn
    generic, public :: asDouble => convert_to_double_fn

    procedure :: convert_to_character_fn 
    generic, public :: asCharacter => convert_to_character_fn 

    procedure :: convert_to_uppercase_sub
    procedure :: convert_to_uppercase_fn
    generic, public :: toUppercase => convert_to_uppercase_sub
    generic, public :: asUppercase => convert_to_uppercase_fn

    procedure :: convert_to_lowercase_sub
    procedure :: convert_to_lowercase_fn
    generic, public :: toLowercase => convert_to_lowercase_sub
    generic, public :: asLowercase => convert_to_lowercase_fn

    procedure :: split_and_return_text_fn
    generic, public :: chomp => split_and_return_text_fn
  
  end type T_STRING

!   type T_STRING_LIST
!     type (T_STRING), dimension(:), allocatable :: sl 

!   contains
  
!     procedure :: append => append_string_sub
! !    procedure :: push => push_string_fn
! !    procedure :: pop => pop_string_fn


!   end type T_STRING_LIST	

  public :: operator(+), assignment(=), operator(==)
  public :: len

  interface operator(+)
    procedure :: concatenate_string_string_fn
    procedure :: concatenate_string_int_fn
    procedure :: concatenate_string_char_fn
    procedure :: concatenate_string_real_fn
    !procedure :: concatenate_int_string_fn
    !procedure :: concatenate_real_string_fb
  end interface operator(+)

  interface assignment(=)
    procedure :: string_to_character_sub
    procedure :: character_to_string_sub
  end interface assignment(=)

  interface operator(==)
    procedure :: is_string_equal_to_string_fn
    procedure :: is_string_equal_to_char_fn
    procedure :: is_char_equal_to_string_fn
  end interface operator(==)    


!   interface isNumeric
!     module procedure is_string_numeric_fn
!   end interface isNumeric

!   interface isInteger
!     module procedure is_string_an_integer_fn
!   end interface isInteger

  interface len
    procedure :: string_length_fn
  end interface len

contains

  elemental function concatenate_string_string_fn(stString1, stString2)	   result(stConcatString)

    type (T_STRING), intent(in)        :: stString1
    type (T_STRING), intent(in)        :: stString2
    type (T_STRING)                    :: stConcatString
  
    ! [ LOCALS ]
    integer (kind=c_int) :: iLen1, iLen2
    integer (kind=c_int) :: iIndex

    iLen1 = len(stString1)
    iLen2 = len(stString2)

    allocate ( stConcatString%sChars( iLen1 + iLen2 ) )

    do iIndex = 1, iLen1
      stConcatString%sChars(iIndex:iIndex) = stString1%sChars(iIndex:iIndex)
    enddo  

    do iIndex = 1, iLen2
      stConcatString%sChars(iIndex+iLen1:iIndex+iLen1) = stString2%sChars(iIndex:iIndex)
    enddo  

  end function concatenate_string_string_fn


  !> Concatenate a variable-length string and a standard Fortran character type.
  !>
  !> @param [in] stString variable-length string object
  !> @param [in] sChar Standard Fortran fixed-length string
  !> @retval stConcatString stString variable-length string object
  elemental function concatenate_string_char_fn(stString1, sChar)	   result(stConcatString)

    type (T_STRING), intent(in)        :: stString1
    character (len=*), intent(in)      :: sChar
    type (T_STRING)                    :: stConcatString
  
    ! [ LOCALS ]
    integer (kind=c_int) :: iLen1, iLen2
    integer (kind=c_int) :: iIndex

    iLen1 = len(stString1)
    iLen2 = len(sChar)

    allocate ( stConcatString%sChars( iLen1 + iLen2 ) )

    do iIndex = 1, iLen1
      stConcatString%sChars(iIndex:iIndex) = stString1%sChars(iIndex:iIndex)
    enddo  

    do iIndex = 1, iLen2
      stConcatString%sChars(iIndex+iLen1:iIndex+iLen1) = sChar(iIndex:iIndex)
    enddo  

  end function concatenate_string_char_fn




  elemental function concatenate_string_int_fn(stString1, iValue)	   result(stConcatString)

    type (T_STRING), intent(in)        :: stString1
    integer (kind=c_int), intent(in)   :: iValue
    type (T_STRING)                    :: stConcatString
  
    ! [ LOCALS ]
    integer (kind=c_int) :: iLen1, iLen2
    integer (kind=c_int) :: iIndex
    character (len=20)   :: sValue

    write(sValue, fmt="(i20)") iValue
    sValue = adjustl(sValue)

    iLen1 = len(stString1)
    iLen2 = len_trim(sValue)

    allocate ( stConcatString%sChars( iLen1 + iLen2 ) )

    do iIndex = 1, iLen1
      stConcatString%sChars(iIndex:iIndex) = stString1%sChars(iIndex:iIndex)
    enddo  

    do iIndex = 1, iLen2
      stConcatString%sChars(iIndex+iLen1:iIndex+iLen1) = sValue(iIndex:iIndex)
    enddo  

  end function concatenate_string_int_fn
  


  pure function concatenate_string_real_fn(stString1, rValue)	   result(stConcatString)

    type (T_STRING), intent(in)        :: stString1
    real (kind=c_float), intent(in)    :: rValue
    type (T_STRING)                    :: stConcatString
  
    ! [ LOCALS ]
    integer (kind=c_int) :: iLen1, iLen2
    integer (kind=c_int) :: iIndex
    character (len=28)   :: sValue

    write(sValue, fmt="(g16.9)") rValue
    sValue = adjustl(sValue)

    iLen1 = len(stString1)
    iLen2 = len_trim(sValue)

    allocate ( stConcatString%sChars( iLen1 + iLen2 ) )

    do iIndex = 1, iLen1
      stConcatString%sChars(iIndex:iIndex) = stString1%sChars(iIndex:iIndex)
    enddo  

    do iIndex = 1, iLen2
      stConcatString%sChars(iIndex+iLen1:iIndex+iLen1) = sValue(iIndex:iIndex)
    enddo  

  end function concatenate_string_real_fn




  elemental subroutine character_to_string_sub(stString, sChar)

    type (T_STRING), intent(out)    :: stString
    character (len=*), intent(in)   :: sChar

    stString = character_to_string_fn(sChar)

  end subroutine character_to_string_sub
  


  elemental function character_to_string_fn(sChar)  result(stString)

    character (len=*), intent(in)        :: sChar
    type (T_STRING)                      :: stString
    
    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    allocate(stString%sChars(len_trim(sChar)))

    forall (iIndex = 1:len_trim(sChar))
    	stString%sChars(iIndex) = sChar(iIndex:iIndex)
    end forall

  end function character_to_string_fn



  elemental subroutine string_to_character_sub(sChar, stString)

    character (len=*), intent(out)  :: sChar
    class (T_STRING), intent(in)    :: stString

    sChar = convert_to_character_fn(stString)

  end subroutine string_to_character_sub



  pure function convert_to_character_fn(stString)  result(sChar)

    class (T_STRING), intent(in)         :: stString
    character (len=len(stString))        :: sChar

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    do iIndex = 1,len(stString)
    	sChar(iIndex:iIndex) = stString%sChars(iIndex)
    enddo

  end function convert_to_character_fn



  pure function string_length_fn(stString)     result(iValue)

  	type (T_STRING), intent(in) :: stString
    integer (kind=c_int) :: iValue

    if (allocated(stString%sChars)) then

      iValue = size(stString%sChars,1)

    else

      iValue = 0

    endif	

  end function string_length_fn

  

  pure function is_string_an_integer_fn(this)   result(lBool)

    class (T_STRING), intent(in) :: this
    logical (kind=c_bool)       :: lBool

    ! [ LOCALS ] 
    integer (kind=c_int) :: iResult
    character (len=len(this)) :: sChar

    sChar = convert_to_character_fn(this)

    iResult = verify(string = sChar, &
                     set = sINTEGER)

    if (iResult == 0) then
    	lBool = lTRUE
    else
    	lBool = lFALSE
    endif

  end function is_string_an_integer_fn



  pure function is_string_numeric_fn(this)   result(lBool)

    class (T_STRING), intent(in) :: this
    logical (kind=c_bool)       :: lBool

    ! [ LOCALS ] 
    integer (kind=c_int) :: iResult
    character (len=len(this)) :: sChar

    sChar = convert_to_character_fn(this)

    iResult = verify(string = sChar, &
    	               set = sNUMERIC)

    if (iResult == 0) then
    	lBool = lTRUE
    else
    	lBool = lFALSE
    endif

  end function is_string_numeric_fn

!   subroutine append_string_sub(this, stString)

!     class (T_STRING_LIST), intent(inout) :: this
!     class (T_STRING), intent(in)         :: stString

!     ! [ LOCALS ] 
!     type (T_STRING_LIST) :: templist
!     integer (kind=c_int) :: iSize

!     if (allocated(this%sl)) then
!       iSize = size(this%sl,1)
!       allocate(templist%sl(iSize))

!       templist%sl(1:iSize) = this%sl(1:iSize)
!       deallocate(this%sl)
!       allocate(this%sl(iSize+1))
!       this%sl(1:iSize) = templist%sl(1:iSize)
!       this%sl(iSize + 1) = this%sl(iSize)
 
!     else
!     	allocate(this%sl(1))
!     	this%sl(1) = stString
!     endif
    	

!   end subroutine append_string_sub  
    
 function split_and_return_text_fn(this, sDelimiters)    result(sChar)

    class (T_STRING), intent(inout)           :: this
    character (len=*), intent(in), optional   :: sDelimiters
    character (len=:), allocatable            :: sChar

    ! [ LOCALS ]
    character (len=:), allocatable :: sMyDelimiters
    integer (kind=c_int) :: iIndex
    character (len=len(this)) :: sTempText

    if ( present(sDelimiters) ) then
    	sMyDelimiters = sDelimiters
    else
    	sMyDelimiters = sWHITESPACE
    endif
    
    sTempText = convert_to_character_fn(this)

    iIndex = scan( string = sTempText, &
    	            set = sMyDelimiters )

    if (iIndex == 0) then
      sChar = sTempText
      this = ""
    else
      sChar = trim(adjustl( sTempText(1:iIndex-1) ) )
      this = trim(adjustl(sTempText(iIndex + 1:) ) )
    endif  

  end function split_and_return_text_fn  	

  

  function is_string_equal_to_string_fn(stString1, stString2)  result(lBool)

    type (T_STRING), intent(in) :: stString1
    type (T_STRING), intent(in) :: stString2
    logical (kind=c_bool)       :: lBool  

    if (trim(stString1%asUppercase() ) .eq. &
        trim(stString2%asUppercase() ) )  then

      lBool = lTRUE

    else
    
      lBool = lFALSE

    endif    

  end function is_string_equal_to_string_fn



  function is_string_equal_to_char_fn(stString, sChar)  result(lBool)

    type (T_STRING), intent(in)   :: stString
    character (len=*), intent(in) :: sChar
    logical (kind=c_bool)         :: lBool  

    ! [ LOCALS ]
    type (T_STRING) :: stString2

    stString2 = sChar  

    if (trim(stString%asUppercase() ) .eq. &
        trim(stString2%asUppercase() ) )  then

      lBool = lTRUE

    else
    
      lBool = lFALSE

    endif    

  end function is_string_equal_to_char_fn



  function is_char_equal_to_string_fn(sChar, stString)  result(lBool)

    character (len=*), intent(in) :: sChar
    type (T_STRING), intent(in)   :: stString
    logical (kind=c_bool)         :: lBool  

    ! [ LOCALS ]
    type (T_STRING) :: stString2

    stString2 = sChar  

    if (trim(stString%asUppercase() ) .eq. &
        trim(stString2%asUppercase() ) )  then

      lBool = lTRUE

    else
    
      lBool = lFALSE

    endif    

  end function is_char_equal_to_string_fn


  function convert_to_int_fn(this)   result(iResult)

    class (T_STRING), intent(in) :: this
    integer (kind=c_int)         :: iResult

    ! [ LOCALS ]
    character (len=len(this)) :: sTempText

    sTempText = this%asCharacter()

    if (this%isInteger()) then
      read(sTempText, fmt=*) iResult
    else
      iResult = - ( huge(iResult) - 100 )  
    endif

  end function convert_to_int_fn



  function convert_to_float_fn(this)   result(rResult)

    class (T_STRING), intent(in) :: this
    real (kind=c_float)          :: rResult

    ! [ LOCALS ]
    character (len=len(this)) :: sTempText

    sTempText = this%asCharacter()

    if (this%isNumeric()) then
      read(sTempText, fmt=*) rResult
    else
      rResult = - ( huge(rResult) - 100. )    
    endif

  end function convert_to_float_fn


  function convert_to_double_fn(this)   result(dResult)

    class (T_STRING), intent(in) :: this
    real (kind=c_double)         :: dResult

    ! [ LOCALS ]
    character (len=len(this)) :: sTempText

    sTempText = this%asCharacter()

    if ( this%isNumeric() ) then
      read(sTempText, fmt=*) dResult
    else
      dResult = - ( huge(dResult) - 100. )    
    endif

  end function convert_to_double_fn

  subroutine convert_to_uppercase_sub(this)

    class (T_STRING), intent(inout)      :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iASCII_charnum

    if (len(this) > 0) then
      do iIndex = 1, len(this)
        iASCII_charnum = iachar(this%sChars(iIndex))
        if (     iASCII_charnum >= 97 &
          .and.  iASCII_charnum <= 122) then
          this%sChars(iIndex) = achar(iASCII_charnum - 32)
        endif
      enddo

    endif

  end subroutine convert_to_uppercase_sub



  function convert_to_uppercase_fn(this)  result(sChar)

    class (T_STRING), intent(in)      :: this
    character (len=len(this))         :: sChar

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iASCII_charnum

    sChar = ""

    if (len(this) > 0) then
      do iIndex = 1, len(this)
        iASCII_charnum = iachar(this%sChars(iIndex))
        if (     iASCII_charnum >= 97 &
          .and.  iASCII_charnum <= 122) then
          sChar(iIndex:iIndex) = achar(iASCII_charnum - 32)
        else
          sChar(iIndex:iIndex) = this%sChars(iIndex)
        endif

      enddo

    endif

  end function convert_to_uppercase_fn



 subroutine convert_to_lowercase_sub(this)

    class (T_STRING), intent(inout)      :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iASCII_charnum

    if (len(this) > 0) then
      do iIndex = 1, len(this)
        iASCII_charnum = iachar(this%sChars(iIndex))
        if (     iASCII_charnum >= 65 &
          .and.  iASCII_charnum <= 90) then
          this%sChars(iIndex) = achar(iASCII_charnum + 32)
        endif
      enddo

    endif

  end subroutine convert_to_lowercase_sub



  function convert_to_lowercase_fn(this)  result(sChar)

    class (T_STRING), intent(in)      :: this
    character (len=len(this))         :: sChar

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iASCII_charnum

    sChar = ""

    if (len(this) > 0) then
      do iIndex = 1, len(this)
        iASCII_charnum = iachar(this%sChars(iIndex))
        if (     iASCII_charnum >= 65 &
          .and.  iASCII_charnum <= 90) then
          sChar(iIndex:iIndex) = achar(iASCII_charnum + 32)
        else
          sChar(iIndex:iIndex) = this%sChars(iIndex)
        endif

      enddo

    endif

  end function convert_to_lowercase_fn




end module strings