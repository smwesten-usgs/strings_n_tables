!> @file __FILE__
!!
!! File contains a single module for manipulating strings
module strings

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use types
  implicit none

  private

  !> T_STRING provides variable-length strings along with numerous
  !!  convenience functions to make string manipulation less tedious
  type, public :: T_STRING 
    character (len=1), private, dimension(:), allocatable :: sChars

  contains

    procedure, private :: is_string_numeric_fn
    generic, public    :: isNumeric => is_string_numeric_fn

    procedure, private :: is_string_an_integer_fn
    generic, public    :: isInteger => is_string_an_integer_fn 

    procedure, private :: convert_to_int_fn
    generic, public    :: asInt => convert_to_int_fn

    procedure, private :: convert_to_float_fn
    generic, public    :: asFloat => convert_to_float_fn

    procedure, private :: convert_to_double_fn
    !> @sa convert_to_double_fn
    generic, public    :: asDouble => convert_to_double_fn

    procedure, private :: convert_to_character_fn 
    generic, public    :: asCharacter => convert_to_character_fn 

    procedure, private :: convert_to_uppercase_sub
    procedure, private :: convert_to_uppercase_fn
    generic, public    :: toUppercase => convert_to_uppercase_sub
    generic, public    :: asUppercase => convert_to_uppercase_fn

    procedure, private :: convert_to_lowercase_sub
    procedure, private :: convert_to_lowercase_fn
    generic, public    :: toLowercase => convert_to_lowercase_sub
    generic, public    :: asLowercase => convert_to_lowercase_fn

    procedure, private :: split_and_return_text_fn
    generic, public    :: chomp => split_and_return_text_fn
 
    procedure, private :: replace_character_sub
    generic, public    :: replace => replace_character_sub

    procedure, private :: return_length_fn
    generic, public    :: length => return_length_fn

    procedure, private :: deallocate_sub
    generic, public    :: deallocate => deallocate_sub

  end type T_STRING

  public :: operator(+), assignment(=), operator(==)
  public :: len, assert

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
    procedure :: string_to_string_sub
!    procedure :: string_list_to_string_list_sub
  end interface assignment(=)

  interface operator(==)
    procedure :: is_string_equal_to_string_fn
    procedure :: is_string_equal_to_char_fn
    procedure :: is_char_equal_to_string_fn
    procedure :: is_logical4_equal_to_logical1_fn
    procedure :: is_logical1_equal_to_logical4_fn
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

  interface assert
     procedure :: assert_4bit
     procedure :: assert_1bit
  end interface assert

contains

  subroutine deallocate_sub(this)

    class (T_STRING), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if (allocated(this%sChars) )   deallocate(this%sChars, stat = iStat)

    call assert(iStat == 0, "Failed to deallocate memory", &
         __FILE__, __LINE__)

   end subroutine deallocate_sub 


  
  !! Concatenate two variable-length strings
  !!
  !! @param[in] stString1 first of the two variable-length string objects
  !! @param[in] stString2 second of the variable-length string objects
  !! @retval stConcatString concatenation of stString1 and stString2
  function concatenate_string_string_fn(stString1, stString2)	   result(stConcatString)

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
  function concatenate_string_char_fn(stString1, sChar)	   result(stConcatString)

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



    !> Concatenate a variable-length string and an integer.
    !!
    !! @param [in] stString variable-length string object
    !! @param [in] iValue Integer data value
    !! @retval stConcatString stString variable-length string object
    function concatenate_string_int_fn(stString1, iValue)	   result(stConcatString)

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
  

  !> Concatenate a variable-length string and an real number.
  !!
  !! @param [in] stString variable-length string object
  !! @param [in] fValue Real (float) data value
  !! @retval stConcatString stString variable-length string object
  !! @ingroup T_STRING
  function concatenate_string_real_fn(stString1, rValue)	   result(stConcatString)

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



  !> Convert a standard Fortran character type to a variable-length string object.
  !!
  !! @param [out] stString variable-length string object
  !! @param [in] sChar Standard Fortran character datatype
  subroutine character_to_string_sub(stString, sChar)

    type (T_STRING), intent(out)    :: stString
    character (len=*), intent(in)   :: sChar

    stString = character_to_string_fn(sChar)

  end subroutine character_to_string_sub
  


  subroutine string_to_string_sub(stStringOut, stStringIn)

    type (T_STRING), intent(inout)  :: stStringOut
    type (T_STRING), intent(in)     :: stStringIn

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iIndex

    if (allocated(stStringOut%sChars) ) &
        deallocate(stStringOut%sChars)

    allocate(stStringOut%sChars( len(stStringIn) ), stat = iStat )

    call assert(iStat == 0, "Problem allocating memory", &
        trim(__FILE__), __LINE__)

    do iIndex = 1, len(stStringIn)
      stStringOut%sChars(iIndex) = stStringIn%sChars(iIndex)
    enddo

  end subroutine string_to_string_sub


  !> Convert a standard Fortran character string to a variable-length string object.
  !!
  !! @param[in] sChar standard Fortran character string
  !! @retval stString variable-length string object
  function character_to_string_fn(sChar)  result(stString)

    character (len=*), intent(in)        :: sChar
    type (T_STRING)                      :: stString
    
    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    if (allocated(stString%sChars))  deallocate(stString%sChars)

    allocate(stString%sChars(len_trim(sChar)))

    do iIndex = 1, len_trim(sChar)
    	stString%sChars(iIndex) = sChar(iIndex:iIndex)
    enddo

  end function character_to_string_fn



  subroutine string_to_character_sub(sChar, stString)

    character (len=*), intent(out)  :: sChar
    class (T_STRING), intent(in)    :: stString

    sChar = convert_to_character_fn(stString)

  end subroutine string_to_character_sub


  !> Convert a variable-length string object to a standard
  !! Fortran character string.
  !!
  !! @param [in] stString variable-length string object
  !! @retval sChar standard Fortran character string
  !! @memberof T_STRING
  function convert_to_character_fn(stString)  result(sChar)

    class (T_STRING), intent(in)         :: stString
    character (len=len(stString))        :: sChar

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    do iIndex = 1,len(stString)
    	sChar(iIndex:iIndex) = stString%sChars(iIndex)
    enddo

  end function convert_to_character_fn


  !> Return the number of characters contained in a 
  !! variable-length string object.
  !!
  !! @param [in] stString variable-length string object
  !! @retval iValue number of characters in this variable-length string object
  function return_length_fn(this)     result(iValue)

    class (T_STRING), intent(in) :: this
    integer (kind=c_int) :: iValue

    if (allocated(this%sChars)) then

      iValue = size(this%sChars,1)

    else

      iValue = 0

    endif 

  end function return_length_fn


  function string_length_fn(stString)     result(iValue)

  	type (T_STRING), intent(in) :: stString
    integer (kind=c_int) :: iValue

    if (allocated(stString%sChars)) then

      iValue = size(stString%sChars,1)

    else

      iValue = 0

    endif	

  end function string_length_fn

  

  function is_string_an_integer_fn(this)   result(lBool)

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


  !> Test whether string is made up of exclusively numeric characters.
  !!
  !! @param[in] this  variable-length string object
  !! @retval lBool returns TRUE or FALSE on the basis of whether
  !!         or not the string is comprised of numeric characters
  !! @implements is_string_numeric_fn
  function is_string_numeric_fn(this)   result(lBool)

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

  subroutine replace_character_sub(this, sFind, sReplace)

    class (T_STRING), intent(inout) :: this
    character (len=1), intent(in)  :: sFind
    character (len=1), intent(in)  :: sReplace

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    if ( len(this) > 0 ) then

      do iIndex = 1, len(this)

        if (this%sChars(iIndex) .eq. sFind) &
          this%sChars(iIndex) = sReplace

      enddo  

    endif  

  end subroutine replace_character_sub  


  function is_logical4_equal_to_logical1_fn(lBool4, lBool1)   result(lBool)

    logical (kind=4), intent(in)      :: lBool4
    logical (kind=1), intent(in)      :: lBool1
    logical (kind=c_bool)              :: lBool

    if (logical(lBool4, kind=c_int) .eqv. logical(lBool1, kind=c_int)) then
      lBool = lTRUE
    else
      lBool = lFALSE
    endif
         
  end function is_logical4_equal_to_logical1_fn  


  function is_logical1_equal_to_logical4_fn(lBool1, lBool4)   result(lBool)

    logical (kind=1), intent(in)      :: lBool1
    logical (kind=4), intent(in)      :: lBool4
    logical (kind=c_bool)              :: lBool

    if (logical(lBool1, kind=c_int) .eqv. logical(lBool4, kind=c_int)) then
      lBool = lTRUE
    else
      lBool = lFALSE
    endif
         
  end function is_logical1_equal_to_logical4_fn  


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

  !> Convert a variable-length string object to a double-precision real.
  !!
  !! @param[in]  this  stString object
  !! @retval    dResult double precision real (float) value
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


  subroutine assert_1bit(lCondition, sMessage, sModule, iLine)

    logical (kind=c_bool), intent(in)           :: lCondition
    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 

    if (.not. lCondition) then

      write (*, fmt="(a)") "Error condition: "//trim(sMessage)

      if (present(sModule))  &
        write (*, fmt="(a)")     "   module: "//trim(sModule)

      if (present(iLine))  &
        write (*, fmt="(a,i7)")  "  line no: ", iLine

      stop

    endif      

  end subroutine assert_1bit


  subroutine assert_4bit(lCondition, sMessage, sModule, iLine)

    logical (kind=4), intent(in)                :: lCondition
    character (len=*), intent(in)               :: sMessage
    character (len=*), intent(in), optional     :: sModule
    integer (kind=c_int), intent(in), optional  :: iLine 

    if (.not. lCondition) then

      write (*, fmt="(a)") "Error condition: "//trim(sMessage)

      if (present(sModule))  &
        write (*, fmt="(a)")     "   module: "//trim(sModule)

      if (present(iLine))  &
        write (*, fmt="(a,i7)")  "  line no: ", iLine

      stop

    endif      

  end subroutine assert_4bit

end module strings