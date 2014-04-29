module keywords

  use iso_c_binding, only : c_int, c_float, c_double, c_char
  use strings
  use string_list

  implicit none

  ! integer (kind=c_int), parameter :: INTEGER_DATA = 1
  ! integer (kind=c_int), parameter :: FLOAT_DATA = 2
  ! integer (kind=c_int), parameter :: DOUBLE_DATA = 3
  ! integer (kind=c_int), parameter :: T_STRING_DATA = 4
  ! integer (kind=c_int), parameter :: T_DATETIME_DATA = 5
  ! integer (kind=c_int), parameter :: T_DATE_DATA = 6
  ! integer (kind=c_int), parameter :: T_TIME_DATA = 7
  
  !> data structure to hold kwList, values, a help string, and a pointer to the initialization sub 
  type, public :: T_KEYWORDS
    type (STRING_LIST_T)               :: stlKeywords
    type (STRING_LIST_T)               :: stlValues
    procedure(keySub), pointer         :: init_sub => null()
    character (len=:), allocatable     :: stHelp

  contains
  
    procedure :: addKeyPair => add_keyword_value_pair_sub
    procedure :: addKeyword => add_keyword_sub
    procedure :: addValue => add_value_sub
!    procedure :: setValues => set_values_sub
    procedure :: getValues => get_values_fn
    procedure :: initialize => call_initialize_keylist_sub

  end type T_KEYWORDS

  
  abstract interface 
    subroutine keySub(this)
      import :: T_KEYWORDS
      class (T_KEYWORDS) :: this
    end subroutine keySub
  end interface

contains

  subroutine call_initialize_keylist_sub(this, proc)

    class (T_KEYWORDS), intent(inout)  :: this
    procedure(keySub)                  :: proc

    this%init_sub => proc
    call this%init_sub()

  end subroutine call_initialize_keylist_sub



  subroutine add_keyword_sub(this, sChar)

    class (T_KEYWORDS), intent(inout) :: this
    character (len=*), intent(in)     :: sChar

    call this%stlKeywords%append(sChar)

  end subroutine add_keyword_sub



  subroutine add_value_sub(this, sChar)

    class (T_KEYWORDS), intent(inout) :: this
    character (len=*), intent(in)     :: sChar

    call this%stlValues%append(sChar)

  end subroutine add_value_sub


  subroutine add_keyword_value_pair_sub(this, sKeyword, sArgument)

    class (T_KEYWORDS), intent(inout)    :: this
    character (len=*), intent(in)        :: sKeyword
    character (len=*), intent(in)        :: sArgument

    call this%stlKeyWords%append(sKeyword)
    call this%stlValues%append(sArgument)

  end subroutine add_keyword_value_pair_sub


  function get_values_fn(this)    result(stlString)

    class (T_KEYWORDS), intent(in)    :: this
    type (STRING_LIST_T)              :: stlString

    stlString = this%stlValues

  end function get_values_fn

end module keywords