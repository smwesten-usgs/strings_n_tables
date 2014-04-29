module keywords

  use iso_c_binding, only : c_int, c_float, c_double, c_char
  use strings
  use string_list

  implicit none

  
  !> data structure to hold keywords, values, a help string, and a pointer to the initialization sub 
  type, public :: KEYWORDS_T
    type (STRING_LIST_T)                    :: stlKeywords
    type (STRING_LIST_T)                    :: stlValues
    procedure(keySub), pointer              :: init_sub => null()
    character (len=:), allocatable          :: stHelp

  contains
  
    procedure :: addKeyPair => add_keyword_value_pair_sub
    procedure :: addKeyword => add_keyword_sub
    procedure :: addValue => add_value_sub

    procedure, private :: set_values_int_sub
    procedure, private :: set_values_float_sub
    procedure, private :: set_values_double_sub
    procedure, private :: set_values_string_sub

    generic, public :: setValues => set_values_int_sub, &
                                    set_values_float_sub, &
                                    set_values_double_sub, &
                                    set_values_string_sub

    procedure :: getValues => get_values_fn
    procedure :: initialize => call_initialize_keylist_sub

  end type KEYWORDS_T

  
  abstract interface 
    subroutine keySub(this)
      import :: KEYWORDS_T
      class (KEYWORDS_T) :: this
    end subroutine keySub
  end interface

contains

  subroutine call_initialize_keylist_sub(this, proc)

    class (KEYWORDS_T), intent(inout)  :: this
    procedure(keySub)                  :: proc

    this%init_sub => proc
    call this%init_sub()

  end subroutine call_initialize_keylist_sub

  !----------------------------------------------------------------------------

  subroutine add_keyword_sub(this, sChar)

    class (KEYWORDS_T), intent(inout) :: this
    character (len=*), intent(in)     :: sChar

    call this%stlKeywords%append(sChar)

  end subroutine add_keyword_sub

  !----------------------------------------------------------------------------

  subroutine add_value_sub(this, sChar)

    class (KEYWORDS_T), intent(inout) :: this
    character (len=*), intent(in)     :: sChar

    call this%stlValues%append(sChar)

  end subroutine add_value_sub

  !----------------------------------------------------------------------------

  subroutine set_values_int_sub(this, iValues)    

    class (KEYWORDS_T), intent(inout) :: this
    integer (kind=c_int)              :: iValues(:)

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    call this%stlValues%deallocate()

    do iIndex = 1, ubound(iValues,1)

      call this%stlValues%append(iValues(iIndex))

    enddo 

  end subroutine set_values_int_sub

  !----------------------------------------------------------------------------

  subroutine set_values_float_sub(this)

    class (KEYWORDS_T), intent(inout) :: this

  end subroutine set_values_float_sub

  !----------------------------------------------------------------------------

  subroutine set_values_double_sub(this)

    class (KEYWORDS_T), intent(inout) :: this

  end subroutine set_values_double_sub

  !----------------------------------------------------------------------------

  subroutine set_values_string_sub(this)

    class (KEYWORDS_T), intent(inout) :: this

  end subroutine set_values_string_sub

  !----------------------------------------------------------------------------

  subroutine add_keyword_value_pair_sub(this, sKeyword, sArgument)

    class (KEYWORDS_T), intent(inout)    :: this
    character (len=*), intent(in)        :: sKeyword
    character (len=*), intent(in)        :: sArgument

    call this%stlKeyWords%append(sKeyword)
    call this%stlValues%append(sArgument)

  end subroutine add_keyword_value_pair_sub

  !----------------------------------------------------------------------------

  function get_values_fn(this)    result(stlString)

    class (KEYWORDS_T), intent(in)    :: this
    type (STRING_LIST_T)              :: stlString

    stlString = this%stlValues

  end function get_values_fn

  !----------------------------------------------------------------------------  

end module keywords