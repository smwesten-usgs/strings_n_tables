module options

  use iso_c_binding, only : c_int, c_float, c_double, c_char
  use strings
  use string_list

  implicit none

  private

  !> data structure to hold kwList, arguments, a help string, and a pointer to the initialization sub 
  type, public :: T_KEYWORD_LIST
    type (T_STRING_LIST)  :: stlKeyword
    type (T_STRING_LIST)  :: stlArguments
    procedure(keySub), pointer :: init_sub => null()
    type (T_STRING)       :: stHelp

  contains
  
    procedure :: addKeyword => add_keyword_sub
    procedure :: addArgument => add_argument_sub
    procedure :: getArguments => get_arguments_fn
    procedure :: initialize => call_initialize_keylist_sub

  end type T_KEYWORD_LIST

  
  type, public :: T_BLOCK
    type (T_STRING_LIST)                :: stlBlockName
  	type (T_KEYWORD_LIST), allocatable  :: kwList(:)
    procedure(blockSub), pointer        :: init_sub => null()
    procedure(checkBlockSub), pointer   :: check_block_sub => null()

  contains
  
    procedure :: addBlockname => add_block_name_sub
    procedure :: getBlocknames => get_block_names_fn
    procedure :: addKeyword => add_keyword_list_sub
    procedure :: initialize => call_initialize_block_sub
    procedure :: checkBlock => call_check_block_sub

  end type T_BLOCK

  abstract interface 
    subroutine blockSub(this)
      import :: T_BLOCK
      class (T_BLOCK) :: this
    end subroutine blockSub
  end interface

  abstract interface 
    subroutine keySub(this)
      import :: T_KEYWORD_LIST
      class (T_KEYWORD_LIST) :: this
    end subroutine keySub
  end interface

  abstract interface
    subroutine checkBlockSub(this)
      import :: T_BLOCK
      class (T_BLOCK) :: this
    end subroutine checkBlockSub
  end interface

contains

  subroutine call_initialize_block_sub(this, proc)

    class (T_BLOCK), intent(inout)  :: this
    procedure(blockSub)             :: proc

    this%init_sub => proc
    call this%init_sub()

  end subroutine call_initialize_block_sub


  subroutine call_initialize_keylist_sub(this, proc)

    class (T_KEYWORD_LIST), intent(inout)  :: this
    procedure(keySub)                      :: proc

    this%init_sub => proc
    call this%init_sub()

  end subroutine call_initialize_keylist_sub



  function process_nextline_sub( stString )   result(stlLine)

    class (T_STRING), intent(in)          :: stString
    type (T_STRING_LIST) :: stlLine

    call stlLine%parse(stString)

  end function process_nextline_sub


  subroutine add_keyword_sub(this, sChar)

    class (T_KEYWORD_LIST), intent(inout) :: this
    character (len=*), intent(in)           :: sChar

    ! [ LOCALS ]
    type (T_STRING) :: stString

    stString = sChar

    call this%stlKeyword%append(stString)

  end subroutine add_keyword_sub


  subroutine add_block_name_sub(this, sChar)

    class (T_BLOCK), intent(inout)          :: this
    character (len=*), intent(in)           :: sChar

    ! [ LOCALS ]
    type (T_STRING) :: stString

    stString = sChar

    call this%stlBlockName%append(stString)

  end subroutine add_block_name_sub



  function get_block_names_fn(this)    result(stlString)
    
    class (T_BLOCK), intent(in)    :: this
    type (T_STRING_LIST)           :: stlString

    stlString = this%stlBlockName  

  end function get_block_names_fn



  subroutine add_argument_sub(this, sChar)

    class (T_KEYWORD_LIST), intent(inout) :: this
    character (len=*), intent(in)         :: sChar

    ! [ LOCALS ]
    type (T_STRING) :: stString

    stString = sChar

    call this%stlArguments%append(stString)

  end subroutine add_argument_sub


  function get_arguments_fn(this)    result(stlString)

    class (T_KEYWORD_LIST), intent(in)    :: this
    type (T_STRING_LIST)                  :: stlString

    stlString = this%stlArguments

  end function get_arguments_fn

  
  subroutine add_keyword_list_sub(this, kwList)

    class (T_BLOCK), intent(inout)    :: this
    type (T_KEYWORD_LIST), intent(in) :: kwList

    ! [ LOCALS ]
    integer (kind=c_int) :: iCount
    type (T_KEYWORD_LIST), allocatable :: kwTempList(:)

    iCount = ubound(this%kwList,1)

    allocate(kwTempList(iCount + 1))

    kwTempList(1:iCount) = this%kwList(1:iCount)
    kwTempList(iCount + 1) = kwList
    this%kwList = kwTempList

  end subroutine add_keyword_list_sub

end module options