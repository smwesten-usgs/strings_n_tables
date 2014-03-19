module block

  use iso_c_binding, only : c_int, c_float, c_double, c_char
  use strings
  use string_list
  use keyword_list

  implicit none

  private
  
  type, public :: T_BLOCK
    type (T_STRING_LIST)                :: stlBlockName
  	type (T_KEYWORD_LIST), allocatable  :: kwList(:)
    procedure(blockSub), pointer        :: init_sub => null()
    procedure(checkBlockSub), pointer   :: check_block_sub => null()

  contains
  
    procedure :: addBlockname => add_block_name_sub
    procedure :: getBlocknames => get_block_names_fn
    procedure :: getBlockName => get_block_name_fn
    procedure :: addKeywords => add_keyword_list_sub
    procedure :: initialize => call_initialize_block_sub
    procedure :: checkBlock => call_check_block_sub
    procedure :: printBlock => print_block_sub

  end type T_BLOCK

  abstract interface 
    subroutine blockSub(this)
      import :: T_BLOCK
      class (T_BLOCK) :: this
    end subroutine blockSub
  end interface

  abstract interface
    subroutine checkBlockSub(this)
      import :: T_BLOCK
      class (T_BLOCK) :: this
    end subroutine checkBlockSub
  end interface

contains

  !> Consume an entire Fortran character record and assign
  !! values to the keys found in the keyword list.
  !!
  !! @param[inout] this Object of class T_BLOCK
  !! @param[in] sRecord Record to be parsed and converted to key/value pairs
  subroutine populate_block_by_keyword_sub(this, sRecord)

    class (T_BLOCK), intent(inout)     :: this
    character (len=*), intent(in)      :: sRecord

    ! [ LOCALS ]
    type (T_STRING) :: stString

    stString = sRecord
    

  end subroutine populate_block_by_keyword_sub
    

  subroutine print_block_sub(this)

    class (T_BLOCK), intent(in) :: this

    ! [ LOCALS ]
    type (T_STRING)      :: stBlockname
    integer (kind=c_int) :: iIndex, iIndex2
    integer (kind=c_int) :: iCount
    character (len=:), allocatable :: sBuf1, sBuf2
    type (T_STRING)      :: stString1, stString2

    stBlockname = this%getBlockName()
    write(*, fmt="(/,a)") "BEGIN "//stBlockname%asCharacter()

    do iIndex = 1, ubound(this%kwList,1)
      ! OK hold on...we are attempting to iterate over the
      ! keywords in the iIndex-th member of kwList
      iCount = this%kwList(iIndex)%stlKeyword%count()

      associate (keypair => this%kwList(iIndex) )
      
        do iIndex2 = 1, iCount

          stString1 = keypair%stlKeyword%value(iIndex2)
          stString2 = keypair%stlArguments%value(iIndex2)

          write(*, fmt="(a, '=', a)") stString1%asCharacter(), stString2%asCharacter()

        enddo
    
      end associate

    enddo    

    write(*, fmt="(a,/)") "END "//stBlockname%asCharacter()


  end subroutine print_block_sub
    

  subroutine call_initialize_block_sub(this, proc)

    class (T_BLOCK), intent(inout)  :: this
    procedure(blockSub)             :: proc

    this%init_sub => proc
    call this%init_sub()

  end subroutine call_initialize_block_sub


  subroutine call_check_block_sub(this, proc)

    class (T_BLOCK), intent(inout)  :: this
    procedure(checkBlockSub)        :: proc

    this%check_block_sub => proc
    call this%check_block_sub()

  end subroutine call_check_block_sub



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


  function get_block_name_fn(this)   result(stBlockname)

    class (T_BLOCK), intent(in)    :: this
    type (T_STRING) :: stBlockname

    ! [ LOCALS ]
    type (T_STRING_LIST) :: stlString

    stlString = this%stlBlockname
    stBlockname = stlString%value(1)
  
  end function get_block_name_fn

  
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

end module block