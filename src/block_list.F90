module block_list

  use iso_c_binding, only : c_int, c_float, c_double, c_char
  use strings
  use string_list
  use keyword_list
  use block
  use exceptions

  implicit none

  private
  
  type, public :: T_BLOCK_LIST
    type (T_STRING_LIST)                :: stlBlockNames
    type (T_BLOCK), allocatable         :: blkBlock(:)

  contains
  
    procedure :: addBlock => add_block_sub
    procedure :: getBlockNames => get_blocknames_fn
    procedure :: getBlock => get_block_fn

  end type T_BLOCK_LIST

contains

  
  subroutine add_block_sub(this, blkBlock)

    class (T_BLOCK_LIST), intent(inout)    :: this
    type (T_BLOCK), intent(in)             :: blkBlock

    ! [ LOCALS ]
    integer (kind=c_int) :: iCount
    type (T_BLOCK_LIST)  :: blkTempList

    iCount = ubound(this%blkBlock,1)

    allocate(blkTempList%blkBlock(iCount + 1))

    blkTempList%blkBlock(1:iCount) = this%blkBlock(1:iCount)
    blkTempList%blkBlock(iCount + 1) = blkBlock
    this%blkBlock = blkTempList%blkBlock

    call this%stlBlockNames%append( blkBlock%stlBlockName%value(1) )

  end subroutine add_block_sub

  
  function get_block_fn( this, sBlockname )   result(blkBlock)

    class (T_BLOCK_LIST), intent(in)   :: this
    character (len=*), intent(in)      :: sBlockname
    type (T_BLOCK), allocatable        :: blkBlock

    ! [ LOCALS ]
    integer (kind=c_int), allocatable :: iIndex(:)

    iIndex = this%stlBlocknames%which(sBlockname)

    if (ubound(iIndex,1) > 1)  &
       call die("Nonunique blocknames were used in code. Blockname = "//sBlockname, &
        __FILE__, __LINE__)

    blkBlock = this%blkBlock(iIndex(1))

  end function get_block_fn


  function get_blocknames_fn(this)     result(stlBlockNames)

    class (T_BLOCK_LIST), intent(inout)    :: this
    type (T_STRING_LIST)                   :: stlBlockNames

    stlBlockNames = this%stlBlockNames



  end function get_blocknames_fn

end module block_list