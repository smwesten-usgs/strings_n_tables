module data_frame

use iso_c_binding, only : c_int, c_float, c_double, c_bool
use iso_fortran_env, only : IOSTAT_END
use strings
implicit none

private

character (len=2), parameter :: sWHITESPACE = achar(9)//" "
character (len=1), parameter :: sBACKSLASH = achar(92)
character (len=1), parameter :: sFORWARDSLASH = achar(47)
character (len=1), parameter :: sRETURN = achar(13)

integer (kind=c_int), parameter :: INTEGER_DATA = 1
integer (kind=c_int), parameter :: FLOAT_DATA = 2
integer (kind=c_int), parameter :: DOUBLE_DATA = 3
integer (kind=c_int), parameter :: T_STRING_DATA = 4

logical (kind=c_bool), parameter :: lFALSE = .false.
logical (kind=c_bool), parameter :: lTRUE = .true.

public :: T_DATA_FRAME, T_DATA_COLUMN, T_DATA_FILE

type T_DATA_COLUMN

  type (T_STRING) :: stColumnName
  integer (kind=c_int) :: iDataType
  integer (kind=c_int) :: iIndex
  integer (kind=c_int) :: iCount
  integer (kind=c_int), dimension(:), allocatable :: iData
  real (kind=c_float), dimension(:), allocatable :: rData
  real (kind=c_double), dimension(:), allocatable :: dData
  character (len=MAXCOLWIDTH), dimension(:), allocatable :: sData

contains

!	procedure :: new => create_new_column_sub
!	procedure :: sum => sum_of_column_elements_fn
!	procedure :: mean => mean_of_column_elements_fn
!	procedure :: min => minimum_of_column_elements_fn
!	procedure :: max => maximum_of_column_elements_fn
!	procedure :: median => median_of_column_elements_fn

end type T_DATA_COLUMN


type T_DATA_FRAME

  type (T_DATA_COLUMN), dimension(:), allocatable :: tCol
  

end type T_DATA_FRAME  


type T_DATA_FILE

  type (T_STRING)        :: stFilename
  type (T_STRING)        :: stDelimiters
  logical (kind=c_bool)  :: lIsOpen = lFALSE  
  integer (kind=c_int)   :: iUnitNum
  integer (kind=c_int)   :: iStat
  type (T_STRING)        :: stMissingValue

contains
 
  procedure :: openFile => open_file_sub
  procedure :: closeFile => close_file_sub
  procedure :: isOpen => is_file_open_fn
  procedure :: exists => does_file_exist_fn
!  procedure :: readHeader => read_header_sub
!  procedure :: readHeaderMultiline => read_multiline_header_sub
!  procedure :: numRows => return_num_rows_fn
!  procedure :: numCols => return_num_cols_fn
!  procedure :: countRows => count_rows_sub
!  procedure :: countColumns => cound_columns_sub  
  procedure :: getRow => get_row_of_data_sub
!  procedure :: putRow => put_row_of_data_sub
!  procedure :: getNext => get_next_data_item_fn

end type T_DATA_FILE


contains

  

  subroutine open_file_sub(this, stFilename)
 
    class (T_DATA_FILE) :: this
  	type (T_STRING) :: stFilename

    if (.not. this%isOpen(stFilename ) ) then

      open(newunit=this%iUnitNum, file=stFilename%asCharacter(), iostat=this%iStat)
      if (this%iStat == 0)  this%lIsOpen = lTRUE
    endif  

  end subroutine open_file_sub
  

  subroutine close_file_sub(this)

    class (T_DATA_FILE) :: this

    if ( this%isOpen(this%stFilename ) ) &
      close(unit=this%iUnitNum, iostat=this%iStat)

  end subroutine close_file_sub  


  function does_file_exist_fn(this, stFilename)    result(lExists)

    class (T_DATA_FILE) :: this
    type (T_STRING) :: stFilename
    logical(kind=c_bool) :: lExists

    inquire(file=stFilename%asCharacter(), exist=lExists)

  end function does_file_exist_fn



  function is_file_open_fn(this, stFilename)    result(lOpened)

    class (T_DATA_FILE)           :: this
    type (T_STRING), intent(in)   :: stFilename
    logical(kind=c_bool)          :: lOpened

    inquire(file=stFilename%asCharacter(), opened=lOpened)

  end function is_file_open_fn	


  function get_row_of_data_sub(this)   result(stString)

    class (T_DATA_FILE), intent(in)            :: this
    type (T_STRING)                            :: stString

    ! [ LOCALS ] 
    integer (kind=c_int) :: iStat
    character (len=:), allocatable :: sChar
  
    sChar = ""

    if (this%lIsOpen) then

      read (unit = this%iUnitNum, fmt = *, iostat = iStat) sChar
      stString = trim(sChar)

    endif  

  end function get_row_of_data_sub  


  subroutine create_new_column_sub(this, sColumnName, iDataType, iCount)

    class (T_DATA_COLUMN)               :: this
    character (len=*), intent(in)       :: sColumnName
    integer (kind=c_int), intent(in)    :: iDataType
    integer (kind=c_int),intent(in)     :: iCount

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    select case (iDataType)

      case (INTEGER_DATA)

        allocate( this%iData(iCount), stat=iStat )

      case (FLOAT_DATA)

        allocate( this%rData(iCount), stat=iStat )

      case (DOUBLE_DATA)

        allocate( this%dData(iCount), stat=iStat )

      case (T_STRING_DATA)

        allocate( this%sData(iCount), stat=iStat )

      case default

        call assert(lFALSE, "Unhandled case select value", &
        	             trim(__FILE__), __LINE__)


    end select  

  end subroutine create_new_column_sub	


end module data_frame