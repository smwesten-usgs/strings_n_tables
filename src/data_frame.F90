module data_frame

use iso_c_binding, only : c_int, c_float, c_double, c_bool
implicit none

private

character (len=2), parameter :: sWHITESPACE = achar(9)//" "
character (len=1), parameter :: sBACKSLASH = achar(92)
character (len=1), parameter :: sFORWARDSLASH = achar(47)
character (len=1), parameter :: sRETURN = achar(13)

integer (kind=c_int), parameter :: MAXCOLWIDTH = 15

integer (kind=c_int), parameter :: INTEGER_DATA = 1
integer (kind=c_int), parameter :: FLOAT_DATA = 2
integer (kind=c_int), parameter :: DOUBLE_DATA = 3
integer (kind=c_int), parameter :: CHARACTER_DATA = 4

public :: T_DATA_FRAME, T_DATA_COLUMN, T_DATA_FILE

type T_DATA_COLUMN

  character (len=1), dimension(:), allocatable :: sColumnName
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


  

end type T_DATA_FRAME  


type T_DATA_FILE

  character (len=1), dimension(:), allocatable :: sFilename
  character (len=1),dimension(:), allocatable :: sDelimiters
  integer (kind=c_int) :: iUnitNum
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iNumRows
  integer (kind=c_int) :: iNumCols
  character (len=1), dimension(:), allocatable :: sMissingValue

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
!  procedure :: getRow => get_row_of_data_sub
!  procedure :: putRow => put_row_of_data_sub
!  procedure :: getNext => get_next_data_item_fn

end type T_DATA_FILE


contains

  

  subroutine open_file_sub(this, sFilename)
 
    class (T_DATA_FILE) :: this
  	character(len=*) :: sFilename

    if (.not. this%isOpen(sFilename) ) &

      open(newunit=this%iUnitNum, file=sFilename, iostat=this%iStat)

  end subroutine open_file_sub
  

  subroutine close_file_sub(this)

    class (T_DATA_FILE) :: this

    if ( this%isOpen(this%sFilename) ) &
      close(unit=this%iUnitNum, iostat=this%iStat)

  end subroutine close_file_sub  


  function does_file_exist_fn(this, sFilename)    result(lExists)

    class (T_DATA_FILE) :: this
    character(len=*), intent(in) :: sFilename
    logical(kind=c_bool) :: lExists

    inquire(file=sFilename, exist=lExists)

  end function does_file_exist_fn



  function is_file_open_fn(this, sFilename)    result(lOpened)

    class (T_DATA_FILE) :: this
    character(len=*), intent(in) :: sFilename
    logical(kind=c_bool) :: lOpened

    inquire(file=sFilename, opened=lOpened)

  end function is_file_open_fn	



  subroutine chomp(sRecord, sItem, sDelimiters)

    ! ARGUMENTS
    character (len=*), intent(inout)                 :: sRecord
    character (len=256), intent(out)                 :: sItem
    character (len=*), intent(in)                    :: sDelimiters
    ! LOCALS
    integer (kind=c_int) :: iR                      ! Index in sRecord
    integer (kind=c_int) :: iB                      !
    integer (kind=c_int) :: iLen

    iB = 0

    ! eliminate any leading spaces
    sRecord = adjustl(sRecord)
    ! find the end position of 'sRecord'
    iLen = len_trim(sRecord)

    ! find the POSITION of the first delimiter found
    iR = SCAN(trim(sRecord),sDelimiters)

    if(iR==0) then
      sItem = trim(sRecord)   ! no delimiters found; return entirety of sRecord
      sRecord = ""            ! as sItem
    else
      sItem = trim(sRecord(1:iR-1))
      sRecord = trim( adjustl(sRecord(iR+1:)) )
    end if

  end subroutine chomp


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

      case (CHARACTER_DATA)

        allocate( this%sData(iCount), stat=iStat )

      case default

        call fullstop ("Unhandled case select value", &
        	             trim(__FILE__), __LINE__)


    end select  

  end subroutine create_new_column_sub	


  subroutine fullstop(sMessage, sFilename, iLineNum)

    character (len=*) :: sMessage
    character (len=*) :: sFilename
    integer (kind=c_int) :: iLineNum

    ! [ LOCALS ]
    character (len=1), dimension(:), allocatable :: sMessageText

    write(sMessageText, fmt="(a,' [',a,', line number: ',i6,']')") &
    	trim(sMessage), trim(sFilename), iLineNum

    write(*, fmt="(a)") sMessageText

    stop

  end subroutine fullstop


end module data_frame