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

character (len=8192) :: sBuf

public :: FLOAT_DATA, INTEGER_DATA, DOUBLE_DATA
public :: T_DATA_FRAME, T_DATA_COLUMN, T_DATA_FILE

type  T_DATA_COLUMN

  integer (kind=c_int) :: iCurrentRecord = 0
  integer (kind=c_int) :: iOrder
  integer (kind=c_int) :: iDataType
  integer (kind=c_int) :: iCount
  integer (kind=c_int), dimension(:), allocatable :: iData
  real (kind=c_float), dimension(:), allocatable :: fData
  real (kind=c_double), dimension(:), allocatable :: dData
  logical (kind=c_bool), dimension(:), allocatable :: lMask
  type (T_STRING_LIST) :: stData

contains

	procedure :: new => create_new_column_sub
	procedure :: sum => sum_of_column_elements_fn
!	procedure :: mean => mean_of_column_elements_fn
!	procedure :: min => minimum_of_column_elements_fn
!	procedure :: max => maximum_of_column_elements_fn
!	procedure :: median => median_of_column_elements_fn

end type T_DATA_COLUMN







type T_DATA_FRAME

  integer (kind=c_int), dimension(:), allocatable        :: iDataTypes
  type (T_STRING_LIST)                                   :: stColNames
  type (T_DATA_COLUMN), dimension(:), allocatable        :: col
  integer (kind=c_int)                                   :: iCount  

contains

  ! procedure :: makeUnique => make_unique_identifier_sub
  !> take contents of serveral columns and concatenate them to 
  !> create a unique ID (think METALICUS)

  procedure :: initialize_data_frame_sub
  generic :: initialize => initialize_data_frame_sub

  procedure :: populate_data_frame_by_row_sub
  generic :: rowvals => populate_data_frame_by_row_sub

  procedure :: summarize_data_frame_sub
  generic :: summarize => summarize_data_frame_sub


end type T_DATA_FRAME  


type T_DATA_FILE

  type (T_STRING)        :: stFilename
  type (T_STRING)        :: stDelimiters
  integer (kind=c_int)   :: iCurrentRecord = 0
  integer (kind=c_int)   :: iRecordCount = 0
  logical (kind=c_bool)  :: lIsOpen = lFALSE  
  integer (kind=c_int)   :: iUnitNum
  integer (kind=c_int)   :: iStat
  type (T_STRING)        :: stMissingValue

contains
 
  procedure :: open_file_string_sub
  procedure :: open_file_char_sub
  generic :: openFile => open_file_string_sub, &
                         open_file_char_sub

  procedure :: closeFile => close_file_sub
  procedure :: isOpen => is_file_open_fn
  procedure :: exists => does_file_exist_fn
  procedure :: readHeader => read_header_fn
  procedure :: countLines => count_number_of_lines_sub

!  procedure :: readHeaderMultiline => read_multiline_header_sub
!  procedure :: numRows => return_num_rows_fn
!  procedure :: numCols => return_num_cols_fn
!  procedure :: countRows => count_rows_sub
!  procedure :: countColumns => cound_columns_sub  
  procedure :: getRow => get_row_of_data_sub
!  procedure :: putRow => put_row_of_data_sub
!  procedure :: getNext => get_next_data_item_fn

  procedure :: deallocate => deallocate_strings_sub

end type T_DATA_FILE


contains

  function sum_of_column_elements_fn(this)  result(dSum)

    class(T_DATA_COLUMN), intent(in)       :: this
    real (kind=c_double)                   :: dSum

    select case (this%iDataType)

      case (INTEGER_DATA)

        dSum = sum(this%iData, this%lMask)

      case (FLOAT_DATA)
      
        dSum = sum(this%fData, this%lMask)

      case (DOUBLE_DATA)
      
        dSum = sum(this%dData, this%lMask)

      case default
      
        dSum = -9999.      

    end select

  end function sum_of_column_elements_fn




  subroutine initialize_data_frame_sub(this, stColNames, iDataTypes, iRecordCount)

    class (T_DATA_FRAME), intent(inout)               :: this
    type (T_STRING_LIST), intent(in)               :: stColNames
    integer (kind=c_int), dimension(:), intent(in) :: iDataTypes
    integer (kind=c_int), intent(in)               :: iRecordCount

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iIndex
    character (len=64) :: sChar
    type (T_STRING) :: stString

    this%stColNames = stColNames
    this%iDataTypes = iDataTypes

    allocate( this%col(this%stColNames%count() ) )

    this%iCount = this%stColNames%count()

    print *, "Number of columns: ", this%iCount

    do iIndex = 1, this%iCount

      !> create a new column space for each item detected in the header
      call this%col(iIndex)%new(iDataType = this%iDataTypes(iIndex), &
                                iCount = iRecordCount)

      stString = this%stColNames%value(iIndex)

      sChar = stString%asCharacter()
      print *, "  Creating new column for "//trim(sChar), &
               "  with room for ", iRecordCount," values."

    enddo  

  end subroutine initialize_data_frame_sub



  subroutine deallocate_strings_sub(this)

    class (T_DATA_FILE), intent(inout) :: this

    call this%stFilename%deallocate()
    call this%stDelimiters%deallocate()
    call this%stMissingValue%deallocate()

  end subroutine deallocate_strings_sub
  


  subroutine open_file_string_sub(this, stFilename)
 
    class (T_DATA_FILE), intent(inout) :: this
  	type (T_STRING), intent(in)     :: stFilename

    if (.not. this%isOpen(stFilename ) ) then

      open(newunit=this%iUnitNum, file=stFilename%asCharacter(), iostat=this%iStat)
      call assert(this%iStat == 0, "Failed to open file.", __FILE__, __LINE__)

      if (this%iStat == 0)  this%lIsOpen = lTRUE

      call this%countLines()

      open(newunit=this%iUnitNum, file=stFilename%asCharacter(), iostat=this%iStat)
      call assert(this%iStat == 0, "Failed to open file.", __FILE__, __LINE__)

      if (this%iStat == 0)  this%lIsOpen = lTRUE

      write(*, fmt="(a, i8, a)") "Opened file. ", this%iRecordCount, " records present."

    else

      print *, "Failed to open file..."

    endif  

  end subroutine open_file_string_sub
  

  subroutine open_file_char_sub(this, sFilename)
 
    class (T_DATA_FILE), intent(inout) :: this
    character (len=*), intent(in)   :: sFilename

    ! [ LOCALS ]
    type (T_STRING) :: stString

    stString = sFilename  

    if (.not. this%isOpen(stString) ) then

      open(newunit=this%iUnitNum, file=sFilename, iostat=this%iStat)
      call assert(this%iStat == 0, "Failed to open file.", __FILE__, __LINE__)
      
      if (this%iStat == 0)  this%lIsOpen = lTRUE

      call this%countLines()

      open(newunit=this%iUnitNum, file=sFilename, iostat=this%iStat)
      call assert(this%iStat == 0, "Failed to open file.", __FILE__, __LINE__)

      if (this%iStat == 0)  this%lIsOpen = lTRUE

      write(*, fmt="(a, i8, a)") "Opened file. ", this%iRecordCount, " records present."

    else

      print *, "Failed to open file..."

    endif  

  end subroutine open_file_char_sub


  subroutine close_file_sub(this)

    class (T_DATA_FILE) :: this

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


  function read_header_fn(this)   result (stList)

    class (T_DATA_FILE), intent(inout) :: this
    type (T_STRING_LIST)            :: stList

    ! [ LOCALS ] 
    type (T_STRING) :: stString
    type (T_STRING) :: stSubString  

    stString = this%getRow() 

    this%iCurrentRecord = this%iCurrentRecord + 1
    this%iRecordCOunt = this%iRecordCount - 1

    do while ( stString%length() > 0)

      stSubString = stString%chomp(",")
      call stSubString%replace(" ", "_")
      call stList%append(stSubString)

    enddo       

  end function read_header_fn



  subroutine count_number_of_lines_sub(this)

    class (T_DATA_FILE), intent(inout)   :: this

    ! [ LOCALS ] 
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iResult

    iResult = 0
    iStat = 0

    if (this%lIsOpen) then

      do 

        read (unit = this%iUnitNum, fmt=*, iostat = iStat) 
        iResult = iResult + 1

        if (iStat == IOSTAT_END) exit

      enddo

      call this%closeFile()
      this%lIsOpen = lFALSE

      this%iRecordCount = iResult

    endif  

  end subroutine count_number_of_lines_sub




  function get_row_of_data_sub(this)   result(stString)

    class (T_DATA_FILE), intent(inout)         :: this
    type (T_STRING)                            :: stString

    ! [ LOCALS ] 
    integer (kind=c_int) :: iStat

    if (this%lIsOpen) then

      read (unit = this%iUnitNum, fmt = "(a)", iostat = iStat) sBuf
      stString = trim(sBuf)

      this%iCurrentRecord = this%iCurrentRecord + 1

      if (iStat == IOSTAT_END) then 
        call this%closeFile()
        this%lIsOpen = lFALSE
      endif  

    endif  

  end function get_row_of_data_sub  

  


  subroutine create_new_column_sub(this, iDataType, iCount)

    class (T_DATA_COLUMN)               :: this
    integer (kind=c_int), intent(in)    :: iDataType
    integer (kind=c_int),intent(in)     :: iCount

    ! [ LOCALS ] 
    integer (kind=c_int) :: iStat

    this%iDataType = iDataType

    allocate( this%lMask(iCount), stat = iStat)
    this%lMask = lTRUE

    select case(iDataType)

      case (INTEGER_DATA)

        allocate( this%iData(iCount), stat = iStat )

      case (FLOAT_DATA)

        allocate( this%fData(iCount), stat = iStat)  

      case (DOUBLE_DATA)

        allocate( this%dData(iCount), stat = iStat)  

      case (T_STRING_DATA)  

        iStat = 0

      case default

        call assert(lFALSE, "Internal programming error", &
            __FILE__, __LINE__)

    end select

    call assert( iStat == 0, "Failed to allocate memory while creating a new column", &
        __FILE__, __LINE__)

  end subroutine create_new_column_sub  


  subroutine populate_data_frame_by_row_sub( this, stString , sDelimiters)

    class (T_DATA_FRAME), intent(inout)    :: this
    type (T_STRING), intent(inout)         :: stString
    character (len=*), intent(in)          :: sDelimiters

    ! [ LOCALS ] 
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum
    type (T_STRING)      :: stSubString

    iIndex = 0

    do while (stString%length() > 0)

      stSubString = stString%chomp(sDelimiters)

      iIndex = iIndex + 1

      this%col(iIndex)%iCurrentRecord = this%col(iIndex)%iCurrentRecord + 1

      iRowNum = this%col(iIndex)%iCurrentRecord

      select case (this%col(iIndex)%iDataType )
 
        case (INTEGER_DATA)


          this%col(iIndex)%iData(iRowNum) = stSubString%asInt()

        case (FLOAT_DATA)

          this%col(iIndex)%fData(iRowNum) = stSubString%asFloat()

        case (DOUBLE_DATA)

          this%col(iIndex)%dData(iRowNum) = stSubString%asDouble()

        case (T_STRING_DATA)

      end select

    enddo  


  end subroutine populate_data_frame_by_row_sub  





  subroutine summarize_data_frame_sub( this )

    class (T_DATA_FRAME), intent(inout)    :: this

    ! [ LOCALS ] 
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum
    type (T_STRING)      :: stSubString

    ! [ LOCALS ]
    character (len=64) :: sChar
    type (T_STRING) :: stString

    do iIndex = 1, this%iCount

      stString = this%stColNames%value(iIndex)

      sChar = stString%asCharacter()

      write(*, "(/,a)") "Variable name: "//trim(sChar)

      select case (this%iDataTypes(iIndex) )
 
        case (INTEGER_DATA)

           write (*, fmt="(5x, a, i8)") "Count: ", size(this%col(iIndex)%iData,1)
           write (*, fmt="(7x, a, i8)") "Min: ", minval(this%col(iIndex)%iData)           
           write (*, fmt="(7x, a, i8)") "Max: ", maxval(this%col(iIndex)%iData)                      



        case (FLOAT_DATA)

           write (*, fmt="(5x, a, i8)") "Count: ", size(this%col(iIndex)%fData,1)
           write (*, fmt="(7x, a, g15.5)") "Min: ", minval(this%col(iIndex)%fData)           
           write (*, fmt="(7x, a, g15.5)") "Max: ", maxval(this%col(iIndex)%fData) 
           write (*, fmt="(7x, a, g15.5)") "Sum: ", this%col(iIndex)%sum()                     

        case (DOUBLE_DATA)

           write (*, fmt="(5x, a, i8)") "Count: ", size(this%col(iIndex)%dData,1)
           write (*, fmt="(7x, a, g16.7)") "Min: ", minval(this%col(iIndex)%dData)           
           write (*, fmt="(7x, a, g16.7)") "Max: ", maxval(this%col(iIndex)%dData)                      

        case (T_STRING_DATA)

      end select

    enddo  


  end subroutine summarize_data_frame_sub


!   subroutine create_new_column_sub(this, sColumnName, iCount)

!     class (T_DATA_COLUMN)               :: this
!     character (len=*), intent(in)       :: sColumnName
!     integer (kind=c_int),intent(in)     :: iCount

!     ! [ LOCALS ]
!     integer (kind=c_int) :: iStat

!     select type (this)

!       class is (T_DATA_COLUMN_INT)

!         allocate( this%iData(iCount), stat=iStat )

!       class is (T_DATA_COLUMN_FLOAT)

!         allocate( this%rData(iCount), stat=iStat )

!       class default

!         call assert(lFALSE, "Unhandled case select value", &
!         	             trim(__FILE__), __LINE__)

!     end select  

!   end subroutine create_new_column_sub	


end module data_frame