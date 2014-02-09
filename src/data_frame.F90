module data_frame

use iso_c_binding, only : c_int, c_float, c_double, c_bool
use iso_fortran_env, only : IOSTAT_END
use strings
use types
implicit none

private

public :: FLOAT_DATA, INTEGER_DATA, DOUBLE_DATA
public :: T_DATA_FRAME, T_DATA_COLUMN

type T_DATA_COLUMN

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

procedure, private :: create_new_column_sub
generic, public    :: new => create_new_column_sub

procedure, private :: sum_of_column_elements_fn
generic, public    :: sum => sum_of_column_elements_fn


procedure, private :: min_of_column_elements_fn
generic, public    :: min => min_of_column_elements_fn

procedure, private :: max_of_column_elements_fn
generic, public    :: max => max_of_column_elements_fn

procedure, private :: count_of_column_elements_fn
generic, public    :: count => count_of_column_elements_fn

procedure, private :: mean_of_column_elements_fn
generic, public    :: mean => mean_of_column_elements_fn

! procedure :: mean => mean_of_column_elements_fn
! procedure :: min => minimum_of_column_elements_fn
! procedure :: max => maximum_of_column_elements_fn
! procedure :: median => median_of_column_elements_fn

end type T_DATA_COLUMN







type T_DATA_FRAME

  integer (kind=c_int), dimension(:), allocatable :: iDataTypes
  type (T_STRING_LIST) :: stColNames
  type (T_DATA_COLUMN), dimension(:), allocatable :: col
  integer (kind=c_int) :: iCount

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


contains

function sum_of_column_elements_fn(this) result(dSum)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dSum

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



function mean_of_column_elements_fn(this) result(dMean)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dMean

    ! [ LOCALS ]
    real (kind=c_double) :: dCount
    dCount = this%count()

    select case (this%iDataType)

      case (INTEGER_DATA)

        dMean = sum(this%iData, this%lMask) / dCount

      case (FLOAT_DATA)
      
        dMean = sum(this%fData, this%lMask) / dCount

      case (DOUBLE_DATA)
      
        dMean = sum(this%dData, this%lMask) / dCount

      case default
      
        dMean = -9999.

    end select

end function mean_of_column_elements_fn




function min_of_column_elements_fn(this) result(dMin)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dMin

    select case (this%iDataType)

      case (INTEGER_DATA)

        dMin = minval(this%iData, this%lMask)

      case (FLOAT_DATA)
      
        dMin = minval(this%fData, this%lMask)

      case (DOUBLE_DATA)
      
        dMin = minval(this%dData, this%lMask)

      case default
      
        dMin = -9999.

    end select

end function min_of_column_elements_fn



function max_of_column_elements_fn(this) result(dMax)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dMax

    select case (this%iDataType)

      case (INTEGER_DATA)

        dMax = maxval(this%iData, this%lMask)

      case (FLOAT_DATA)
      
        dMax = maxval(this%fData, this%lMask)

      case (DOUBLE_DATA)
      
        dMax = maxval(this%dData, this%lMask)

      case default
      
        dMax = -9999.

    end select

end function max_of_column_elements_fn



function count_of_column_elements_fn(this) result(dCount)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dCount  

    dCount = count(this%lMask)

    
end function count_of_column_elements_fn




  subroutine initialize_data_frame_sub(this, stColNames, iDataTypes, iRecordCount)

    class (T_DATA_FRAME), intent(inout) :: this
    type (T_STRING_LIST), intent(in) :: stColNames
    integer (kind=c_int), dimension(:), intent(in) :: iDataTypes
    integer (kind=c_int), intent(in) :: iRecordCount

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
      print *, " Creating new column for "//trim(sChar), &
               " with room for ", iRecordCount," values."

    enddo

  end subroutine initialize_data_frame_sub



  subroutine create_new_column_sub(this, iDataType, iCount)

    class (T_DATA_COLUMN) :: this
    integer (kind=c_int), intent(in) :: iDataType
    integer (kind=c_int),intent(in) :: iCount

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

    class (T_DATA_FRAME), intent(inout) :: this
    type (T_STRING), intent(inout) :: stString
    character (len=*), intent(in) :: sDelimiters

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum
    type (T_STRING) :: stSubString

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

    class (T_DATA_FRAME), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum
    type (T_STRING) :: stSubString

    ! [ LOCALS ]
    character (len=64) :: sChar
    type (T_STRING) :: stString

    do iIndex = 1, this%iCount

      stString = this%stColNames%value(iIndex)

      sChar = stString%asCharacter()

      write(*, "(/,a)") "Variable name: "//trim(sChar)


      write (*, fmt="(5x, a, i8)") "Count: ", int( this%col(iIndex)%count() )
      write (*, fmt="(7x, a, g15.5)") "Min: ", this%col(iIndex)%min()
      write (*, fmt="(7x, a, g15.5)") "Max: ", this%col(iIndex)%max()
      write (*, fmt="(7x, a, g15.5)") "Sum: ", this%col(iIndex)%sum()
      write (*, fmt="(7x, a, g15.5)") "Mean: ", this%col(iIndex)%mean()

     enddo 


  end subroutine summarize_data_frame_sub


! subroutine create_new_column_sub(this, sColumnName, iCount)

! class (T_DATA_COLUMN) :: this
! character (len=*), intent(in) :: sColumnName
! integer (kind=c_int),intent(in) :: iCount

! ! [ LOCALS ]
! integer (kind=c_int) :: iStat

! select type (this)

! class is (T_DATA_COLUMN_INT)

! allocate( this%iData(iCount), stat=iStat )

! class is (T_DATA_COLUMN_FLOAT)

! allocate( this%rData(iCount), stat=iStat )

! class default

! call assert(lFALSE, "Unhandled case select value", &
! trim(__FILE__), __LINE__)

! end select

! end subroutine create_new_column_sub


end module data_frame