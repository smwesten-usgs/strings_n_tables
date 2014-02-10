module data_column

use iso_c_binding, only : c_int, c_float, c_double, c_bool
use iso_fortran_env, only : IOSTAT_END
use strings
use string_list
use types
implicit none

private

public :: T_DATA_COLUMN

type T_DATA_COLUMN

  integer (kind=c_int), private :: iCurrentRecord = 0
  integer (kind=c_int), private :: iOrder
  integer (kind=c_int), private :: iDataType
  integer (kind=c_int) :: iCount
  integer (kind=c_int), dimension(:), allocatable, public :: iData
  real (kind=c_float), dimension(:), allocatable, public :: fData
  real (kind=c_double), dimension(:), allocatable, public :: dData
  logical (kind=c_bool), dimension(:), allocatable, public :: lMask
  type (T_STRING_LIST), allocatable, public :: stData

contains

  procedure, private :: create_new_column_sub
  generic, public    :: new => create_new_column_sub

  procedure, private :: put_next_integer_value_fn
  procedure, private :: put_next_float_value_fn
  procedure, private :: put_next_double_value_fn
  procedure, private :: put_next_string_value_fn
  generic, public    :: putval => put_next_integer_value_fn, &
                                  put_next_float_value_fn, &
                                  put_next_double_value_fn, &
                                  put_next_string_value_fn

  procedure, private :: sum_of_column_elements_fn
  generic, public    :: sum => sum_of_column_elements_fn

  procedure, private :: return_current_record_number_fn
  generic, public    :: currentRecnum => return_current_record_number_fn

  procedure, private :: increment_current_record_number_fn
  generic, public    :: incrementRecnum => increment_current_record_number_fn

  procedure, private :: return_data_type_fn
  generic, public    :: datatype => return_data_type_fn

  procedure, private :: min_of_column_elements_fn
  generic, public    :: min => min_of_column_elements_fn

  procedure, private :: max_of_column_elements_fn
  generic, public    :: max => max_of_column_elements_fn

  procedure, private :: count_of_column_elements_fn
  generic, public    :: count => count_of_column_elements_fn

  procedure, private :: mean_of_column_elements_fn
  generic, public    :: mean => mean_of_column_elements_fn

end type T_DATA_COLUMN



contains

  function put_next_integer_value_fn(this, iValue)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    integer (kind=c_int), intent(in)    :: iValue
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%currentRecnum()
    this%iData(iRecNum) = iValue
    iRecNum = this%incrementRecnum()

  end function put_next_integer_value_fn

!--------------------------------------------------------------------

  function put_next_float_value_fn(this, fValue)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    real (kind=c_float), intent(in)     :: fValue
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%currentRecnum()
    this%fData(iRecNum) = fValue
    iRecNum = this%incrementRecnum()

  end function put_next_float_value_fn

!--------------------------------------------------------------------

  function put_next_double_value_fn(this, dValue)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    real (kind=c_double), intent(in)    :: dValue
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%currentRecnum()
    this%dData(iRecNum) = dValue
    iRecNum = this%incrementRecnum()

  end function put_next_double_value_fn

!--------------------------------------------------------------------

  function put_next_string_value_fn(this, stString)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    type (T_STRING)                     :: stString
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%currentRecnum()
    call this%stData%append(stString)
    iRecNum = this%incrementRecnum()

  end function put_next_string_value_fn

!--------------------------------------------------------------------

  function return_current_record_number_fn(this)     result(iRecNum)

    class (T_DATA_COLUMN), intent(in)   :: this
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%iCurrentRecord

  end function return_current_record_number_fn
  
!--------------------------------------------------------------------

  function increment_current_record_number_fn(this, iIncrementAmt)  result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    integer (kind=c_int), optional         :: iIncrementAmt
    integer (kind=c_int)                   :: iRecNum

    if (present(iIncrementAmt) ) then

      this%iCurrentRecord = this%iCurrentRecord + iIncrementAmt

    else

      this%iCurrentRecord = this%iCurrentRecord + 1

    endif

    iRecNum = this%iCurrentRecord

  end function increment_current_record_number_fn

!--------------------------------------------------------------------

  function return_data_type_fn(this)    result(iDataType)

    class (T_DATA_COLUMN), intent(in) :: this
    integer (kind=c_int)              :: iDataType

    iDataType = this%iDataType

  end function return_data_type_fn  

!--------------------------------------------------------------------

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

!--------------------------------------------------------------------

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

!--------------------------------------------------------------------

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

!--------------------------------------------------------------------

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

!--------------------------------------------------------------------

  function count_of_column_elements_fn(this) result(dCount)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dCount  

    dCount = count(this%lMask)

    
  end function count_of_column_elements_fn

!--------------------------------------------------------------------

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


end module data_column