module data_column

use iso_c_binding, only : c_int, c_float, c_double, c_bool
use iso_fortran_env, only : IOSTAT_END
use strings
use string_list
use datetime
use types
implicit none

private

type, public :: T_DATA_COLUMN

  integer (kind=c_int), private :: iCurrentRecord = 0
  integer (kind=c_int), private :: iOrder
  integer (kind=c_int), private :: iDataType
  integer (kind=c_int) :: iCount
  logical (kind=c_bool), dimension(:), allocatable, public :: lMask

contains

  private
!  procedure, private :: create_new_column_sub
  procedure, public    :: new => create_new_column_sub

!   procedure, private :: put_next_integer_value_fn
!   procedure, private :: put_next_float_value_fn
!   procedure, private :: put_next_double_value_fn
!   procedure, private :: put_next_string_value_fn
!   procedure, private :: put_next_datetime_value_fn
!   generic, public    :: putval => put_next_integer_value_fn, &
!                                   put_next_float_value_fn, &
!                                   put_next_double_value_fn, &
!                                   put_next_datetime_value_fn, &
!                                   put_next_string_value_fn

!  procedure, private :: sum_of_column_elements_fn
  procedure, public    :: sum => sum_of_column_elements_fn

!  procedure, private :: return_current_record_number_fn
  procedure, public    :: currentRecnum => return_current_record_number_fn

!  procedure, private :: increment_current_record_number_fn
  procedure, public    :: incrementRecnum => increment_current_record_number_fn

!  procedure, private :: return_data_type_fn
  procedure, public    :: datatype => return_data_type_fn

!  procedure, private :: min_of_column_elements_fn
  procedure, public    :: min => min_of_column_elements_fn

!  procedure, private :: max_of_column_elements_fn
  procedure, public    :: max => max_of_column_elements_fn

!  procedure, private :: count_of_column_elements_fn
  procedure, public    :: count => count_of_column_elements_fn

!  procedure, private :: mean_of_column_elements_fn
  procedure, public    :: mean => mean_of_column_elements_fn

end type T_DATA_COLUMN

!------

type, extends(T_DATA_COLUMN), public :: T_DATA_COLUMN_INTEGER

  integer (kind=c_int), dimension(:), allocatable, private  :: iData

contains 

  procedure, public :: putval => put_next_integer_value_fn

end type T_DATA_COLUMN_INTEGER

!------

type, extends(T_DATA_COLUMN), public :: T_DATA_COLUMN_FLOAT

  real (kind=c_float), dimension(:), allocatable, private   :: fData

contains 

  procedure, public :: putval => put_next_float_value_fn

end type T_DATA_COLUMN_FLOAT

!------

type, extends(T_DATA_COLUMN), public :: T_DATA_COLUMN_DOUBLE

  real (kind=c_double), dimension(:), allocatable, private  :: dData

contains

  procedure, public :: putval => put_next_double_value_fn

end type T_DATA_COLUMN_DOUBLE

!------

type, extends(T_DATA_COLUMN), public :: T_DATA_COLUMN_DATETIME

  type (T_DATETIME), dimension(:), pointer, private         :: pDatetime

contains

  procedure, public :: putval => put_next_datetime_value_fn

end type T_DATA_COLUMN_DATETIME

!------

type, extends(T_DATA_COLUMN), public :: T_DATA_COLUMN_STRING

  type (T_STRING_LIST), private                             :: stData

contains

  procedure, public :: putval => put_next_string_value_fn

end type T_DATA_COLUMN_STRING

!------



contains

  function put_next_integer_value_fn(this, iValue)   result(iRecNum)

    class (T_DATA_COLUMN_INTEGER), intent(inout)   :: this
    integer (kind=c_int), intent(in)    :: iValue
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%currentRecnum()
    this%iData(iRecNum) = iValue
    iRecNum = this%incrementRecnum()

  end function put_next_integer_value_fn

!--------------------------------------------------------------------

  function put_next_float_value_fn(this, fValue)   result(iRecNum)

    class (T_DATA_COLUMN_FLOAT), intent(inout)   :: this
    real (kind=c_float), intent(in)     :: fValue
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%currentRecnum()
    this%fData(iRecNum) = fValue
    iRecNum = this%incrementRecnum()

  end function put_next_float_value_fn

!--------------------------------------------------------------------

  function put_next_double_value_fn(this, dValue)   result(iRecNum)

    class (T_DATA_COLUMN_DOUBLE), intent(inout)   :: this
    real (kind=c_double), intent(in)    :: dValue
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%currentRecnum()
    this%dData(iRecNum) = dValue
    iRecNum = this%incrementRecnum()

  end function put_next_double_value_fn

!--------------------------------------------------------------------

  function put_next_datetime_value_fn(this, stDatetime)   result(iRecNum)

    class (T_DATA_COLUMN_DATETIME), intent(inout)   :: this
    type (T_STRING), intent(in)            :: stDatetime
    integer (kind=c_int)                   :: iRecNum

    ! [ LOCALS ]
    type (T_DATETIME), pointer :: pDT

    iRecNum = this%currentRecnum()
    pDT => this%pDatetime(iRecNum)
    call pDT%parseDate( stDatetime ) 
    iRecNum = this%incrementRecnum()

  end function put_next_datetime_value_fn

!--------------------------------------------------------------------

  function put_next_string_value_fn(this, stString)   result(iRecNum)

    class (T_DATA_COLUMN_STRING), intent(inout)   :: this
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

    select type(this)

      type is (T_DATA_COLUMN_INTEGER)

        iDataType = INTEGER_DATA

      type is (T_DATA_COLUMN_FLOAT)  

        iDataType = FLOAT_DATA

      type is (T_DATA_COLUMN_DOUBLE)  

        iDataType = DOUBLE_DATA

      type is (T_DATA_COLUMN_DATETIME)  

        iDataType = T_DATETIME_DATA

      type is (T_DATA_COLUMN_STRING)  

        iDataType = T_STRING_DATA

      class default
    
        iDataType = -9999

    end select  

  end function return_data_type_fn  

!--------------------------------------------------------------------

  function sum_of_column_elements_fn(this) result(dSum)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dSum

    select type (this)

      type is (T_DATA_COLUMN_INTEGER)

        dSum = sum(this%iData, this%lMask)

      type is (T_DATA_COLUMN_FLOAT)
      
        dSum = sum(this%fData, this%lMask)

      type is (T_DATA_COLUMN_DOUBLE)
      
        dSum = sum(this%dData, this%lMask)

      class default
      
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

    select type (this)

      type is (T_DATA_COLUMN_INTEGER)

        dMean = sum(this%iData, this%lMask) / dCount

      type is (T_DATA_COLUMN_FLOAT)
      
        dMean = sum(this%fData, this%lMask) / dCount

      type is (T_DATA_COLUMN_DOUBLE)
      
        dMean = sum(this%dData, this%lMask) / dCount

      class default
      
        dMean = -9999.

    end select

  end function mean_of_column_elements_fn

!--------------------------------------------------------------------

  function min_of_column_elements_fn(this) result(dMin)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dMin

    select type (this)

      type is (T_DATA_COLUMN_INTEGER)

        dMin = minval(this%iData, this%lMask)

      type is (T_DATA_COLUMN_FLOAT)
      
        dMin = minval(this%fData, this%lMask)

      type is (T_DATA_COLUMN_DOUBLE)
      
        dMin = minval(this%dData, this%lMask)

      class default
      
        dMin = -9999.

    end select

  end function min_of_column_elements_fn

!--------------------------------------------------------------------

  function max_of_column_elements_fn(this) result(dMax)

    class(T_DATA_COLUMN), intent(in) :: this
    real (kind=c_double) :: dMax

    select type (this)

      type is (T_DATA_COLUMN_INTEGER)

        dMax = maxval(this%iData, this%lMask)

      type is (T_DATA_COLUMN_FLOAT)
      
        dMax = maxval(this%fData, this%lMask)

      type is (T_DATA_COLUMN_DOUBLE)
      
        dMax = maxval(this%dData, this%lMask)

      class default
      
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

  subroutine create_new_column_sub(this, iCount)

    class (T_DATA_COLUMN) :: this
    integer (kind=c_int),intent(in) :: iCount

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    !> lMask entries will be used later on to assist in 
    !> subsetting of particular chunks of data
    allocate( this%lMask(iCount), stat = iStat)
    this%lMask = lTRUE

    select type (this)

      type is (T_DATA_COLUMN_INTEGER)

        allocate( this%iData(iCount), stat = iStat )

      type is (T_DATA_COLUMN_FLOAT)

        allocate( this%fData(iCount), stat = iStat)

      type is (T_DATA_COLUMN_DOUBLE)

        allocate( this%dData(iCount), stat = iStat)

      type is (T_DATA_COLUMN_STRING)

        iStat = 0

      type is (T_DATA_COLUMN_DATETIME)

        allocate( this%pDatetime(iCount), stat=iStat)

      class default

        call assert(lFALSE, "Internal programming error", &
            __FILE__, __LINE__)

    end select

  call assert( iStat == 0, "Failed to allocate memory while creating a new column", &
        __FILE__, __LINE__)

  end subroutine create_new_column_sub


end module data_column