module parameter

  use iso_c_binding
  use strings
  use string_list
  use data_frame
  use data_column
  use data_file
  implicit_none

  type, public :: T_PARAMETER
    type (T_STRING)      :: stParamName
    type (T_STRING)      :: stLongParamName
    integer (kind=c_int) :: iParameterType     ! INTEGER_DATA, FLOAT_DATA, DOUBLE_DATA
    integer (kind=c_int), allocatable     :: iParamValue(:)
    real (kind=c_float), allocatable      :: fParamValue(:)
    real (kind=c_double), allocatable     :: dParamValue(:)
    
    integer (kind=c_int), allocatable     :: iParamDefaults(:)
    real (kind=c_float), allocatable      :: fParamDefaults(:)
    real (kind=c_double), allocatable     :: dParamDefaults(:)

  contains

  	procedure, private :: new_parameter_int_sub
  	procedure, private :: new_parameter_float_sub
    procedure, private :: new_parameter_double_sub
    generic, publid    :: new => new_parameter_int_sub, &
                                 new_parameter_float_sub, &
                                 new_parameter_double_sub

  end type T_PARAMETER

  integer (kind=c_int), parameter, public :: PARAM_MINIMUM, PARAM_MAXIMUM, PARAM_DEFAULT 

contains

  subroutine new_parameter_int_sub(this, sParamName, sLongParamName, iParamValues, iParamDefaults)

    class (T_PARAMETER), intent(in)         :: this
    character (len=*), intent(in)           :: sParamName
    character (len=*), intent(in)           :: sLongParamName
    

  end subroutine new_parameter_int_sub
  	

end module parameter