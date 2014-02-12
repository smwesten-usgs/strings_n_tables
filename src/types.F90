!> @file Common global general-purpose definitions

module types

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  implicit none

  public

  character (len=2), parameter :: sWHITESPACE = achar(9)//" "
  character (len=1), parameter :: sBACKSLASH = achar(92)
  character (len=1), parameter :: sCOMMENT_CHARS = "#"
  character (len=1), parameter :: sFORWARDSLASH = achar(47)
  character (len=1), parameter :: sRETURN = achar(13)
  character (len=1), parameter :: sDOUBLEQUOTE = achar(34)

  character (len=52), parameter :: sALPHA = &
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  character (len=26), parameter :: sUPPERCASE = &
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  

  character (len=15), parameter :: sNUMERIC = &
    "0123456789.eE+-"  

  character (len=10), parameter :: sINTEGER = &
    "0123456789"

  integer (kind=c_int), parameter :: INTEGER_DATA = 1
  integer (kind=c_int), parameter :: FLOAT_DATA = 2
  integer (kind=c_int), parameter :: DOUBLE_DATA = 3
  integer (kind=c_int), parameter :: T_STRING_DATA = 4
  integer (kind=c_int), parameter :: T_DATETIME_DATA = 5

  logical (kind=c_bool), parameter :: lFALSE = .false.
  logical (kind=c_bool), parameter :: lTRUE = .true.

  real (kind=c_float), parameter :: fZERO = 0.0_c_float
  real (kind=c_double), parameter :: dZERO = 0.0_c_double

contains

function pf(lBool)   result(sPassFail)

  logical (kind=c_bool), intent(in) :: lBool
  character (len=4) :: sPassFail  

  if (lBool) then
    sPassFail = "PASS"
  else
    sPassFail = "FAIL"  
  endif

end function pf  


function ok(lBool)   result(sOK)

  logical (kind=c_bool), intent(in) :: lBool
  character (len=2) :: sOK  

  if (lBool) then
    sOK = "OK"
  else
    sOK = "  "  
  endif

end function ok  

end module types