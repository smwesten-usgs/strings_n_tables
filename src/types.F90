module types

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  implicit none

  public

  character (len=2), parameter :: sWHITESPACE = achar(9)//" "
  character (len=1), parameter :: sBACKSLASH = achar(92)
  character (len=1), parameter :: sCOMMENT_CHARS = "#"
  character (len=1), parameter :: sFORWARDSLASH = achar(47)
  character (len=1), parameter :: sRETURN = achar(13)

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

  logical (kind=c_bool), parameter :: lFALSE = .false.
  logical (kind=c_bool), parameter :: lTRUE = .true.

end module types