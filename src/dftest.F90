program dftest

  use iso_c_binding
  use strings
  use string_list
  use data_frame
  use data_file
  use types

  implicit none

  type (T_DATA_FILE) :: tFile
  type (T_STRING) :: stString
  type (T_STRING) :: st
  type (T_STRING_LIST) :: stl
  type (T_DATA_FRAME) :: df
  type (T_STRING_LIST) :: stHeader	

  integer (kind=c_int), dimension(:), allocatable :: iDataType

  call tFile%open("rjh_21Nov13_Sample Lake.5.DMCM_fish_concs.csv")

  stHeader = tFile%readHeader()

  allocate(iDataType(stHeader%count()))

  iDataType = FLOAT_DATA

  call df%initialize(stHeader, iDataType, tFile%numrecords())

  do while (tFile%isOpen() )

    stString = tFile%readLine()

    call df%rowvals(stString, ",")

  enddo

  !call stString%deallocate()
  !call st%deallocate()
  !call stl%deallocate()
  !call tFile%deallocate()

  call df%summarize()

end program dftest