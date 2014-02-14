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
  type (T_DATA_FRAME) :: df, df2
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

  call tFile%close()


  call tFile%open("Q_BEC_BE_6500.ssf")

  call stl%deallocate()

  st = "USGS_ID"
  call stl%append(st)

  st = "Date"
  call stl%append(st)

  st = "Time"
  call stl%append(st)

  st = "Discharge"
  call stl%append(st)

  deallocate(iDataType)
  allocate(iDataType(4))

  iDataType = [ T_STRING_DATA, T_DATETIME_DATA, T_STRING_DATA, FLOAT_DATA ]

  call df2%initialize(stl, iDataType, tFile%numrecords())

  do while (tFile%isOpen() )

    stString = tFile%readLine()

    call df2%rowvals( stString, sWHITESPACE )

  enddo

  call df2%summarize()

end program dftest