program dftest

  use iso_c_binding
  use strings
  use string_list
  use data_frame
  use data_column
  use data_file
  use datetime
  use types

  implicit none

  type (T_DATA_FILE) :: tFile
  type (T_STRING) :: stString
  type (T_STRING) :: st
  type (T_STRING_LIST) :: stl
  type (T_DATA_FRAME) :: df, df2
  type (T_STRING_LIST) :: stHeader	
  class (T_DATA_COLUMN), pointer :: pColumn
  class (T_DATETIME), pointer    :: pDate

  integer (kind=c_int) :: iIndex
  integer (kind=c_int) :: iUpperbound

  integer (kind=c_int), dimension(:), allocatable :: iDataType

  integer (kind=c_int), dimension(:), allocatable :: iResult

  call tFile%open("rjh_21Nov13_Sample Lake.5.DMCM_fish_concs.csv")

  stHeader = tFile%readHeader()

  allocate(iDataType(stHeader%count()))

  iDataType = FLOAT_DATA

  call df%initialize(stHeader, iDataType, tFile%numrecords())

  do while (tFile%isOpen() )

    stString = tFile%readLine()

    call df%putrow(stString, ",")

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

  iDataType = [ T_STRING_DATA, T_DATE_DATA, T_TIME_DATA, FLOAT_DATA ]

  call df2%initialize(stl, iDataType, tFile%numrecords())

  do while (tFile%isOpen() )

    stString = tFile%readLine()

    call df2%putrow( stString, sWHITESPACE )

  enddo

!  call df2%summarize()

!   iResult = df2%findcol("Date")
  
!   print *, "Column number that contains Date:", iResult(1)

!   pColumn => df2%getcol(iResult(1))

!   iUpperbound = pColumn%count()

!   do iIndex=1, iUpperbound

!     pDate => pColumn%getval(iIndex)
!     call pDate%printdate()

!   enddo


  call df2%summarize()

  pColumn => df2%getcol( "Discharge" )
  call pColumn%select(300.,GT)
  call df2%summarize()

end program dftest