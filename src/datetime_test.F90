program datetime_test

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use strings
  use datetime
  use exceptions
  implicit none

  character (len=:), allocatable :: sDateFormat, sDate

  type (T_DATETIME) :: dtDate1
  type (T_DATETIME) :: dtDate2
  type (T_DATETIME) :: dtDate3
  type (T_DATETIME) :: dtDate4

  real (kind=c_float) :: fValue
  integer (kind=c_int) :: iValue
  real (kind=c_double) :: dValue
  integer (kind=c_int) :: iIndex

  sDateFormat = "YYYY-MM-DD"

  call dtDate1%setDateFormat( sDateFormat )
  call dtDate1%parseDate( '1960-03-14' )
  call dtDate1%parseTime( '13:12:07')

  call dtDate2%setDateFormat( "MM.YYYY.DD" )
  call dtDate2%parseDate( "03.1960.15" )

  call dtDate3%setDateFormat( "MM/DD/YYYY" )
  call dtDate3%parseDate("03/15/1960")


  print *, dtDate1%prettydate()
  print *, dtDate2%prettydate()
  print *, dtDate3%prettydate()

  print *, dtDate1%prettydate()//" > "//dtDate2%prettydate()//"? ", &
             dtDate1 > dtDate2 	

  print *, dtDate2%prettydate()//" >= "//dtDate3%prettydate()//"? ", &
             dtDate2 >= dtDate3 	

  dtDate4 = dtDate1 - 42

  print *, dtDate1%prettydatetime()//" - "//" 42? "//dtDate4%prettydatetime()  


  ! remove *, below to trigger weird compiler error
  do iIndex = 1,36,2
  	dtDate4 = dtDate1%addMonth(iIndex)
  	dtDate2 = dtDate1%subtractMonth(iIndex)
    print *, dtDate4%prettydatetime(), "   ", dtDate2%prettydatetime()
  enddo  

print *, dtDate1%prettydatetime()

dtDate1 = dtDate1%addMonth(3)

print *, dtDate1%prettydatetime()

end program datetime_test