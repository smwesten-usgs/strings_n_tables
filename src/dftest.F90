program dftest

  use iso_c_binding
  use strings
  use data_frame

  implicit none

  type (T_DATA_FILE) :: tFile
  type (T_STRING) :: stString
  type (T_STRING) :: st
  type (T_STRING_LIST) :: stl

  call tFile%openFile("rjh_21Nov13_Sample Lake.5.DMCM_fish_concs.csv")

  stString = tFile%getRow()

  print *, stString%asCharacter()

  do while (len(stString) > 0)

    st = stString%chomp(",")
    call st%replace(" ", "_")
    call stl%append(st)

  end do	

  call stl%print()

end program dftest