program dftest

  use iso_c_binding
  use strings
  use data_frame

  implicit none

  type (T_DATA_FILE) :: tFile

  call tFile%openFile("rjh_21Nov13_Sample Lake.5.DMCM_fish_concs.csv")



end program dftest