program test_options

  use iso_c_binding, only : c_int, c_bool, c_float, c_double
  use string_list
  use block
  use keyword_list
  use block_list
  use initialize_swb_options
  implicit none

  character (len=:), allocatable :: sMyFile(:)

  type (T_STRING_LIST) :: stlString

  type (T_BLOCK) :: myBlock
  type (T_BLOCK) :: myBlock2

  type (T_BLOCK_LIST) :: myBlockList

  type (T_KEYWORD_LIST) :: myKeyList, myKeyList2
 
  call myBlock%initialize( initialize_GRID_DEFINITION_block )

  call myBlockList%addBlock( myBlock )

  call myBlock2%initialize( initialize_PRECIPITATION_DATA_block )
  call myBlockList%addBlock( myBlock2 )

  sMyFile = ["GRID 188 268  568500.   4737500.0   250.0                                                       ", &
            "BASE_PROJECTION_DEFINITION +proj=utm +zone=18 +north +ellps=GRS80 +datum=NAD83 +units=m +no_defs", &
            "PRECIPITATION NETCDF http://cida.usgs.gov/thredds/dodsC/new_gmo                                 ", &
            "PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lonlat +ellps=GRS80 +datum=WGS84 +no_defs        ", &
            "PRECIPITATION_CONVERSION_FACTOR 3.937E-2                                                        ", &
            "PRECIPITATION_MISSING_VALUES_CODE 1.0E+15                                                       ", &
            "PRECIPITATION_MISSING_VALUES_OPERATOR >=                                                        "]

!  stlString = myBlock%getblocknames()
!  call stlString%print()

 ! stlString = myKeyList%getarguments()
 ! call stlString%print()

  stlString = myBlockList%getblocknames()
  
  myBlock = myBlockList%getblock("GRID_DEFINITION")

  call myBlock%printBlock()

end program test_options