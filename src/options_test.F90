program test_options

  use iso_c_binding, only : c_int, c_bool, c_float, c_double
  use options
  use string_list
  use initialize_pest_options
  implicit none

  character (len=:), allocatable :: sBlock(:)

  type (T_STRING_LIST) :: stlString

  type (T_BLOCK) :: myBlock

  type (T_KEYWORD_LIST) :: myKeyList

  call myKeyList%initialize( initialize_CONTEXT_keyword )

  call myBlock%initialize( initialize_GRID_DATA_block )
  call myBlock%addkeyword( myKeyList )


  sBlock = ["BEGIN GRID_DATA                        ", &
            "  CONTEXT alleistgut                   ", &
            "  SOILS_GRID ARC_ASCII input\myGrid.asc", &
            "  OPTION2 more arguments               ", &
            "END GRID_DATA                          "] 

  stlString = myBlock%getblocknames()
  call stlString%print()

  stlString = myKeyList%getarguments()
  call stlString%print()

end program test_options