module initialize_swb_options
 
  use types
  use keyword_list
  use block
  implicit none


contains

  subroutine initialize_GRID_DEFINITION_block(this)

    class (T_BLOCK)       :: this

    type (T_KEYWORD_LIST) :: kwList

    call kwList%addKeyPair("nx", "NA")
    call kwList%addKeyPair("ny", "NA")
    call kwList%addKeyPair("x0", "NA")
    call kwList%addKeyPair("y0", "NA")
    call kwList%addKeyPair("x1", "NA")
    call kwList%addKeyPair("y1", "NA")
    call kwList%addKeyPair("cell_size", "NA")
    call kwList%addKeyPair("resolution", "NA")

    call this%addblockname("GRID_DEFINITION")
    call this%addKeywords(kwList)

  end subroutine initialize_GRID_DEFINITION_block	



  subroutine initialize_PRECIPITATION_DATA_block(this)

    class (T_BLOCK)       :: this

    call this%addblockname("PRECIPITATION_DATA")


  end subroutine initialize_PRECIPITATION_DATA_block



end module initialize_swb_options