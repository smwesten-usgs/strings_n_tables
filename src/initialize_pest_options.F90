module initialize_pest_options
 
  use types
  use keyword_list
  use block
  implicit none

contains

  subroutine initialize_CONTEXT_keyword(this)

  	class (T_KEYWORD_LIST)  :: this

    call this%addkeyword("CONTEXT");
    call this%addkeyword("CONTXT")
    call this%addargument("all")
    call this%addargument("pest_optimization")

  end subroutine initialize_CONTEXT_keyword


  subroutine initialize_GRID_DATA_block(this)

    class (T_BLOCK)       :: this

    call this%addblockname("GRID_DATA")
    call this%addblockname("GRIDDED_DATA")
    call this%addblockname("GRIDDED_DATASET")

  end subroutine initialize_GRID_DATA_block	



end module initialize_pest_options