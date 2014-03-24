module dictionary

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use types
  use strings
  use string_list
  use keyword_list
  implicit none

  private

  type, public :: T_DICTIONARY
    integer (kind=c_int) :: iCount = 0
    type (T_KEYWORD_LIST), allocatable :: kwList(:)

  contains
  
    procedure, public :: getValues => get_values_associated_with_matching_keyword_fn
    procedure, public :: addKeyPair => add_keyword_list_object_sub  	

  end type T_DICTIONARY	


contains

  subroutine initialize_dictionary_sub(this, iInitialSize)

    class (T_DICTIONARY), intent(inout) :: this
    integer (kind=c_int), intent(in), optional :: iInitialSize

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if (present(iInitialSize)) then

      call this%deallocate()
      allocate(this%kwList(iInitialSize), stat=iStat)
      this%iCount = 0

    elseif ( allocated(this%ksList) ) then  

      this%iCount = 0

    else  

      allocate(this%kwList(20), stat = iStat)

    endif

    call assert(iStat == 0, &
      "Failed to allocate memory for dictionary object.", __FILE__, __LINE__)


  end subroutine initialize_dictionary_sub



 subroutine deallocate_all_dictionary_items_sub(this)

    class (T_DICTIONARY), intent(inout) :: this
    
    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iStat

    if ( allocated(this%kwList) ) then
      if (ubound(this%kwList,1) > 0) then

        do iIndex = 1, ubound(this%kwList,1)

          call this%kwList(iIndex)%deallocate()
        
        enddo

      endif

    endif   


  end subroutine deallocate_all_dictionary_items_sub


end module dictionary