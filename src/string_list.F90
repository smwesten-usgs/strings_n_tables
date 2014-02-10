module string_list

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use types
  use strings
  implicit none

  private

  type, public :: T_STRING_LIST
    type (T_STRING), dimension(:), allocatable, private :: sl 
    integer (kind=c_int), private :: iCount = 0 

  contains
  
    procedure, private :: initialize => initialize_list_sub

    procedure, private :: append_string_sub
    procedure, private :: append_char_sub
    generic, public    :: append => append_char_sub, append_string_sub

    procedure, private :: return_size_of_vector_fn
    generic, public    :: count => return_size_of_vector_fn

!    procedure :: push => push_string_fn
!    procedure :: pop => pop_string_fn
    procedure, private :: return_string_at_index_fn
    generic, public    :: value => return_string_at_index_fn

    procedure, private :: return_matching_lines_fn
    generic, public    :: grep => return_matching_lines_fn

    procedure, private :: return_position_of_matching_text_fn
    generic, public    :: which => return_position_of_matching_text_fn

    procedure, private :: deallocate_all_list_items_sub
    generic, public    :: deallocate => deallocate_all_list_items_sub

    procedure, private :: print_to_screen_sub
    generic, public    :: print => print_to_screen_sub

  end type T_STRING_LIST	

  public :: assignment(=)

  interface assignment(=)
    procedure :: string_list_to_string_list_sub
  end interface assignment(=)

contains

  function return_size_of_vector_fn(this)   result(iResult)

    class (T_STRING_LIST), intent(in) :: this
    integer (kind=c_int)              :: iResult

    iResult = this%iCount

  end function return_size_of_vector_fn
  


  subroutine deallocate_all_list_items_sub(this)

    class (T_STRING_LIST), intent(inout) :: this
    
    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iStat

    if (this%iCount > 0) then

      do iIndex = 1, this%iCount

        call this%sl(iIndex)%deallocate()
        
      enddo

    endif   


  end subroutine deallocate_all_list_items_sub


  subroutine string_list_to_string_list_sub(stListOut, stListIn)

    type (T_STRING_LIST), intent(inout) :: stListOut
    type (T_STRING_LIST), intent(in)    :: stListIn

    ! [ LOCALS ] 
    integer (kind=c_int) :: iIndex

    call stListOut%initialize()   ! nuke anything that exists in the list...

    if (stListIn%iCount > 0) then

      do iIndex = 1, stListIn%iCount
        call stListOut%append(stListIn%sl(iIndex))
      enddo

    endif

  end subroutine string_list_to_string_list_sub


function return_string_at_index_fn(this, iIndex)  result(stString)

  class (T_STRING_LIST), intent (in)   :: this
  integer (kind=c_int)                 :: iIndex
  type (T_STRING)                      :: stString

  if ( ( iIndex >= lbound(this%sl,1) ) &
      .and. ( iIndex <= ubound(this%sl,1) ) ) then

    stString = this%sl(iIndex)

  endif

end function return_string_at_index_fn  

  subroutine initialize_list_sub(this)

    class (T_STRING_LIST), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    if ( allocated(this%sl) ) then  

      this%iCount = 0

    else  

      allocate(this%sl(20), stat = iStat)

      call assert(iStat == 0, &
          "Failed to allocate memory for list of string objects.")

    endif

  end subroutine initialize_list_sub


  function return_matching_lines_fn(this, sChar)     result(stString)

    class (T_STRING_LIST), intent(in) :: this
    character (len=*), intent(in)     :: sChar
    type (T_STRING_LIST)              :: stString

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iResult

    do iIndex=1, this%iCount

      iResult = index(string = this%sl(iIndex)%asCharacter(), &
                      substring = sChar)

      if (iResult /= 0)  call stString%append( this%sl(iIndex) )

    enddo  

  end function return_matching_lines_fn
  


  function return_position_of_matching_text_fn(this, sChar)     result(iResult)

    class (T_STRING_LIST), intent(in)                    :: this
    character (len=*), intent(in)                        :: sChar
    integer (kind=c_int), dimension(:), allocatable      :: iResult

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iCount
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iRetVal
    integer (kind=c_int), dimension(this%iCount) :: iTempResult

    iCount = 0

    do iIndex=1, this%iCount

      iRetval = index(string = this%sl(iIndex)%asCharacter(), &
                      substring = sChar)

      if (iRetval /= 0)  then

        iCount = iCount + 1
        iTempResult(iCount) = iIndex

      endif

    enddo  

    if (iCount == 0) then

      allocate(iResult(1), stat=iStat)
      iResult(1) = -9999

    else
    
      allocate(iResult(iCount), stat=iStat)
      iResult(1:iCount) = iTempResult(1:iCount)

    endif  

  end function return_position_of_matching_text_fn




  subroutine append_string_sub(this, stString)

    class (T_STRING_LIST), intent(inout)    :: this
    type (T_STRING), intent(in)        :: stString

    call list_check_allocation_sub(this)

    this%iCount = this%iCount + 1
    this%sl(this%iCount) = stString

  end subroutine append_string_sub  


  subroutine list_check_allocation_sub(this)

    class (T_STRING_LIST), intent(inout)  :: this


    ! [ LOCALS ] 
    type (T_STRING_LIST) :: templist
    integer (kind=c_int) :: iSize
    integer (kind=c_int) :: iNewSize
    integer (kind=c_int) :: iIndex

    if (allocated(this%sl)) then

      iSize = size(this%sl,1)

      !> We have reached the current maximum number of allocated
      !> string entities in this list
      !> NEED TO ALLOCATE MEMORY IN A NEW LIST, COPY EXISTING LIST
      if (iSize == this%iCount) then
        !> increase size by 30%
        iNewSize = int(real(iSize) * 0.3) + iSize
        allocate(templist%sl(iNewSize))

        !> copy each existing list item to temporary list
        do iIndex=1, iSize
          templist%sl(iIndex) = this%sl(iIndex)
        enddo

        !> transfer the memory associated with templist to
        !> the master list object
        call move_alloc(templist%sl, this%sl)

      endif

    else

      call this%initialize()

    endif

  end subroutine list_check_allocation_sub  



  subroutine append_char_sub(this, sChar)

    class (T_STRING_LIST), intent(inout) :: this
    character (len=*), intent(in)        :: sChar

    ! [ LOCALS ]
    type (T_STRING) :: stString

    call list_check_allocation_sub(this)

    this%iCount = this%iCount + 1
    stString = sChar
    this%sl(this%iCount) = stString

  end subroutine append_char_sub  



  subroutine print_to_screen_sub(this)

    class (T_STRING_LIST), intent(in) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    if (this%iCount > 0) then
      
      do iIndex = 1, this%iCount

        write(*, fmt="(a)") this%sl(iIndex)%asCharacter()

      enddo  

    endif

  end subroutine print_to_screen_sub

  
end module string_list