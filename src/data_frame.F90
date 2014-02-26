module data_frame

use iso_c_binding, only : c_int, c_float, c_double, c_bool
use iso_fortran_env, only : IOSTAT_END
use exceptions
use strings
use string_list
use data_column
use datetime
use types
implicit none

private

public :: T_DATA_FRAME


type T_DATA_FRAME

  integer (kind=c_int), allocatable        :: iDataTypes(:)
  type (T_STRING_LIST)                     :: stColNames
  logical (kind=c_bool), dimension(:), allocatable, public :: lMask
  class (T_DATA_COLUMN_PTR), allocatable   :: Columns(:)
  logical (kind=c_bool)                    :: lHasDate = lFALSE

contains

  ! procedure :: makeUnique => make_unique_identifier_sub
  !> take contents of serveral columns and concatenate them to
  !> create a unique ID (think METALICUS)

  procedure, private :: initialize_data_frame_sub
  generic, public    :: initialize => initialize_data_frame_sub

  procedure, private :: populate_data_frame_by_row_sub
  generic, public    :: putrow => populate_data_frame_by_row_sub

  procedure, private :: summarize_data_frame_sub
  generic, public    :: summarize => summarize_data_frame_sub

  procedure, private :: find_column_by_name_fn
  generic, public    :: findcol => find_column_by_name_fn

  procedure, private :: select_rows_from_column_sub
  generic, public    :: select => select_rows_from_column_sub

  procedure, private :: get_column_pointer_byindex_fn
  procedure, private :: get_column_pointer_byname_fn
  generic, public    :: getcol => get_column_pointer_byindex_fn, &
                                  get_column_pointer_byname_fn

end type T_DATA_FRAME


contains

  subroutine select_rows_from_column_sub( this, sColname, iComparison, iValue, fValue, dValue, dtValue, stValue )

    class (T_DATA_FRAME), intent(inout)            :: this
    character (len=*), intent(in)                  :: sColname
    integer (kind=c_int), intent(in)               :: iComparison
    integer (kind=c_int), intent(in), optional     :: iValue
    real (kind=c_float), intent(in), optional      :: fValue
    real (kind=c_double), intent(in), optional     :: dValue
    type (T_DATETIME), intent(in), optional        :: dtValue
    type (T_STRING), intent(in), optional          :: stValue

    ! [ LOCALS ] 
    class (T_DATA_COLUMN), pointer :: pColumn

    pColumn => this%getcol( sColname )

    if ( associated(pColumn) ) then

      if (present(iValue) ) then

        call pColumn%select( iValue, iComparison )
      
      elseif (present(fValue) ) then

        call pColumn%select( fValue, iComparison )        

      elseif (present(dValue) ) then

        call pColumn%select( dValue, iComparison )        

      elseif (present(dValue) ) then

        call pColumn%select( fValue, iComparison )        

      elseif (present(dtValue) ) then

        call pColumn%select( dtValue, iComparison )        

!      elseif (present(stValue) ) then

!        call pColumn%select( stValue, iComparison )        

      endif

      this%lMask = pColumn%lMask
      
    endif    




  end subroutine select_rows_from_column_sub  



  function get_column_pointer_byindex_fn( this, iColNum )    result( pColumn )

    class (T_DATA_FRAME), intent(in)         :: this
    integer (kind=c_int)                     :: iColNum
    class (T_DATA_COLUMN), pointer           :: pColumn 

    pColumn => null()

    if (allocated(this%Columns)) then

      if (iColNum >= lbound(this%Columns,1) &
         .and. iColNum <= ubound(this%Columns,1) ) then

        pColumn => this%Columns(iColNum)%pColumn

      endif
    
    else

      call warn("'Columns' member has not been allocated yet.", __FILE__, __LINE__)

    endif    

  end function get_column_pointer_byindex_fn



  function get_column_pointer_byname_fn( this, sColName )    result( pColumn )

    class (T_DATA_FRAME), intent(in)         :: this
    character (len=*), intent(in)            :: sColName
    class (T_DATA_COLUMN), pointer           :: pColumn 

    ! [ LOCALS ]
    integer (kind=c_int), allocatable :: iColNum(:)

    iColNum = this%findcol(sColName)

    if (ubound(iColNum,1) > 0) then

      pColumn => this%getcol(iColNum(1))

    else
    
      pColumn => null()
      call warn("Failed to find a column with name "//trim(sColName), __FILE__,__LINE__)

    endif  

  end function get_column_pointer_byname_fn



  function find_column_by_name_fn( this, sChar )   result( iColNum )

    class (T_DATA_FRAME), intent(in)   :: this
    character (len=*), intent(in)      :: sChar
    integer (kind=c_int), allocatable  :: iColNum(:)

    ! [ LOCALS ]
    integer (kind=c_int) :: iCount

    iCount = this%stColNames%countMatches(sChar)

    if (iCount > 0) then

!      allocate(iColNum(iCount))
      iColNum = this%stColNames%which(sChar)

    else
    
      allocate(iColNum(1))  
      iColNum = 0

    endif  

  end function find_column_by_name_fn



  subroutine initialize_data_frame_sub( this, stColNames, iDataTypes, iRecordCount )

    class (T_DATA_FRAME), intent(inout) :: this
    type (T_STRING_LIST), intent(in) :: stColNames
    integer (kind=c_int), dimension(:), intent(in) :: iDataTypes
    integer (kind=c_int), intent(in) :: iRecordCount

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    integer (kind=c_int) :: iIndex
    character (len=64) :: sChar
    type (T_STRING) :: stString

    this%stColNames = stColNames
    this%iDataTypes = iDataTypes

    !> allocate space for the required number of class T_DATA_COLUMN_PTR 
    allocate( this%Columns(this%stColNames%count() ), stat=iStat )

    call assert(iStat==0, "Failed to allocate memory for data frame", __FILE__, __LINE__)

    do iIndex = 1, ubound(this%Columns,1)

      allocate( T_DATA_COLUMN :: this%Columns(iIndex)%pColumn, stat=iStat )
 
      associate ( col => this%Columns(iIndex)%pColumn )

        call col%new( iDataType = iDataTypes(iIndex), iCount = iRecordCount )
            
        stString = this%stColNames%value(iIndex)

        print *, " Creating new column for "//asCharacter(stString) &
                 //" with room for "//asCharacter(iRecordCount)//" values."

      end associate

    enddo

    ! initialize dataframe row mask; all are selected initially
    allocate( this%lMask(iRecordCount), stat=iStat )
    this%lMask = lTRUE

  end subroutine initialize_data_frame_sub



  subroutine populate_data_frame_by_row_sub( this, stString, sDelimiters )

    class (T_DATA_FRAME), intent(inout) :: this
    type (T_STRING), intent(inout) :: stString
    character (len=*), intent(in) :: sDelimiters

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum
    type (T_STRING) :: stSubString
    integer (kind=c_int) :: iRecnum

    iIndex = 0

    do while (stString%length() > 0)

      stSubString = stString%chomp(sDelimiters)

      iIndex = iIndex + 1

      !iRowNum = this%Columns(iIndex)%pColumn%incrementRecnum()

      if (iIndex > ubound(this%Columns,1))  stop ("Too many columns read in.")

      associate (col => this%Columns(iIndex)%pColumn )
   
        select case ( col%datatype() )
   
          case (INTEGER_DATA)

            iRecnum = col%putval( stSubString%asInt() )

          case (FLOAT_DATA)

            iRecnum = col%putval( stSubString%asFloat() )

          case (DOUBLE_DATA)

            iRecnum = col%putval( stSubString%asDouble() )

          case (T_STRING_DATA)

            iRecnum = col%putval( stSubString )

          case (T_DATETIME_DATA)  

            iRecnum = col%putdatetime( stSubstring )

          case (T_DATE_DATA)

            iRecNum = col%putdate( stSubstring )

          case (T_TIME_DATA)

            iRecNum = col%puttime( stSubString )

          case default

            call die("Unhandled select case", __FILE__, __LINE__)

        end select

      end associate

    enddo


  end subroutine populate_data_frame_by_row_sub





  subroutine summarize_data_frame_sub( this )

    class (T_DATA_FRAME), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum
    type (T_STRING) :: stSubString

    ! [ LOCALS ]
    character (len=64) :: sChar
    type (T_STRING) :: stString

    print *, ubound(this%Columns,1)

    do iIndex = 1, ubound(this%Columns,1)
 
      stString = this%stColNames%value(iIndex)

      sChar = stString%asCharacter()

      write(*, "(/,a)") "Variable name: "//trim(sChar)

      write (*, fmt="(5x, a, i8)") "Count: ", int( this%Columns(iIndex)%pColumn%count( this%lMask ) )
      write (*, fmt="(7x, a, g15.5)") "Min: ", this%Columns(iIndex)%pColumn%min( this%lMask )
      write (*, fmt="(7x, a, g15.5)") "Max: ", this%Columns(iIndex)%pColumn%max( this%lMask )
      write (*, fmt="(7x, a, g15.5)") "Sum: ", this%Columns(iIndex)%pColumn%sum( this%lMask )
      write (*, fmt="(7x, a, g15.5)") "Mean: ", this%Columns(iIndex)%pColumn%mean( this%lMask )

    enddo 


  end subroutine summarize_data_frame_sub


end module data_frame