module data_frame

use iso_c_binding, only : c_int, c_float, c_double, c_bool
use iso_fortran_env, only : IOSTAT_END
use strings
use string_list
use data_column
use types
implicit none

private

public :: T_DATA_FRAME


type T_DATA_FRAME

  integer (kind=c_int), allocatable        :: iDataTypes(:)
  type (T_STRING_LIST)                     :: stColNames
  class (T_DATA_COLUMN_PTR), allocatable   :: Columns(:)

contains

  ! procedure :: makeUnique => make_unique_identifier_sub
  !> take contents of serveral columns and concatenate them to
  !> create a unique ID (think METALICUS)

  procedure, private :: initialize_data_frame_sub
  generic, public    :: initialize => initialize_data_frame_sub

  procedure, private :: populate_data_frame_by_row_sub
  generic, public    :: rowvals => populate_data_frame_by_row_sub

  procedure, private :: summarize_data_frame_sub
  generic, public    :: summarize => summarize_data_frame_sub


end type T_DATA_FRAME


contains

  subroutine initialize_data_frame_sub(this, stColNames, iDataTypes, iRecordCount)

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
    allocate( this%Columns(this%stColNames%count() ) )

    print *, "Number of columns: ", ubound(this%Columns,1)

    do iIndex = 1, ubound(this%Columns,1)

      select case ( this%iDataTypes(iIndex) )

        case (INTEGER_DATA)

          !> reallocate to class T_DATA_COLUMN_INTEGER
          allocate( T_DATA_COLUMN_INTEGER :: this%Columns(iIndex)%pColumn )

        case (FLOAT_DATA)

          allocate( T_DATA_COLUMN_FLOAT :: this%Columns(iIndex)%pColumn )

        case (DOUBLE_DATA)

          allocate( T_DATA_COLUMN_DOUBLE :: this%Columns(iIndex)%pColumn )

        case (T_STRING_DATA)

          allocate( T_DATA_COLUMN_STRING :: this%Columns(iIndex)%pColumn )

        case (T_DATETIME_DATA)

          allocate( T_DATA_COLUMN_DATETIME :: this%Columns(iIndex)%pColumn )

        case default

          stop (" unhandled case in file data_column, line ~ 94")

      end select 

      call this%Columns(iIndex)%pColumn%new( iCount = iRecordCount )
          
      stString = this%stColNames%value(iIndex)

      sChar = stString%asCharacter()
      print *, " Creating new column for "//trim(sChar), &
               " with room for ", iRecordCount," values."

    enddo

  end subroutine initialize_data_frame_sub



  subroutine populate_data_frame_by_row_sub( this, stString, sDelimiters)

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

      select type ( col => this%Columns(iIndex)%pColumn )
 
        type is (T_DATA_COLUMN_INTEGER)

        iRecnum = col%putval( stSubString%asInt() )

        type is (T_DATA_COLUMN_FLOAT)

          iRecnum = col%putval( stSubString%asFloat() )

        type is (T_DATA_COLUMN_DOUBLE)

          iRecnum = col%putval( stSubString%asDouble() )

        type is (T_DATA_COLUMN_STRING)

          iRecnum = col%putval( stSubString )

        type is (T_DATA_COLUMN_DATETIME)  

          iRecnum = col%putval( stSubstring )

        class default 

      end select

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

    do iIndex = 1, ubound(this%Columns,1)

      stString = this%stColNames%value(iIndex)

      sChar = stString%asCharacter()

      write(*, "(/,a)") "Variable name: "//trim(sChar)


      write (*, fmt="(5x, a, i8)") "Count: ", int( this%Columns(iIndex)%pColumn%count() )
      write (*, fmt="(7x, a, g15.5)") "Min: ", this%Columns(iIndex)%pColumn%min()
      write (*, fmt="(7x, a, g15.5)") "Max: ", this%Columns(iIndex)%pColumn%max()
      write (*, fmt="(7x, a, g15.5)") "Sum: ", this%Columns(iIndex)%pColumn%sum()
      write (*, fmt="(7x, a, g15.5)") "Mean: ", this%Columns(iIndex)%pColumn%mean()

    enddo 


  end subroutine summarize_data_frame_sub


end module data_frame