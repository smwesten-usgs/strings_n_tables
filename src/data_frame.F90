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

  integer (kind=c_int), dimension(:), allocatable :: iDataTypes
  type (T_STRING_LIST) :: stColNames
  class (T_DATA_COLUMN), dimension(:), allocatable :: col
  integer (kind=c_int) :: iCount

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

    allocate( this%col(this%stColNames%count() ) )

    this%iCount = this%stColNames%count()

    print *, "Number of columns: ", this%iCount

    do iIndex = 1, this%iCount

      associate ( current_column => this%col(iIndex) )

        select case ( this%iDataTypes(iIndex) )

          case (INTEGER_DATA)

            allocate(T_DATA_COLUMN_INTEGER:this%col(iIndex))
            call current_column%new( iCount = iRecordCount )
      !> create a new column space for each item detected in the header
!      call this%col(iIndex)%new(iDataType = this%iDataTypes(iIndex), &
!                                iCount = iRecordCount)
          case default

        end select
        
      end associate 
          
      stString = this%stColNames%value(iIndex)

      sChar = stString%asCharacter()
      print *, " Creating new column for "//trim(sChar), &
               " with room for ", iRecordCount," values."

    enddo

  end subroutine initialize_data_frame_sub



  subroutine populate_data_frame_by_row_sub( this, stString , sDelimiters)

    class (T_DATA_FRAME), intent(inout) :: this
    type (T_STRING), intent(inout) :: stString
    character (len=*), intent(in) :: sDelimiters

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum
    type (T_STRING) :: stSubString
    type (T_DATA_COLUMN) :: tCol

    iIndex = 0

    do while (stString%length() > 0)

      stSubString = stString%chomp(sDelimiters)

      iIndex = iIndex + 1

      iRowNum = this%col(iIndex)%incrementRecnum()

      associate ( tCol => this%col(iIndex) )
      
        select type ( tCol )
 
          type is (T_DATA_COLUMN_INTEGER)

            call tCol%putval( stSubString%asInt() )

          type is (T_DATA_COLUMN_FLOAT)

            call tCol%putval( stSubString%asFloat() )

          type is (T_DATA_COLUMN_DOUBLE)

            call tCol%putval( stSubString%asDouble() )

          type is (T_DATA_COLUMN_DATETIME)

            call tCol%putval( stSubString )

          type is (T_DATA_COLUMN_STRING)  

            call tCol%putval( stSubstring )

          class default 

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

    do iIndex = 1, this%iCount

      stString = this%stColNames%value(iIndex)

      sChar = stString%asCharacter()

      write(*, "(/,a)") "Variable name: "//trim(sChar)


      write (*, fmt="(5x, a, i8)") "Count: ", int( this%col(iIndex)%count() )
      write (*, fmt="(7x, a, g15.5)") "Min: ", this%col(iIndex)%min()
      write (*, fmt="(7x, a, g15.5)") "Max: ", this%col(iIndex)%max()
      write (*, fmt="(7x, a, g15.5)") "Sum: ", this%col(iIndex)%sum()
      write (*, fmt="(7x, a, g15.5)") "Mean: ", this%col(iIndex)%mean()

     enddo 


  end subroutine summarize_data_frame_sub


end module data_frame