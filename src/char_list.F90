module char_list

  use iso_c_binding, only : c_int
  implicit none


  type T_CHARLIST
    character (len=:), allocatable        :: s
    class (T_CHARLIST), pointer           :: next => null()
  contains

  end type T_CHARLIST



    public :: clist_last
    public :: clist_create
    public :: clist_append
    public :: clist_print

contains

  function clist_create( sChar )       result(pListElement)

    character (len=*), intent(in)    :: sChar
    type (T_CHARLIST), pointer       :: pListElement

    allocate(pListElement)

    pListElement%s = sChar
    pListElement%next => null()

  end function clist_create 


  subroutine clist_append(pListElement, sChar)

    type (T_CHARLIST), intent(inout), pointer     :: pListElement
    character (len=*), intent(in)                 :: sChar

    ! [ LOCALS ] 
    type (T_CHARLIST), pointer                :: pNewElement
    type (T_CHARLIST), pointer                :: pLast
    integer (kind=c_int)                      :: iStat
    integer (kind=c_int)                      :: iIndex
    integer (kind=c_int)                      :: iLen

    iLen = len_trim(sChar)

    pLast => clist_last(pListElement)
    allocate( pNewElement, stat=iStat )
    pNewElement = T_CHARLIST( sChar, null() )
    pLast%next => pNewElement

  end subroutine clist_append


  subroutine clist_print(pList)

    type (T_CHARLIST), intent(in), pointer    :: pList

    ! [ LOCALS ]
    character (len=:), allocatable        :: sChar
    type (T_CHARLIST), pointer            :: pCurrent

    if (associated( pList )) then

      pCurrent => pList

      do while ( associated( pCurrent) )

        sChar = trim( clist_getstring( pCurrent ) )
 
        print *, trim( sChar )

        pCurrent => pCurrent%next

      enddo  

    endif 

  end subroutine clist_print


  function clist_last(pListElement)   result(pLast)

    class (T_CHARLIST), intent(inout), pointer   :: pListElement
    class (T_CHARLIST), pointer                  :: pLast

    pLast => pListElement

    do while ( associated(pLast%next) )

      pLast => pLast%next

    enddo  

  end function clist_last

  function clist_getstring(pListElement)   result(sChar)

    type (T_CHARLIST), intent(in), pointer     :: pListElement
    character (len=256)                         :: sChar

    ! [ LOCALS ]
    integer (kind=c_int) :: iLen
    integer (kind=c_int) :: iIndex

    if ( associated(pListElement) ) then

      sChar = trim( pListElement%s )

    else

      sChar = "<none>"  

    endif


  end function clist_getstring


end module char_list