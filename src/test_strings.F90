program test_strings

use strings
use types
use string_list
use iso_c_binding, only : c_int
implicit none

type (T_STRING) :: st1
type (T_STRING) :: st2
type (T_STRING) :: st3
type (T_STRING) :: st4
type (T_STRING) :: st5
type (T_STRING) :: st6
type (T_STRING) :: st, st_t

logical (kind=c_bool), dimension(6) :: lr

type (T_STRING), dimension(50) :: sta

type (T_STRING_LIST) :: stl, stl2

character (len=2042) :: sChar
integer (kind=c_int) :: iIndex
character (len=32) :: sBuf

st1 = "3.141592654"
st2 = "WD-40"
st3 = "6.02214078E+23"
st4 = "46"
st5 = "(608) 821-3888"
st6 = "2.344e-7"

call stl%append(st1)
call stl%append(st2)
call stl%append(st3)
call stl%append(st4)
call stl%append(st5)
call stl%append(st6)

do iIndex=1,ubound(lr,1)
  st = stl%value(iIndex)	
  lr(iIndex) = st%isNumeric()
enddo

write(*, fmt="(/,/,a)") "Tests of isNumeric"
write(*, fmt="(a)") repeat("-", 40)
write(*, fmt="(a,t25x,a)") "value", "isNumeric?"
write(*, fmt="(a)") repeat("-", 40)

do iIndex=1,ubound(lr,1)
  st = stl%value(iIndex)	
  write(*, fmt="(a,t30,l)") st%asCharacter(), lr(iIndex)
enddo

write(*, fmt="(/,'[',a,']',a,/,/)") pf(    ( lr(1)) &
                                     .and. (.not. lr(2)) &
                                     .and. ( lr(3)) &
	                                   .and. lr(4) &
	                                   .and. (.not. lr(5)) &
	                                   .and. ( lr(6)) ), &
	                                   "  { test of isNumeric }"


do iIndex=1,ubound(lr,1)
  st = stl%value(iIndex)	
  lr(iIndex) = st%isInteger()
enddo

write(*, fmt="(/,/,a)") "Tests of isInteger"
write(*, fmt="(a)") repeat("-", 40)
write(*, fmt="(a,t25x,a)") "value", "isInteger?"
write(*, fmt="(a)") repeat("-", 40)

do iIndex=1,ubound(lr,1)
  st = stl%value(iIndex)	
  write(*, fmt="(a,t30,l)") st%asCharacter(), lr(iIndex)
enddo

write(*, fmt="(/,'[',a,']',a,/,/)") pf(        (.not. lr(1)) &
                                     .and. (.not. lr(2)) &
                                     .and. (.not. lr(3)) &
	                                   .and. lr(4) &
	                                   .and. (.not. lr(5)) &
	                                   .and. (.not. lr(6)) ), &
	                                    "  { test of isInteger }"



write(*, fmt="(/,/,a)") "Tests of asInt"
write(*, fmt="(a)") repeat("-", 40)
write(*, fmt="(a,t25x,a)") "value", "asInt"
write(*, fmt="(a)") repeat("-", 40)

do iIndex=1,ubound(lr,1)
  st = stl%value(iIndex)	
  write(*, fmt="(a,t24,i12)") st%asCharacter(), st%asInt()
enddo



write(*, fmt="(/,/,a)") "Tests of asFloat"
write(*, fmt="(a)") repeat("-", 40)
write(*, fmt="(a,t25x,a)") "value", "asFloat"
write(*, fmt="(a)") repeat("-", 40)

do iIndex=1,ubound(lr,1)
  st = stl%value(iIndex)	
  write(*, fmt="(a,t24,g14.8)") st%asCharacter(), st%asFloat()
enddo



write(*, fmt="(/,/,a)") "Tests of asDouble"
write(*, fmt="(a)") repeat("-", 40)
write(*, fmt="(a,t25x,a)") "value", "asDouble"
write(*, fmt="(a)") repeat("-", 40)

do iIndex=1,ubound(lr,1)
  st = stl%value(iIndex)	
  write(sBuf, fmt="(g16.10)") st%asDouble()
  st_t = sBuf
  write(*, fmt="(a,t24,g16.10,t42)") st%asCharacter(), st%asDouble()
enddo

write(*, fmt="(/,a)")  "Test of 'remove': remove ':;/,'"
write(*, fmt="(a)") repeat("-", 40)
st1 = "::The file, a small one, is located at c:/my/data/lives/here;"

st2 = st1%remove(":/;,")

write(*, fmt="(a)")  st1%asCharacter()
write(*, fmt="(a)")  st2%asCharacter()


write(*, fmt="(/,a)")  "Test of list navigation functions"
write(*, fmt="(a)") repeat("-", 40)

st = stl%first()
write(*, fmt="(a)")  "First item in string list: "//st%asCharacter()

st = stl%last()
write(*, fmt="(a)")  "Last item in string list: "//st%asCharacter() 

st = stl%previous()
write(*, fmt="(a)")  "Previous item in list: "//st%asCharacter()

st = stl%previous()
write(*, fmt="(a)")  "Previous item in list: "//st%asCharacter()

st = stl%previous()
write(*, fmt="(a)")  "Previous item in list: "//st%asCharacter()

st = stl%previous()
write(*, fmt="(a)")  "Previous item in list: "//st%asCharacter()

st = stl%previous()
write(*, fmt="(a)")  "Previous item in list: "//st%asCharacter()

end program test_strings