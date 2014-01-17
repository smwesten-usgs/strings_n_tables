program test

use strings
use iso_c_binding, only : c_int
implicit none

type (T_STRING) :: st1
type (T_STRING) :: st2
type (T_STRING) :: st3
type (T_STRING) :: st4
type (T_STRING) :: st

type (T_STRING), dimension(50) :: sta

type (T_STRING_LIST) :: stl, stl2

character (len=2042) :: sChar
integer (kind=c_int) :: iIndex

sChar = "Four score and seven years ago our fathers brought forth on this continent a new nation, conceived in liberty, and dedicated to the proposition that all men are created equal." &
    //" Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battlefield of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this." &
    //"But, in a larger sense, we can not dedicate, we can not consecrate, we can not hallow this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract." &
    //" The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. " &
    //" It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion—that we here highly resolve that these dead shall not have died in vain—that this nation, under God, shall have a new birth of freedom—and that government of the people, by the people, for the people, shall not perish from the earth."

st = sChar

sChar = "49"
st1 = sChar

sChar = "Crime and Punishment"
st2 = sChar

st3 = st1 + st2
sChar = st3

print *, sChar

st3 = st3 + st1 + 27 + " " + 3.1315
sChar = st3
print *, sChar

print *, st3%asCharacter()

st4 = "6.022169E+27"

print *, st4%isNumeric()
print *, st4%isInteger()

iIndex = 1

do while( len(st) > 0)

  !sta(iIndex) = st%chomp(",")
  call stl2%append( st%chomp(",") )
  print *, stl2%sl(iIndex)%asCharacter()
  iIndex = iIndex + 1

enddo	



print *, sqrt(st1%asDouble())

print *, sqrt(st2%asDouble())

print *, st1%asCharacter()

print *, st2%asCharacter()
print *, st2%asUppercase()
print *, st2%asLowercase()
call st2%toUppercase()
print *, st2%asCharacter()

print *, st2 == st2
print *, st1 == st2

print *, st2 == "Crime and Punishment"
print *, "Crime and Punishment" == st2
print *, "Crime and Punishment" == st3

stl = stl2%grep("consecrate")

call stl%print()

end program test