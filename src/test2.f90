program test2

use iso_varying_string
implicit none

type (varying_string) :: st1
type (varying_string) :: st2
type (varying_string) :: st3

character (len=20) :: sChar1, sChar2, sChar3

sChar1 = "Nincom"
st1 = sChar1

sChar2 = "poop"
st2 = sChar2

st3 = st1 // st2
sChar3 = st3

print *, sChar3


end program test2