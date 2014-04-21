program testchar

  use iso_c_binding, only : c_float
  use strings2
  use char_list

  character (len=:), allocatable :: c1, c2, c3, c4

  type (T_CHARLIST), pointer :: plist

  allocate (pList) 	

  c1 = "String 1"
  c2 = "String 2"
  c3 = c1 // " "//c2

  print *, c1
  print *, c2
  print *, c3

  c3 = "This is a completely new string"

  print *, c3

  c4 =  c1 + " " + c2 + " " + 42 + " " + 3.141593_c_float

  print *, c4

  print *, __FILE__, ": ", __LINE__
  
  call clist_append(plist, c1)
  print *, __FILE__, ": ", __LINE__

  call clist_append(plist, c2)
  print *, __FILE__, ": ", __LINE__  
  
  call clist_append(plist, c3)
  print *, __FILE__, ": ", __LINE__

  call clist_append(plist, c4)

  call clist_print(pList)


end program testchar