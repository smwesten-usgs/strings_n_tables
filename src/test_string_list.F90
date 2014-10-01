program test_string_list

  use iso_c_binding, only : c_int, c_double, c_float, c_bool
  use types
  use strings
  use exceptions
  use string_list

  type (STRING_LIST_T) :: mylist

  print *, __FILE__,": ", __LINE__
  call mylist%append("One")
  print *, __FILE__,": ", __LINE__  
  call mylist%append("Two")
  print *, __FILE__,": ", __LINE__  
  call mylist%append("Three")
  print *, __FILE__,": ", __LINE__  

  call mylist%print()


end program test_string_list