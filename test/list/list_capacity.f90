program list_assign
  !--- USE statements ------------------------------------------------
  use gfcl_list
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Declaration of variables  -------------------------------------
  type(List) :: numbers
  !--- Initialise variables ------------------------------------------
  call numbers%initialise()
  !--- Executable code -----------------------------------------------
  print *, "Initially, numbers%empty(): ", numbers%empty()
  print *, "Initially, numbers%size() : ", numbers%size()
  print *, "Initially, size(numbers)  : ", size(numbers)
 
  call numbers%push_back(42);
  call numbers%push_back(13317); 

  print *, "After adding elements, numbers%empty(): ", numbers%empty()
  print *, "After adding elements, numbers%size() : ", numbers%size()
  print *, "After adding elements, size(numbers)  : ", size(numbers)
end program list_assign
