program list_splice
  !--- USE statements ------------------------------------------------
  use gfcl_list
  use gfcl_list_iterator
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Declaration of variables  -------------------------------------
  type(List) :: list1, list2
  type(ListIterator) :: it
  !--- Initialise variables ------------------------------------------
  call list1%initialise( [  1,  2,  3,  4,  5 ] )
  call list2%initialise( [ 10, 20, 30, 40, 50 ] )
  it = list1%begin()
  !--- Executable code -----------------------------------------------
  call advance(it,2)
  call list1%splice(it,list2)
  write(*,'(A)',advance='no') "list1:"
  call display_list(list1)
  write(*,'(A)',advance='no') "list2:"
  call display_list(list2)

  call list2%splice(list2.begin(),it,list1.end())
  write(*,'(A)',advance='no') "list1:"
  call display_list(list1)
  write(*,'(A)',advance='no') "list2:"
  call display_list(list2)

contains
  subroutine display_list(lst)
    use gfcl_list
    use gfcl_list_iterator
    !--- Declaration of variables  -------------------------------------
    type(List), intent(in) :: lst
    !--- Declaration of variables  -------------------------------------
    type(ListIterator) :: it
    !--- Executable code -----------------------------------------------
    it = lst%begin()
    do while (it /= lst%end())
       select type(ptr => it%get())
          type is (integer)
          write(*,'(I3)', advance='no') ptr
       end select
       call it%next()
    end do
    write(*,*) ""
  end subroutine display_list

end program list_splice
