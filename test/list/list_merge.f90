program list_merge
  !--- USE statements ------------------------------------------------
  use gfcl_list
  use gfcl_list_iterator
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Declaration of variables  -------------------------------------
  type(List) :: list1, list2
  !--- Initialise variables ------------------------------------------
  call list1%initialise( [ 5, 9, 0, 1, 3 ] )
  call list2%initialise( [ 8, 7, 2, 6, 4 ] )
  !--- Executable code -----------------------------------------------
  call list1%sort(bp)
  call list2%sort(bp)

  write(*,'(A)',advance='no') "list1:  "
  call display_list(list1)
  write(*,'(A)',advance='no') "list2:  "
  call display_list(list2)

  call list1%merge(list2,bp)
  write(*,'(A)',advance='no') "merged: "
  call display_list(list1)
  
contains
  subroutine display_list(tList)
    use gfcl_list
    use gfcl_list_iterator
    !--- Declaration of variables  -------------------------------------
    type(List), intent(in) :: tList
    !--- Declaration of variables  -------------------------------------
    type(ListIterator) :: iterator
    integer, pointer :: pointer
    !--- Executable code -----------------------------------------------
    iterator = tList%begin()
    do while (iterator /= tList%end())
       select type(itr => iterator%get())
          type is (integer)
          write(*,'(I2)', advance='no') itr
       end select
       call iterator%next()
    end do
    write(*,*) ""
  end subroutine display_list

  logical function bp(a,b)
    class(*), intent(in) :: a,b
    select type (a)
    type is (integer)
       select type (b)
       type is (integer)
          bp = a<b
          return
       end select
    end select
    bp = .false.
  end function bp

end program list_merge
