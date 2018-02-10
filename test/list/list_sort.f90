program list_merge
  !--- USE statements ------------------------------------------------
  use gfcl_list
  use gfcl_list_iterator
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Declaration of variables  -------------------------------------
  type(List) :: lst
  !--- Initialise variables ------------------------------------------
  call lst%initialise( [ 8,7,5,9,0,1,3,2,6,4 ] )
  !--- Executable code -----------------------------------------------
  write(*,'(A)',advance='no') "before:    "
  call display_list(lst)
  call lst%sort(bp1)
  write(*,'(A)',advance='no') "ascending: "
  call display_list(lst)
  call lst%sort(bp2)
  write(*,'(A)',advance='no') "descending:"
  call display_list(lst)

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

  logical function bp1(a,b)
    class(*), intent(in) :: a,b
    select type (a)
    type is (integer)
       select type (b)
       type is (integer)
          bp1 = a<b
          return
       end select
    end select
    bp1 = .false.
  end function bp1

  logical function bp2(a,b)
    class(*), intent(in) :: a,b
    select type (a)
    type is (integer)
       select type (b)
       type is (integer)
          bp2 = a>b
          return
       end select
    end select
    bp2 = .false.
  end function bp2

end program list_merge
