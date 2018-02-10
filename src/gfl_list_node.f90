module gfcl_list_node
  !--- USE statements ------------------------------------------------
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Private/public section ----------------------------------------
  private
  !--- Data types ----------------------------------------------------
  ! An actual list node, containing the list element, and pointers to
  ! the previous and next node in the list.
  type, public :: ListNode
     !--- Component part
     class(*)       , allocatable :: ta_data_
     class(ListNode), pointer     :: tp_next_ => null()
     class(ListNode), pointer     :: tp_prev_ => null()

   contains
     !--- Type-bound-procedure part
     procedure :: hook     => hook__
     procedure :: unhook   => unhook__
     procedure :: swap     => swap__
     procedure :: transfer => transfer__
     procedure :: reverse  => reverse__

     procedure :: next => next__
     procedure :: prev => prev__
     procedure :: get  => get__
     procedure :: set  => set__

     final :: final___
  end type ListNode
  !--- Interfaces ----------------------------------------------------               
contains

  subroutine hook__(t_node1, t_node2)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout),target :: t_node1, t_node2
    ! --- Executable Code ----------------------------------------------
    t_node1%tp_next_          => t_node2
    t_node1%tp_prev_          => t_node2%tp_prev_
    t_node2%tp_prev_%tp_next_ => t_node1
    t_node2%tp_prev_          => t_node1
  end subroutine hook__

  ! remove node from cyclic linked list
  subroutine unhook__(t_node)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout) :: t_node
    ! --- Declaration of variables -------------------------------------
    class(ListNode), pointer :: tp_next, tp_prev
    ! --- Executable Code ----------------------------------------------
    ! remove links
    tp_next          => t_node%tp_next_
    tp_prev          => t_node%tp_prev_
    tp_prev%tp_next_ => tp_next
    tp_next%tp_prev_ => tp_prev
  end subroutine unhook__

  subroutine construct_node__(t_node,t_value)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode) , intent(inout) :: t_node
    class(*)        , intent(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    if (allocated(t_node%ta_data_)) deallocate(t_node%ta_data_)
    allocate(t_node%ta_data_,source=t_value)
  end subroutine construct_node__

  subroutine swap__(t_node1,t_node2)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout), target :: t_node1,t_node2
    ! --- Declaration of variables -------------------------------------
    class(ListNode), pointer :: tp
    ! --- Executable Code ----------------------------------------------
    if (.not.associated(t_node1%tp_next_,t_node1)) then
       if (.not.associated(t_node2%tp_next_,t_node2)) then
          ! both t_node1 and y are not empty
          tp               => t_node1%tp_next_
          t_node1%tp_next_ => t_node2%tp_next_
          t_node2%tp_next_ => tp
          tp               => t_node1%tp_prev_
          t_node1%tp_prev_ => t_node2%tp_prev_
          t_node2%tp_prev_ => tp

          t_node1%tp_next_%tp_prev_ => t_node1
          t_node1%tp_prev_%tp_next_ => t_node1
          t_node2%tp_next_%tp_prev_ => t_node2
          t_node2%tp_prev_%tp_next_ => t_node2
       else
          ! x not empty, y is
          t_node2%tp_next_ => t_node1%tp_next_
          t_node2%tp_prev_ => t_node1%tp_prev_

          t_node2%tp_next_%tp_prev_ => t_node2
          t_node2%tp_prev_%tp_next_ => t_node2

          t_node1%tp_next_ => t_node1
          t_node1%tp_prev_ => t_node1
       end if
    else if (.not.associated(t_node2%tp_next_,t_node2)) then
       ! x is empty, y is not
       t_node1%tp_next_          => t_node2%tp_next_
       t_node1%tp_prev_          => t_node2%tp_prev_
       t_node1%tp_next_%tp_prev_ => t_node1
       t_node1%tp_prev_%tp_next_ => t_node1
       t_node2%tp_next_          => t_node2
       t_node2%tp_prev_          => t_node2
    end if
  end subroutine swap__

  subroutine transfer__(t_node,t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout), target :: t_node, t_first, t_last
    ! --- Declaration of variables -------------------------------------
    class(ListNode), pointer :: tp
    ! --- Executable Code ----------------------------------------------
    if (.not.associated(t_node%tp_prev_%tp_next_,t_last)) then
       ! Remove [t_first,t_last) from its old position
       t_last %tp_prev_%tp_next_ => t_node
       t_first%tp_prev_%tp_next_ => t_last
       t_node %tp_prev_%tp_next_ => t_first
       ! Splice [t_first,t_last) into its new position
       tp               => t_node %tp_prev_
       t_node %tp_prev_ => t_last %tp_prev_
       t_last %tp_prev_ => t_first%tp_prev_
       t_first%tp_prev_ => tp
    end if
  end subroutine transfer__

  subroutine reverse__(t_node)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout), target :: t_node
    ! --- Declaration of variables -------------------------------------
    class(ListNode), pointer :: tp_1,tp_2
    ! --- Executable Code ----------------------------------------------
    tp_2 => t_node
    do
       tp_1          => tp_2%tp_next_
       tp_2%tp_next_ => tp_2%tp_prev_
       tp_2%tp_prev_ => tp_1
       tp_2          => tp_2%tp_prev_
       if (associated(tp_2,t_node)) exit
    end do
  end subroutine reverse__

  function next__(t_node) result(tp)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout) :: t_node
    class(ListNode), pointer       :: tp
    ! --- Executable Code ----------------------------------------------
    tp => t_node%tp_next_
  end function next__

  function prev__(t_node) result(tp)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout) :: t_node
    class(ListNode), pointer       :: tp
    ! --- Executable Code ----------------------------------------------
    tp => t_node%tp_prev_
  end function prev__

  function get__(t_node) result(tp)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout), target :: t_node
    class(*)       , pointer       :: tp
    ! --- Executable Code ----------------------------------------------
    tp => t_node%ta_data_
  end function get__

  subroutine set__(t_node,t_value)
    ! --- Declaration of arguments -------------------------------------
    class(ListNode), intent(inout) :: t_node
    class(*)       , intent(in   ) :: t_value
    ! --- Executable Code ----------------------------------------------
    if (allocated(t_node%ta_data_)) deallocate(t_node%ta_data_)
    allocate(t_node%ta_data_,source=t_value)
  end subroutine set__

  subroutine final___(t_node)
    ! --- Declaration of arguments -------------------------------------
    type(ListNode), intent(inout) :: t_node
    ! --- Executable Code ----------------------------------------------
    if (allocated(t_node%ta_data_)) deallocate(t_node%ta_data_)
  end subroutine final___

  
end module gfcl_list_node
