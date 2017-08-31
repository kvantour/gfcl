MODULE gfcl_list_node
  !--- USE statements ------------------------------------------------
  !--- Implicit statement --------------------------------------------
  IMPLICIT NONE
  !--- Private/public section ----------------------------------------
  PRIVATE
  !--- Data types ----------------------------------------------------
  ! An actual list node, containing the list element, and pointers to
  ! the previous and next node in the list.
  TYPE, PUBLIC :: ListNode
     !--- Component part
     PRIVATE
     CLASS(*)       , ALLOCATABLE :: ta_data_
     CLASS(ListNode), POINTER     :: tp_next_ => NULL()
     CLASS(ListNode), POINTER     :: tp_prev_ => NULL()

   CONTAINS
     !--- Type-bound-procedure part
     PROCEDURE :: hook     => hook__
     PROCEDURE :: unhook   => unhook__
     PROCEDURE :: swap     => swap__
     PROCEDURE :: transfer => transfer__
     PROCEDURE :: reverse  => reverse__

     PROCEDURE :: get_next => get_next__
     PROCEDURE :: get_prev => get_prev__
     PROCEDURE :: get_data => get_data__
     PROCEDURE :: set_data => set_data__

     !     FINAL :: final___
  END TYPE ListNode
  !--- Interfaces ----------------------------------------------------               
CONTAINS

  SUBROUTINE hook__(t_node1, t_node2)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout),TARGET :: t_node1, t_node2
    ! --- Executable Code ----------------------------------------------
    t_node1%tp_next_          => t_node2
    t_node1%tp_prev_          => t_node2%tp_prev_
    t_node2%tp_prev_%tp_next_ => t_node1
    t_node2%tp_prev_          => t_node1
  END SUBROUTINE hook__

  ! remove node from cyclic linked list
  SUBROUTINE unhook__(t_node)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout) :: t_node
    ! --- Declaration of variables -------------------------------------
    CLASS(ListNode), POINTER :: tp_next, tp_prev
    ! --- Executable Code ----------------------------------------------
    ! remove links
    tp_next          => t_node%tp_next_
    tp_prev          => t_node%tp_prev_
    tp_prev%tp_next_ => tp_next
    tp_next%tp_prev_ => tp_prev
  END SUBROUTINE unhook__

  SUBROUTINE copy_data(t_node1,t_node2)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout) :: t_node1
    CLASS(ListNode), INTENT(in)    :: t_node2
    ! --- Executable Code ----------------------------------------------
    IF (ALLOCATED(t_node1%ta_data_)) DEALLOCATE(t_node1%ta_data_)
    ALLOCATE(t_node1%ta_data_,source=t_node2%ta_data_)
  END SUBROUTINE copy_data

  SUBROUTINE swap__(t_node1,t_node2)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout), TARGET :: t_node1,t_node2
    ! --- Declaration of variables -------------------------------------
    CLASS(ListNode), POINTER :: tp
    ! --- Executable Code ----------------------------------------------
    IF (.NOT.ASSOCIATED(t_node1%tp_next_,t_node1)) THEN
       IF (.NOT.ASSOCIATED(t_node2%tp_next_,t_node2)) THEN
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
       ELSE
          ! x not empty, y is
          t_node2%tp_next_ => t_node1%tp_next_
          t_node2%tp_prev_ => t_node1%tp_prev_

          t_node2%tp_next_%tp_prev_ => t_node2
          t_node2%tp_prev_%tp_next_ => t_node2

          t_node1%tp_next_ => t_node1
          t_node1%tp_prev_ => t_node1
       END IF
    ELSE IF (.NOT.ASSOCIATED(t_node2%tp_next_,t_node2)) THEN
       ! x is empty, y is not
       t_node1%tp_next_          => t_node2%tp_next_
       t_node1%tp_prev_          => t_node2%tp_prev_
       t_node1%tp_next_%tp_prev_ => t_node1
       t_node1%tp_prev_%tp_next_ => t_node1
       t_node2%tp_next_          => t_node2
       t_node2%tp_prev_          => t_node2
    END IF
  END SUBROUTINE swap__

  SUBROUTINE transfer__(t_node,t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout), TARGET :: t_node, t_first, t_last
    ! --- Declaration of variables -------------------------------------
    CLASS(ListNode), POINTER :: tp
    ! --- Executable Code ----------------------------------------------
    IF (.NOT.ASSOCIATED(t_node%tp_prev_%tp_next_,t_last)) THEN
       ! Remove [t_first,t_last) from its old position
       t_last %tp_prev_%tp_next_ => t_node
       t_first%tp_prev_%tp_next_ => t_last
       t_node %tp_prev_%tp_next_ => t_first
       ! Splice [t_first,t_last) into its new position
       tp               => t_node %tp_prev_
       t_node %tp_prev_ => t_last %tp_prev_
       t_last %tp_prev_ => t_first%tp_prev_
       t_first%tp_prev_ => tp
    END IF
  END SUBROUTINE transfer__

  SUBROUTINE reverse__(t_node)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout), TARGET :: t_node
    ! --- Declaration of variables -------------------------------------
    CLASS(ListNode), POINTER :: tp_1,tp_2
    ! --- Executable Code ----------------------------------------------
    tp_2 => t_node
    DO
       tp_1          => tp_2%tp_next_
       tp_2%tp_next_ => tp_2%tp_prev_
       tp_2%tp_prev_ => tp_1
       tp_2          => tp_2%tp_prev_
       IF (ASSOCIATED(tp_2,t_node)) EXIT
    END DO
  END SUBROUTINE reverse__

  FUNCTION get_next__(t_node) RESULT(tp)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout) :: t_node
    TYPE(ListNode) , POINTER       :: tp
    ! --- Executable Code ----------------------------------------------
    tp => t_node%tp_next_
  END FUNCTION get_next__

  FUNCTION get_prev__(t_node) RESULT(tp)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout) :: t_node
    TYPE(ListNode) , POINTER       :: tp
    ! --- Executable Code ----------------------------------------------
    tp => t_node%tp_prev_
  END FUNCTION get_prev__

  FUNCTION get_data__(t_node) RESULT(tp)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout), TARGET :: t_node
    CLASS(*)       , POINTER       :: tp
    ! --- Executable Code ----------------------------------------------
    tp => t_node%ta_data_
  END FUNCTION  get_data__

  SUBROUTINE set_data__(t_node,t_data)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListNode), INTENT(inout) :: t_node
    CLASS(*)       , INTENT(in   ) :: t_data
    ! --- Executable Code ----------------------------------------------
    IF (ALLOCATED(t_node%ta_data_)) DEALLOCATE(t_node%ta_data_)
    ALLOCATE(t_node%ta_data_,source=t_data)
  END SUBROUTINE set_data__

  SUBROUTINE final___(t_node)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListNode), INTENT(inout) :: t_node
    ! --- Executable Code ----------------------------------------------
    IF (ALLOCATED(t_node%ta_data_)) DEALLOCATE(t_node%ta_data_)
  END SUBROUTINE final___

  
END MODULE gfcl_list_node
