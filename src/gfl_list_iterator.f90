MODULE gfcl_list_iterator
  !--- USE statements ------------------------------------------------
  USE gfcl_iterators
  USE gfcl_list_node
  !--- Implicit statement --------------------------------------------
  IMPLICIT NONE
  !--- Private/public section ----------------------------------------
  PRIVATE
  !--- Data types ----------------------------------------------------
  ! a list iterator used to tranverse the list and point to objects to
  ! insert or delete.
  TYPE, EXTENDS(BidirectionalIterator) :: ListIterator
     !--- Component part
     PRIVATE
     TYPE(ListNode), POINTER :: tp_node => NULL()
     
   CONTAINS
     !--- Type-bound-procedure part
     PRIVATE
     PROCEDURE :: equal_    => equal__
     PROCEDURE :: notequal_ => notequal__
     PROCEDURE :: next_     => next__
     PROCEDURE :: prev_     => prev__

     PROCEDURE :: initial_void_ => initial_void__
     PROCEDURE :: initial_copy_ => initial_copy__
     
     GENERIC, PUBLIC :: initial => initial_void_, initial_copy_
  END TYPE ListIterator

  !--- Interfaces ----------------------------------------------------               
CONTAINS

  ! creates an empty iterator
  SUBROUTINE initial_void__(t_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    NULLIFY(t_this%tp_node)
  END SUBROUTINE initial_void__

  ! creates a copy from an iterator
  SUBROUTINE initial_copy__(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout)         :: t_this
    CLASS(ListIterator), INTENT(in)   , TARGET :: t_that
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node => t_that%tp_node
  END SUBROUTINE initial_copy__

  SUBROUTINE final__(t_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    NULLIFY(t_this%tp_node)
  END SUBROUTINE final__

  ! next
  SUBROUTINE next__(t_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node => t_this%tp_node%get_next()
  END SUBROUTINE next__

  ! dec
  SUBROUTINE prev__(t_this)
    ! --- Declaration of arguments -------------------------------------
    ! decrements the iterator it and returns it
    CLASS(ListIterator), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node => t_this%tp_node%get_prev()
  END SUBROUTINE prev__

  ! eq
  FUNCTION equal__(t_this,t_that) RESULT(b)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    CLASS(ListIterator)   , INTENT(in) :: t_this
    CLASS(ForwardIterator), INTENT(in) :: t_that
    LOGICAL                            :: b
    ! --- Executable Code ----------------------------------------------
    SELECT TYPE (t_that)
    TYPE is (ListIterator)
       b = ASSOCIATED(t_this%tp_node,t_that%tp_node)
    CLASS default
       b = .FALSE.
    END SELECT
  END FUNCTION equal__

  ! neq
  FUNCTION notequal__(t_this,t_that) RESULT(b)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    CLASS(ListIterator)   , INTENT(in) :: t_this
    CLASS(ForwardIterator), INTENT(in) :: t_that
    LOGICAL                            :: b
    ! --- Executable Code ----------------------------------------------
    SELECT TYPE (t_that)
    TYPE IS (ListIterator)
       b = .NOT.ASSOCIATED(t_this%tp_node,t_that%tp_node)
    CLASS default
       b = .TRUE.
    END SELECT
  END FUNCTION notequal__

  ! assign
  SUBROUTINE assign(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    ! make t_this and t_that equal iterators
    TYPE(ListIterator), INTENT(out) :: t_this
    TYPE(ListIterator), INTENT(in)  :: t_that
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node => t_that%tp_node
  END SUBROUTINE assign

  ! assign_node
  SUBROUTINE assign_node(t_this,t_node)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(out)        :: t_this
    TYPE(ListNode)    , INTENT(in), TARGET :: t_node
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node => t_node
  END SUBROUTINE assign_node

  ! get_at
  FUNCTION get_at(t_this) RESULT(tp_data)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(in) :: t_this
    CLASS(*)          , POINTER    :: tp_data
    ! --- Executable Code ----------------------------------------------
    tp_data => t_this%tp_node%get_data()
  END FUNCTION get_at

  ! set_at
  SUBROUTINE set_at(t_this,t_data)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(inout) :: t_this
    CLASS(*)          , INTENT(in)    :: t_data
    ! --- Executable Code ----------------------------------------------
    CALL t_this%tp_node%set_data(t_data)
  END SUBROUTINE set_at

  ! transfer
  SUBROUTINE transfer(t_this,t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(inout) :: t_this,t_first,t_last
    ! --- Executable Code ----------------------------------------------
    CALL list_node_transfer(t_this%tp_node,t_first%tp_node,t_last%tp_node)
  END SUBROUTINE transfer
  
  ! distance
  FUNCTION distance(t_first,t_last) RESULT(n)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(in) :: t_first,t_last
    INTEGER  :: n
    TYPE(ListIterator) :: itr
    ! --- Executable Code ----------------------------------------------
    n = 0
    itr = t_first
    DO WHILE (itr /= t_last)
       n = n+1
       CALL itr%next()
    END DO
  END FUNCTION  distance

END MODULE gfcl_list_iterator
