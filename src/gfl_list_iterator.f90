MODULE gfcl_list_iterator
  !--- USE statements ------------------------------------------------
  USE gfcl_iterators
  USE gfcl_list_node
  !--- Implicit statement --------------------------------------------
  IMPLICIT NONE
  !--- Private/public section ----------------------------------------
  PRIVATE
  PUBLIC :: advance
  PUBLIC :: ListIterator
  !--- Data types ----------------------------------------------------
  ! a list iterator used to tranverse the list and point to objects to
  ! insert or delete.
  TYPE, EXTENDS(BidirectionalIterator) :: ListIterator
     !--- Component part
     TYPE(ListNode), POINTER :: tp_node_ => NULL()
     
   CONTAINS
     PRIVATE
     !--- Type-bound-procedure part
     ! initialisors ====================================================
     PROCEDURE         :: initialise_void__
     PROCEDURE         :: initialise_copy__
     GENERIC  , PUBLIC :: initialise       => initialise_void__, &
                                              initialise_copy__

     ! comparators =====================================================
     PROCEDURE :: equal_    => equal__
     PROCEDURE :: notequal_ => notequal__

     ! modifiers =======================================================
     PROCEDURE :: next_     => next__
     PROCEDURE :: prev_     => prev__

     PROCEDURE, PUBLIC :: get     => get__
     PROCEDURE, PUBLIC :: set     => set__
  END TYPE ListIterator

  !--- Interfaces ------------------------------------------------------
  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE assign, assign_node
  END INTERFACE

CONTAINS

  ! creates an empty iterator
  SUBROUTINE initialise_void__(t_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    NULLIFY(t_this%tp_node_)
  END SUBROUTINE initialise_void__

  ! creates a copy from an iterator
  SUBROUTINE initialise_copy__(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout)         :: t_this
    CLASS(ListIterator), INTENT(in)   , TARGET :: t_that
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_that%tp_node_
  END SUBROUTINE initialise_copy__

  SUBROUTINE final__(t_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    NULLIFY(t_this%tp_node_)
  END SUBROUTINE final__

  ! next
  SUBROUTINE next__(t_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_this%tp_node_%tp_next_
  END SUBROUTINE next__

  ! dec
  SUBROUTINE prev__(t_this)
    ! --- Declaration of arguments -------------------------------------
    ! decrements the iterator it and returns it
    CLASS(ListIterator), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_this%tp_node_%tp_prev_
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
       b = ASSOCIATED(t_this%tp_node_,t_that%tp_node_)
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
       b = .NOT.ASSOCIATED(t_this%tp_node_,t_that%tp_node_)
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
    t_this%tp_node_ => t_that%tp_node_
  END SUBROUTINE assign

  ! assign_node
  SUBROUTINE assign_node(t_this,t_node)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(out)        :: t_this
    TYPE(ListNode)    , INTENT(in), TARGET :: t_node
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_node
  END SUBROUTINE assign_node

  ! get_at
  FUNCTION get__(t_this) RESULT(tp_value)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(in) :: t_this
    CLASS(*)           , POINTER    :: tp_value
    ! --- Executable Code ----------------------------------------------
    tp_value => t_this%tp_node_%ta_data_
  END FUNCTION get__

  ! set_at
  SUBROUTINE set__(t_this,t_value)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ListIterator), INTENT(inout) :: t_this
    CLASS(*)           , INTENT(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    CALL t_this%tp_node_%set(t_value)
  END SUBROUTINE set__

  ! transfer
  SUBROUTINE transfer(t_this,t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(inout) :: t_this,t_first,t_last
    ! --- Executable Code ----------------------------------------------
    CALL t_this%tp_node_%transfer(t_first%tp_node_,t_last%tp_node_)
  END SUBROUTINE transfer
  
  ! distance
  FUNCTION distance(t_first,t_last) RESULT(n)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), VALUE :: t_first,t_last
    INTEGER                   :: n
    ! --- Executable Code ----------------------------------------------
    n = 0
    DO WHILE (t_first /= t_last)
       n = n + 1
       CALL t_first%next()
    END DO
  END FUNCTION distance

END MODULE gfcl_list_iterator
