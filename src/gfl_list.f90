  !===================================================================
  !
  ! PROJECT : Generic Fortran Container Library
  !
  ! NAME : linked list
  !
  ! GFCL_LIST :
  !
  ! Lists are sequence containers that allow constant time insert and
  ! erase operations anywhere within the sequence, and iteration in
  ! both directions.
  !
  ! List containers are implemented as doubly-linked lists; Doubly
  ! linked lists can store each of the elements they contain in
  ! different and unrelated storage locations. The ordering is kept by
  ! the association to each element of a link to the element preceding
  ! it and a link to the element following it.
  !
  ! They are very similar to forward_list: The main difference being
  ! that forward_list objects are single-linked lists, and thus they
  ! can only be iterated forwards, in exchange for being somewhat
  ! smaller and more efficient.
  !
  ! Compared to other base standard sequence containers (array, vector
  ! and deque), lists perform generally better in inserting,
  ! extracting and moving elements in any position within the
  ! container for which an iterator has already been obtained, and
  ! therefore also in algorithms that make intensive use of these,
  ! like sorting algorithms.
  !
  ! The main drawback of lists and forward_lists compared to these
  ! other sequence containers is that they lack direct access to the
  ! elements by their position; For example, to access the sixth
  ! element in a list one has to iterate from a known position (like
  ! the beginning or the end) to that position, which takes linear
  ! time in the distance between these. They also consume some extra
  ! memory to keep the linking information associated to each element
  ! (which may be an important factor for large lists of small-sized
  ! elements).
  !
  ! USAGE :
  !===================================================================

MODULE gfcl_list_module
  !--- USE statements ------------------------------------------------
  USE gfcl_iterators
  !--- Implicit statement --------------------------------------------
  IMPLICIT NONE
  !--- Private/public section ----------------------------------------
  PRIVATE
  PUBLIC :: gfcl_list, gfcl_list_iterator

  !--- Data types ----------------------------------------------------

  !=== List node =====================================================
  ! An actual list node, containing the list element, and pointers to
  ! the previous and next node in the list.
  TYPE :: node_T
     CLASS(*)    , ALLOCATABLE :: c_data
     TYPE(node_T), POINTER     :: p_next => NULL()
     TYPE(node_T), POINTER     :: p_prev => NULL()

   CONTAINS
     FINAL :: node_finalize
  END TYPE node_T

  !=== linked list ===================================================
  ! The gfcl_list TYPE is a standard container with linear time access
  ! to elements, and fixed time insertion/deletion at any point in the
  ! sequence.
  !
  ! This is a doubly linked list. Traversal up and down the list
  ! requires linear time, but adding and removing elements (or @e
  ! nodes) is done in constant time, regardless of where the change
  ! takes place.
  !
  ! The module std_list provides specialized algorithms unique to
  ! linked lists, such as splicing, sorting, and in-place reversal.
  !
  ! A list is conceptually represented as
  !
  !    A <---> B <---> C <---> D
  !
  ! however, it is actually circular; a link exists between A and D.
  ! The list TYPE holds (as its only data member) a private list_node
  ! referencing D, not A!  To get to the head of the list, we start at
  ! the tail and move forward by one. When this member iterator's
  ! next/previous pointers refer to itself, the list is empty.
  TYPE :: gfcl_list
!     PRIVATE
     TYPE(node_T) :: t_node
     
  END TYPE gfcl_list

  !=== List iterator =================================================
  ! a list iterator used to tranverse the list and point to objects to
  ! insert or delete.
  TYPE, EXTENDS(bidirectional_iterator) :: gfcl_list_iterator
     PRIVATE
     TYPE(node_T), POINTER :: p_node => NULL()
     
   CONTAINS
     PROCEDURE       :: initial_void => list_iterator_initial
     PROCEDURE       :: initial_copy => list_iterator_initial_copy

     PROCEDURE         :: equal    => list_iterator_eq
     PROCEDURE         :: notequal => list_iterator_neq
     PROCEDURE, PUBLIC :: next     => list_iterator_next
     PROCEDURE, PUBLIC :: prev     => list_iterator_prev
     
     GENERIC, PUBLIC :: initial => initial_void, initial_copy
  END TYPE gfcl_list_iterator
  
  !--- Interfaces ----------------------------------------------------               
CONTAINS
  !--- source --------------------------------------------------------

  ! add node1 before node2
  SUBROUTINE node_hook(t_node1, t_node2)
    ! --- Declaration of arguments -------------------------------------
    TYPE(node_T), INTENT(inout),TARGET :: t_node1, t_node2
    ! --- Executable Code ----------------------------------------------
    t_node1%p_next        => t_node2
    t_node1%p_prev        => t_node2%p_prev
    t_node2%p_prev%p_next => t_node1
    t_node2%p_prev        => t_node1
  END SUBROUTINE node_hook

  ! remove node from cyclic linked list
  SUBROUTINE node_unhook(t_node)
    ! --- Declaration of arguments -------------------------------------
    TYPE(node_T), INTENT(inout) :: t_node
    ! --- Declaration of variables -------------------------------------
    TYPE(node_T), POINTER :: p_next, p_prev
    ! --- Executable Code ----------------------------------------------
    ! remove links
    p_next        => t_node%p_next
    p_prev        => t_node%p_prev
    p_prev%p_next => p_next
    p_next%p_prev => p_prev
  END SUBROUTINE node_unhook

  SUBROUTINE node_copy_data(t_node1,t_node2)
    ! --- Declaration of arguments -------------------------------------
    TYPE(node_T), INTENT(inout) :: t_node1
    TYPE(node_T), INTENT(in)    :: t_node2
    ! --- Executable Code ----------------------------------------------
    IF (ALLOCATED(t_node1%c_data)) DEALLOCATE(t_node1%c_data)
    ALLOCATE(t_node1%c_data,source=t_node2%c_data)
  END SUBROUTINE node_copy_data

  SUBROUTINE node_swap(t_node1,t_node2)
    ! --- Declaration of arguments -------------------------------------
    TYPE(node_T), INTENT(inout), TARGET :: t_node1,t_node2
    ! --- Declaration of variables -------------------------------------
    TYPE(node_T), POINTER :: t
    ! --- Executable Code ----------------------------------------------
    IF (.NOT.ASSOCIATED(t_node1%p_next,t_node1)) THEN
       IF (.NOT.ASSOCIATED(t_node2%p_next,t_node2)) THEN
          ! both t_node1 and y are not empty
          t              => t_node1%p_next
          t_node1%p_next => t_node2%p_next
          t_node2%p_next => t
          t              => t_node1%p_prev
          t_node1%p_prev => t_node2%p_prev
          t_node2%p_prev => t

          t_node1%p_next%p_prev => t_node1
          t_node1%p_prev%p_next => t_node1
          t_node2%p_next%p_prev => t_node2
          t_node2%p_prev%p_next => t_node2
       ELSE
          ! x not empty, y is
          t_node2%p_next => t_node1%p_next
          t_node2%p_prev => t_node1%p_prev

          t_node2%p_next%p_prev => t_node2
          t_node2%p_prev%p_next => t_node2

          t_node1%p_next        => t_node1
          t_node1%p_prev        => t_node1
       END IF
    ELSE IF (.NOT.ASSOCIATED(t_node2%p_next,t_node2)) THEN
       ! x is empty, y is not
       t_node1%p_next        => t_node2%p_next
       t_node1%p_prev        => t_node2%p_prev
       t_node1%p_next%p_prev => t_node1
       t_node1%p_prev%p_next => t_node1
       t_node2%p_next        => t_node2
       t_node2%p_prev        => t_node2
    END IF
  END SUBROUTINE node_swap

  SUBROUTINE node_transfer(t_node,t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    TYPE(node_T), INTENT(inout), TARGET :: t_node, t_first, t_last
    ! --- Declaration of variables -------------------------------------
    TYPE(node_T), POINTER :: t
    ! --- Executable Code ----------------------------------------------
    IF (.NOT.ASSOCIATED(t_node%p_prev%p_next,t_last)) THEN
       ! Remove [t_first,t_last) from its old position
       t_last %p_prev%p_next => t_node
       t_first%p_prev%p_next => t_last
       t_node %p_prev%p_next => t_first
       ! Splice [t_first,t_last) into its new position
       t              => t_node %p_prev
       t_node %p_prev => t_last %p_prev
       t_last %p_prev => t_first%p_prev
       t_first%p_prev => t
    END IF
  END SUBROUTINE node_transfer

  SUBROUTINE node_reverse(t_node)
    ! --- Declaration of arguments -------------------------------------
    TYPE(node_T), INTENT(inout), TARGET :: t_node
    ! --- Declaration of variables -------------------------------------
    TYPE(node_T), POINTER :: t,tmp
    ! --- Executable Code ----------------------------------------------
    tmp => t_node
    DO
       t          => tmp%p_next
       tmp%p_next => tmp%p_prev
       tmp%p_prev => t
       tmp        => tmp%p_prev
       IF (ASSOCIATED(tmp,t_node)) EXIT
    END DO
  END SUBROUTINE node_reverse

  SUBROUTINE node_finalize(t_node)
    TYPE(node_T), INTENT(inout) :: t_node
    IF (ALLOCATED(t_node%c_data)) DEALLOCATE(t_node%c_data)
  END SUBROUTINE node_finalize

  !===================================================================
  !                         LIST ITERATOR
  !
  ! General list iterator functions :
  !
  ! list_iterator_inc       increments iterator
  ! list_iterator_inc_f     increments iterator
  ! list_iterator_dec       decrements iterator
  ! list_iterator_dec_f     decrements iterator
  ! list_iterator_eq        compares equality of iterators
  ! list_iterator_neq       compares if iterators are different
  ! list_iterator_assign    assigns content of it2 to it1
  ! list_iterator_assign_node assign node to an iterator
  ! list_iterator_transfer
  !===================================================================

  ! creates an empty iterator
  SUBROUTINE list_iterator_initial(c_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(gfcl_list_iterator), INTENT(inout) :: c_this
    NULLIFY(c_this%p_node)
  END SUBROUTINE list_iterator_initial

  ! creates a copy from an iterator
  SUBROUTINE list_iterator_initial_copy(c_this,c_that)
    ! --- Declaration of arguments -------------------------------------
    CLASS(gfcl_list_iterator), INTENT(inout)         :: c_this
    CLASS(gfcl_list_iterator), INTENT(in)   , TARGET :: c_that
    c_this%p_node => c_that%p_node
  END SUBROUTINE list_iterator_initial_copy

  SUBROUTINE list_iterator_final(c_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(gfcl_list_iterator), INTENT(inout) :: c_this
    NULLIFY(c_this%p_node)
  END SUBROUTINE list_iterator_final

  ! list_iterator_next
  SUBROUTINE list_iterator_next(c_this)
    ! --- Declaration of arguments -------------------------------------
    CLASS(gfcl_list_iterator), INTENT(inout) :: c_this
    c_this%p_node => c_this%p_node%p_next
  END SUBROUTINE list_iterator_next

  ! list_iterator_dec
  SUBROUTINE list_iterator_prev(c_this)
    ! decrements the iterator it and returns it
    CLASS(gfcl_list_iterator), INTENT(inout) :: c_this
    c_this%p_node => c_this%p_node%p_prev
  END SUBROUTINE  list_iterator_prev

  ! list_iterator_eq
  FUNCTION list_iterator_eq(c_this,c_that) RESULT(b)
    ! returns true if it1 and it2 point to the same node
    CLASS(gfcl_list_iterator), INTENT(in) :: c_this
    CLASS(forward_iterator)  , INTENT(in) :: c_that
    LOGICAL :: b
    SELECT TYPE (c_that)
    TYPE is (gfcl_list_iterator)
       b = ASSOCIATED(c_this%p_node,c_that%p_node)
    CLASS default
       b = .FALSE.
    END SELECT
  END FUNCTION list_iterator_eq

  ! list_iterator_neq
  FUNCTION list_iterator_neq(c_this,c_that) RESULT(b)
    ! returns true if it1 and it2 point to the same node
    CLASS(gfcl_list_iterator), INTENT(in) :: c_this
    CLASS(forward_iterator)  , INTENT(in) :: c_that
    LOGICAL :: b
    SELECT TYPE (c_that)
    TYPE IS (gfcl_list_iterator)
       b = .NOT.ASSOCIATED(c_this%p_node,c_that%p_node)
    CLASS default
       b = .TRUE.
    END SELECT
  END FUNCTION list_iterator_neq

  ! list_iterator_assign
  SUBROUTINE list_iterator_assign(t_this,t_that)
    ! make t_this and t_that equal iterators
    TYPE(gfcl_list_iterator), intent(out) :: t_this
    TYPE(gfcl_list_iterator), intent(in)  :: t_that
    t_this%p_node => t_that%p_node
  END SUBROUTINE list_iterator_assign

  ! list_iterator_assign_node
  SUBROUTINE list_iterator_assign_node(t_this,t_node)
    TYPE(gfcl_list_iterator), INTENT(out) :: t_this
    TYPE(node_T), INTENT(in), TARGET :: t_node
    t_this%p_node => t_node
  END SUBROUTINE list_iterator_assign_node

  ! list_iterator_get_at
  FUNCTION list_iterator_get_at(t_this) RESULT(VALUE)
    TYPE(gfcl_list_iterator), INTENT(in) :: t_this
    CLASS(*)                , POINTER    :: value
    value => t_this%p_node%c_data
  END FUNCTION list_iterator_get_at

  ! list_iterator_set_at
  SUBROUTINE list_iterator_set_at(t_this,value)
    TYPE(gfcl_list_iterator), INTENT(inout) :: t_this
    CLASS(*)                , INTENT(in)    :: value
    IF (ALLOCATED(t_this%p_node%c_data)) DEALLOCATE(t_this%p_node%c_data)
    ALLOCATE(t_this%p_node%c_data,source=value)
  END SUBROUTINE list_iterator_set_at

  ! list_iterator_transfer
  SUBROUTINE list_iterator_transfer(t_this,t_first,t_last)
    TYPE(gfcl_list_iterator), INTENT(inout) :: t_this,t_first,t_last
    CALL list_node_transfer(t_this%p_node,t_first%p_node,t_last%p_node)
  END SUBROUTINE list_iterator_transfer
  
  ! list_iterator_distance
  FUNCTION list_iterator_distance(t_first,t_last) RESULT(n)
    TYPE(gfcl_list_iterator), INTENT(in) :: t_first,t_last
    INTEGER  :: n
    TYPE(gfcl_list_iterator) :: itr
    n = 0
    itr = t_first
    DO WHILE (itr /= t_last)
       n = n+1
       CALL itr%next()
    END DO
  END FUNCTION list_iterator_distance


  
END MODULE gfcl_list_module

