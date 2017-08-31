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

MODULE gfcl_list
  !--- USE statements ------------------------------------------------
  !--- Implicit statement --------------------------------------------
  IMPLICIT NONE
  !--- Private/public section ----------------------------------------
  PRIVATE

  !--- Data types ----------------------------------------------------

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
  TYPE :: List
     PRIVATE
!     TYPE(ListNode) :: t_node_
  END TYPE List

  !--- Interfaces ----------------------------------------------------               
CONTAINS
  
END MODULE gfcl_list

