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
  USE gfcl_list_node
  USE gfcl_list_iterator
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
     !--- Component part
     PRIVATE
     TYPE(ListNode) :: t_node_

   CONTAINS
     !--- Type-bound-procedure part
     ! iterators
     PROCEDURE :: begin => begin__
     PROCEDURE :: end   => end__

     ! capacity
     PROCEDURE :: empty    => empty__
     PROCEDURE :: size     => size__
!     PROCEDURE :: max_size => max_size__

     ! element access

  END TYPE List

  !--- Interfaces ----------------------------------------------------               
CONTAINS

  ! initialise_empty__
  ! Constructs an empty container, WITH no elements.
  SUBROUTINE initialise_void__(t_this)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout), TARGET :: t_this
    ! --- Executable Code ----------------------------------------------
    IF (ASSOCIATED(t_this%t_node_%tp_next_)) CALL list_clear(t_this)
    t_this%t_node_%tp_next_ => t_this%t_node_
    t_this%t_node_%tp_prev_ => t_this%t_node_
  END SUBROUTINE initialise_void__

  ! initialise_fill__
  ! Constructs a container filled with count values
  SUBROUTINE initialise_fill__(t_this,i_count,t_value)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    INTEGER   , INTENT(in)    :: i_count
    CLASS(*)  , INTENT(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    CALL initialise_empty__(t_this)
    CALL insert_fill__(t_this%end(),i_count,t_value)
  END SUBROUTINE initialise_fill__

  ! list_construct_range
  SUBROUTINE initialise_range__(t_this,t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List)        , INTENT(inout) :: t_this
    TYPE(ListIterator), INTENT(in)    :: t_first, t_last
    ! --- Executable Code ----------------------------------------------
    CALL initialise_empty__(t_this)
    CALL insert_range__(t_this%end(),t_first,t_last)
  END SUBROUTINE initialise_range__

  SUBROUTINE initialise_copy__(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    TYPE(List), INTENT(in)    :: t_that
    ! --- Executable Code ----------------------------------------------
    CALL initialise_empty__(t_this)
    CALL insert_range__(t_this%end(),t_that%begin(),t_that%end())
  END SUBROUTINE initialise_copy__

  SUBROUTINE finalise__(t_this)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    CALL list_clear(t_this)
  END SUBROUTINE finalise__

  !===================================================================
  ! 
  ! General list iterator querys
  !
  ! list_begin :: returns iterator that points to the first element
  ! list_end   :: returns iterator that points to one past the end
  !
  !===================================================================

  ! list_begin
  FUNCTION begin__(t_this) RESULT(iterator)
    ! --- Declaration of arguments -------------------------------------
    CLASS(List), INTENT(in), TARGET :: t_this
    TYPE(ListIterator)              :: iterator
    ! --- Executable Code ----------------------------------------------
    iterator = ListIterator(t_this%t_node_%tp_next_)
  END FUNCTION begin__

  ! list_end
  FUNCTION end__(t_this) RESULT(iterator)
    ! --- Declaration of arguments -------------------------------------
    CLASS(List), INTENT(in), TARGET :: t_this
    TYPE(ListIterator)              :: iterator
    ! --- Executable Code ----------------------------------------------
    iterator = ListIterator(t_this%t_node_)
  END FUNCTION end__

  !===================================================================
  ! 
  ! list capacity
  !
  ! list_size  :: returns the list size
  ! list_empty :: returns if the list is empty or not
  !
  !===================================================================

  ! Returns the number of elements in the list container.
  FUNCTION size__(t_this) RESULT(i_size)
    ! --- Declaration of arguments -------------------------------------
    CLASS(List), INTENT(in) :: t_this
    INTEGER                 :: i_size
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator)      :: t_iterator
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%begin()
    i_size = 0

    DO WHILE (t_iterator /= t_this%end())
       i_size = i_size + 1
       CALL t_iterator%next()
    END DO
  END FUNCTION size__

  ! Returns whether the list container is empty (i.e. whether its
  ! size is 0).
  FUNCTION empty__(t_this) RESULT(b)
    ! --- Declaration of arguments -------------------------------------
    CLASS(List), INTENT(in), TARGET :: t_this
    LOGICAL                         :: b
    ! --- Executable Code ----------------------------------------------
    b = ASSOCIATED(t_this%t_node_%tp_next_,t_this%t_node_)
  END FUNCTION empty__

  !===================================================================
  ! 
  ! list modifiers
  !
  ! list_assign_range :: create new list with elements of [first,last)
  ! list_assign_fill  :: create new list with n copies of val
  !
  ! list_insert_element :: insert element before position
  ! list_insert_fill    :: insert a number of copies of given elem
  !                        into the list
  ! list_insert_range   :: insert a range into the list
  !
  ! list_erase_element :: erase element at location pos
  ! list_erase_range   :: erase elements [first,last)
  !
  ! list_push_front :: add value to the beginning of the list
  ! list_push_back  :: add value to the end of the list
  ! list_pop_front  :: delete value at the beginning of the list
  ! list_pop_back   :: delete value at the end of the list
  !
  ! list_clear      :: clear content
  ! list_swap       :: swap content of lists
  ! list_resize     :: change the size of the list
  !===================================================================

  ! assign_range
  ! the new content of list are elements constructed from each of the
  ! elements in the range between first and last, in the same
  ! order. First and last could belong to a different list.

  SUBROUTINE assign_range__(t_this, t_first2, t_last2)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List)        , INTENT(inout) :: t_this
    TYPE(ListIterator), VALUE         :: t_first2, t_last2
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator)                :: t_first1, t_last1
    ! --- Executable Code ----------------------------------------------
    t_first1 = t_this%begin()
    t_last1  = t_this%end()

    DO WHILE (t_first1 /= t_last1 .AND. t_first2 /= t_last2)
       CALL t_first1%set(t_first2%get())
       CALL t_first1%next()
       CALL t_first2%next()
    END DO
    IF (t_first2 == t_last2) THEN
       CALL erase_range__(t_first1,t_last1)
    ELSE
       CALL insert_range__(t_last1,t_first2, t_last2)
    END IF
  END SUBROUTINE assign_range__

  ! list_assign_fill
  ! the new contents are n elements, each initialized to a copy of
  ! val.
  SUBROUTINE assign_fill__(t_this,i_count,t_value)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    INTEGER   , VALUE         :: i_count
    CLASS(*)  , INTENT(in)    :: t_value
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator)        :: t_iterator
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%begin()

    DO WHILE(t_iterator /= t_this%end() .AND. i_count > 0)
       CALL t_iterator%set(t_value)
       CALL t_iterator%next()
       i_count = i_count - 1
    END DO
    IF (i_count > 0) THEN
       CALL insert_fill__(t_this%end(),i_count,t_value)
    ELSE
       CALL erase_range__(t_iterator,t_this%end())
    END IF
  END SUBROUTINE assign_fill__

  SUBROUTINE assign_list__(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    TYPE(List), INTENT(in)    :: t_that
    ! --- Executable Code ----------------------------------------------
    CALL assign_range__(t_this,t_that%begin(),t_this%end())
  END SUBROUTINE assign_list__


  ! list_insert_element
  ! adds single element with value val before pos
  ! input pos :: iterator
  ! input val :: TYPE(value_type)
  SUBROUTINE insert_element__(t_position, t_value)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(in)           :: t_position
    CLASS(*)          , INTENT(in), OPTIONAL :: t_value
    ! --- Declaration of variables -------------------------------------
    TYPE(ListNode)    , POINTER              :: tp_node
    ! --- Executable Code ----------------------------------------------
    ALLOCATE(tp_node)
    IF (PRESENT(t_value)) CALL tp_node%set(t_value)
    CALL tp_node%hook(t_position%tp_node_)
  END SUBROUTINE insert_element__

  ! list_insert_fill
  ! Adds n times the value val before position pos
  ! input pos :: iterator
  ! input n   :: integer
  ! input val :: TYPE(value_type)
  SUBROUTINE insert_fill__(t_position,i_count,t_value)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(in)           :: t_position
    INTEGER           , VALUE                :: i_count
    CLASS(*)          , INTENT(in), OPTIONAL :: t_value
    ! --- Executable Code ----------------------------------------------
    ! we do not need to increment pos as the new element remains
    ! pointing at the same node and all elements are inserted
    ! before that on.
    IF (PRESENT(t_value)) THEN
       DO WHILE (i_count > 0)
          CALL insert_element__(t_position, t_value)
          i_count = i_count - 1
       END DO
    ELSE
       DO WHILE (i_count > 0)
          CALL insert_element__(t_position)
          i_count = i_count - 1
       END DO
    END IF
  END SUBROUTINE insert_fill__

  ! list_insert_range
  ! Adds the elements from [first,last) before pos
  ! element last is excluded
  SUBROUTINE insert_range__(t_position, t_first, t_last)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(in) :: t_position
    TYPE(ListIterator), VALUE      :: t_first
    TYPE(ListIterator), INTENT(in) :: t_last
    ! --- Executable Code ----------------------------------------------
    DO WHILE (t_first /= t_last)
       CALL insert_element__(t_position,t_first%get())
       CALL t_first%next()
    END DO
  END SUBROUTINE insert_range__

  ! list_erase_element
  ! Removes from the list container a single element located at pos.
  ! After the erase is done, pos points to the next element
  SUBROUTINE erase_element__(t_position)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(inout) :: t_position
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator)                :: t_tmp
    ! --- Executable Code ----------------------------------------------
    t_tmp = t_position
    CALL t_tmp%next()
    CALL t_position%tp_node_%unhook()
    DEALLOCATE(t_position%tp_node_)
    NULLIFY(t_position%tp_node_)
    t_position = t_tmp
  END SUBROUTINE erase_element__

  ! list_erase_range
  ! Removes from the list container all elements between
  ! [first,last), excluding last
  SUBROUTINE erase_range__(t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), VALUE :: t_first, t_last
    ! --- Executable Code ----------------------------------------------
    DO WHILE (t_first /= t_last)
       CALL erase_element__(t_first)
    END DO
  END SUBROUTINE erase_range__

  ! list_push_front 
  ! Inserts a new element at the beginning of the list, right before
  ! its current first element. The content of val is copied (or
  ! moved) to the inserted element.
  ! input list  : TYPE(List)
  ! input val   : TYPE(value_type)
  SUBROUTINE push_front__(t_this,t_value)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    CLASS(*)  , INTENT(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    CALL insert_element__(t_this%begin(),t_value)
  END SUBROUTINE push_front__

  ! list_push_back
  ! Adds a new element at the end of the list container, after its
  ! current last element. The content of val is copied (or moved) to
  ! the new element.
  ! input list  : TYPE(List)
  ! input val   : TYPE(value_type)
  SUBROUTINE push_back__(t_this,t_value)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    CLASS(*)  , INTENT(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    CALL insert_element__(t_this%end(),t_value)
  END SUBROUTINE push_back__

  ! list_pop_front
  ! Removes the first element in the list container, effectively
  ! reducing its size by one. T_this destroys the removed element.
  ! input list : TYPE(List)
  SUBROUTINE pop_front__(t_this)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator)        :: t_iterator
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%begin()
    CALL erase_element__(t_iterator)
  END SUBROUTINE pop_front__

  ! list_pop_back
  ! Removes the last element in the list container, effectively
  ! reducing its size by one. This destroys the removed element.
  ! input list : TYPE(List)
  SUBROUTINE pop_back__(t_this)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator)        :: t_iterator
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%end()
    CALL t_iterator%prev()
    CALL erase_element__(t_iterator)
  END SUBROUTINE pop_back__

  ! list_clear
  ! Removes all elements from the list container (which are
  ! destroyed), and leaving the container with a size of 0.
  ! input list : TYPE(List)

  SUBROUTINE clear__(t_this)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    CALL erase_range__(t_this%begin(),t_this%end())
  END SUBROUTINE clear__

  ! list_swap
  ! Exchanges the content of the container x by the content of y,
  ! which is another list of the same TYPE. Sizes may differ.  After
  ! the call to this routine, the elements in y are those which were
  ! in x before the call, and the elements of x are those which were
  ! in y. All iterators, references and pointers remain valid for
  ! the swapped objects.
  SUBROUTINE swap__(t_listx, t_listy)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_listx, t_listy
    ! --- Executable Code ----------------------------------------------
    CALL t_listx%t_node_%swap(t_listy%t_node_)
  END SUBROUTINE swap__

  ! list_resize
  ! Resizes the container so that it contains n elements. If n is
  ! smaller than the current container size, the content is reduced
  ! to its first n elements, removing those beyond (and destroying
  ! them). If n is greater than the current container size, the
  ! content is expanded by inserting at the end as many elements as
  ! needed to reach a size of n. If val is specified, the new
  ! elements are initialized as copies of val, otherwise, they are
  ! value-initialized. Notice that this function changes the actual
  ! content of the container by inserting or erasing elements from
  ! it.

  SUBROUTINE resize__(t_this,i_size,t_value)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout)           :: t_this
    INTEGER   , INTENT(in)              :: i_size
    CLASS(*)  , INTENT(in)   , OPTIONAL :: t_value
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator) :: t_iterator
    INTEGER            :: i_counter
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%begin()
    i_counter  = 0

    DO WHILE (t_iterator /= t_this%end() .AND. i_counter < i_size)
       CALL t_iterator%next()
       i_counter = i_counter + 1
    END DO
    IF (i_counter == i_size) THEN
       CALL erase_range__(t_iterator,t_this%end())
    ELSE
       IF (PRESENT(t_value)) THEN
          CALL insert_fill__(t_iterator,i_size - i_counter,t_value)
       ELSE
          CALL insert_fill__(t_iterator,i_size - i_counter)
       END IF
    END IF
  END SUBROUTINE resize__

  !===================================================================
  ! 
  ! list operations
  !
  ! list_splice_list    :: Transfer elements from list to list (whole)
  ! list_splice_element :: Transfer elements from list to list (element)
  ! list_splice_range   :: Transfer elements from list to list (range)
  !
  ! list_merge          :: Merge two sorted lists
  !
  !===================================================================

  ! Insert contents of another list x The elements of x are inserted
  ! in constant time in front of the element referenced by position
  ! pos. x becomes an empty list.
  SUBROUTINE splice_list__(t_position,t_listx)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(in)    :: t_position
    TYPE(List)        , INTENT(inout) :: t_listx
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator)                :: first, last
    ! --- Executable Code ----------------------------------------------
    IF ( .NOT. t_listx%empty() ) THEN
       first = t_listx%begin()
       last  = t_listx%end()
       CALL t_position%tp_node_%transfer(first%tp_node_, last%tp_node_)
    END IF
  END SUBROUTINE splice_list__

  ! Insert element from another list. Removes the element in list x
  ! referenced by it and inserts it into the current list before
  ! position pos.
  SUBROUTINE splice_element__(t_position, t_iterator)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(in) :: t_position
    TYPE(ListIterator), INTENT(in) :: t_iterator
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator) :: t_iterator_next
    ! --- Executable Code ----------------------------------------------
    t_iterator_next = t_iterator
    CALL t_iterator_next%next()
    IF (t_position == t_iterator .OR. t_position == t_iterator_next) RETURN
    CALL t_position%tp_node_%transfer(t_iterator     %tp_node_, &
                                      t_iterator_next%tp_node_  )
  END SUBROUTINE splice_element__


  ! Insert range from another. Removes elements in the range
  ! [first,last) and inserts them before position in constant time.
  ! Undefined if position pos is in [first,last).
  SUBROUTINE splice_range__(t_position, t_first, t_last)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(in) :: t_position, t_first, t_last
    ! --- Executable Code ----------------------------------------------
    IF (t_first /= t_last) THEN
       CALL t_position%tp_node_%transfer(t_first%tp_node_, &
                                         t_last %tp_node_  )
    END IF
  END SUBROUTINE splice_range__

  ! Removes every element in the list for which pred(elem,val) ==
  ! true. Remaining elements stay in list order. Note that this
  ! function only erases the elements, and that if the elements
  ! themselves are pointers, the pointed-to memory is not touched in
  ! any way.  Managing the pointer is the user's responsibility.
  SUBROUTINE remove_value__(t_this,t_value,BinaryPredicate)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout), TARGET :: t_this
    CLASS(*)  , INTENT(in)   , TARGET :: t_value
    INTERFACE
       LOGICAL FUNCTION BinaryPredicate(x,y)
         CLASS(*), INTENT(in) :: x,y
       END FUNCTION BinaryPredicate
    END INTERFACE
    ! --- Declaration of variables -------------------------------------
    CLASS(*) , POINTER :: p1,p2
    TYPE(ListIterator) :: first,last,extra,next
    ! --- Executable Code ----------------------------------------------
    first = t_this%begin()
    last  = t_this%end()
    extra = last
    
    DO WHILE (first /= last)
       next = first
       CALL next%next()

       IF (BinaryPredicate(first%tp_node_%ta_data_,t_value)) THEN
          ! We do this in case we passed a pointer of a listvalue to
          ! the t_value argument
          p1 => t_value
          p2 => first%tp_node_%ta_data_
          IF (ASSOCIATED(p1,p2)) THEN
             extra = first
          ELSE
             CALL erase_element__(first)
          END IF
       END IF
       first = next
    END DO
    IF (extra /= last) THEN
       CALL erase_element__(extra)
    END IF
  END SUBROUTINE remove_value__

  ! Removes every element in the list for which the predicate
  ! pred(elem) returns true.  Remaining elements stay in list order.
  ! Note that this function only erases the elements, and that if
  ! the elements themselves are pointers, the pointed-to memory is
  ! not touched in any way.  Managing the pointer is the user's
  ! responsibility.
  SUBROUTINE remove_if__(t_this,UnaryPredicate)
  ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout), TARGET :: t_this
    INTERFACE
       LOGICAL FUNCTION UnaryPredicate(x)
         CLASS(*), INTENT(in) :: x
       END FUNCTION UnaryPredicate
    END INTERFACE
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator) :: first,last,next
    ! --- Executable Code ----------------------------------------------
    first = t_this%begin()
    last  = t_this%end()
    
    DO WHILE (first /= last)
       next = first
       CALL next%next()
       IF (UnaryPredicate(first%tp_node_%ta_data_)) THEN
          CALL erase_element__(first)
       END IF
       first = next
    END DO
  END SUBROUTINE remove_if__

  ! For each consecutive set of elements [first,last) that satisfy
  ! pred(first,i) where i is an iterator in [first,last), remove all
  ! but the first from every consecutive group of equal elements in
  ! the container. Remaining elements stay in list order.  Note that
  ! this function only erases the elements, and that if the elements
  ! themselves are pointers, the pointed-to memory is not touched in
  ! any way. Managing the pointer is the user's responsibility.
  SUBROUTINE unique__(t_this,BinaryPredicate)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    INTERFACE
       LOGICAL FUNCTION BinaryPredicate(x,y)
         CLASS(*), INTENT(in) :: x,y
       END FUNCTION BinaryPredicate
    END INTERFACE
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator) :: first, last, next
    ! --- Executable Code ----------------------------------------------
    first = t_this%begin()
    last  = t_this%end()
    IF (first /= last) THEN
       next = first
       CALL next%next()
       DO WHILE (next /= last)
          IF (BinaryPredicate(first%tp_node_%ta_data_, &
                              next %tp_node_%ta_data_  )) THEN
             CALL erase_element__(next)
          ELSE
             first = next
          END IF
          next = first
          CALL next%next()
       END DO
    END IF
  END SUBROUTINE unique__

  ! Merges y into the list x by transferring all of its elements at
  ! their respective ordered positions into the container (both
  ! containers shall already be ordered). This effectively removes
  ! all the elements in y (which becomes empty), and inserts them
  ! into their ordered position within list x (which expands in size
  ! by the number of elements transferred). The operation is
  ! performed without constructing nor destroying any element: they
  ! are transferred.

  SUBROUTINE merge__(t_this, t_that, BinaryPredicate)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this, t_that
    INTERFACE
       LOGICAL FUNCTION BinaryPredicate(x,y)
         CLASS(*), INTENT(in) :: x,y
       END FUNCTION BinaryPredicate
    END INTERFACE
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator) :: first1, last1
    TYPE(ListIterator) :: first2, last2
    TYPE(ListIterator) :: next
    ! --- Executable Code ----------------------------------------------
    IF (.NOT.ASSOCIATED(t_this%t_node_%tp_next_%tp_prev_, &
                        t_that%t_node_%tp_next_%tp_prev_)) THEN
       first1 = t_this%begin()
       last1  = t_this%end()
       first2 = t_that%begin()
       last2  = t_that%end()

       DO WHILE (first1 /= last1 .AND. first2 /= last2)
          IF (BinaryPredicate(first2%tp_node_%ta_data_,&
                              first1%tp_node_%ta_data_ )) THEN
             next = first2
             CALL next%next()
             CALL splice_range__(first1,first2,next)
             first2 = next
          ELSE
             CALL first1%next()
          END IF
       END DO
       IF (first2 /= last2) THEN
          CALL splice_range__(last1,first2,last2)
       END IF
    END IF
  END SUBROUTINE merge__
  
  ! Reverse the order of the list this
  SUBROUTINE reverse__(t_this)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    CALL t_this%t_node_%reverse()
  END SUBROUTINE reverse__


  ! Sorts the elements in the list, altering their position within
  ! the container.
  !
  ! The sorting is performed by applying an algorithm that uses comp
  ! to compare elements. This comparison shall produce a strict weak
  ! ordering of the elements (i.e., a consistent transitive
  ! comparison, without considering its reflexiveness).
  !
  ! The resulting order of equivalent elements is stable: i.e.,
  ! equivalent elements preserve the relative order they had before
  ! the call.
  !
  ! The entire operation does not involve the construction,
  ! destruction or copy of any element object. Elements are moved
  ! within the container.
  !
  ! Sorts the elements of this list in NlogN time.
  ! Algorithm : n-way merge sort
  SUBROUTINE sort__(t_this,BinaryPredicate)
    ! --- Declaration of arguments -------------------------------------
    TYPE(List), INTENT(inout) :: t_this
    INTERFACE
       LOGICAL FUNCTION BinaryPredicate(x,y)
         CLASS(*), INTENT(in) :: x,y
       END FUNCTION BinaryPredicate
    END INTERFACE
    ! --- Declaration of variables -------------------------------------
    TYPE(List)                :: carry
    TYPE(List), DIMENSION(64) :: tmp
    INTEGER :: fill, counter, i
    ! --- Executable Code ----------------------------------------------
    CALL initialise_void__(carry)
    DO i = 1,64
       CALL initialise_void__(tmp(i))
    END DO

    ! Do nothing if the list has length 0 or 1
    IF (.NOT.ASSOCIATED(t_this%t_node_%tp_next_        , &
                        t_this%t_node_%tp_next_%tp_prev_ ) .AND. &

         .NOT.ASSOCIATED(t_this%t_node_%tp_next_%tp_next_, &
                         t_this%t_node_%tp_next_%tp_prev_  )       ) THEN
       fill = 1
       DO
          CALL splice_element__(carry%begin(),t_this%begin())
          counter = 1
          DO WHILE (counter /= fill .AND. &
               .NOT. tmp(counter)%empty() )
             CALL merge__(tmp(counter),carry,BinaryPredicate)
             CALL swap__(carry,tmp(counter))
             counter = counter + 1
          END DO
          CALL swap__(carry,tmp(counter))
          IF (counter == fill) fill = fill + 1
          IF (t_this%empty()) EXIT
       END DO
       counter = 2
       DO WHILE (counter  /= fill)
          CALL merge__(tmp(counter),tmp(counter-1),BinaryPredicate)
          counter = counter + 1
       END DO

       CALL swap__(t_this,tmp(fill-1))

    END IF
  END SUBROUTINE sort__


  
END MODULE gfcl_list

