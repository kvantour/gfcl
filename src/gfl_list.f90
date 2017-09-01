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

  ! ! list_construct_default
  ! SUBROUTINE list_construct_default(this)
  !   ! Constructs an empty container, WITH no elements.
  !   TYPE(List), INTENT(inout),TARGET :: this
  !   IF (ASSOCIATED(this%node%next)) CALL list_clear(this)
  !   this%node%next => this%node
  !   this%node%prev => this%node
  !   CALL stl_new(this%node%data_t)
  ! END SUBROUTINE list_construct_default

  ! ! list_construct_default
  ! SUBROUTINE list_construct_fill(this,n,val)
  !   ! Constructs an empty container, WITH no elements.
  !   TYPE(List), INTENT(inout),TARGET :: this
  !   INTEGER, INTENT(in) :: n
  !   TYPE(value_type), INTENT(in) :: val

  !   TYPE(ListIterator) :: pos

  !   CALL list_construct_default(this)
  !   pos%node => this%node
  !   CALL list_insert_fill(pos,n,val)
  ! END SUBROUTINE list_construct_fill

  ! ! list_construct_range
  ! SUBROUTINE list_construct_range(this,first,last)
  !   TYPE(List), INTENT(inout), TARGET :: this
  !   TYPE(ListIterator), INTENT(in) :: first,last
  !   TYPE(ListIterator)             :: tmp

  !   CALL list_construct_default(this)
  !   tmp%node => this%node
  !   CALL list_insert_range(tmp,first,last)
  ! END SUBROUTINE list_construct_range

  ! SUBROUTINE list_construct_copy(this,x)
  !   TYPE(List), INTENT(inout), TARGET :: this
  !   TYPE(List), INTENT(in)            :: x
  !   TYPE(ListIterator) :: pos,first,last
    
  !   CALL list_construct_default(this)
  !   pos = list_begin(x)
  !   first%node => pos%node
  !   pos = list_end(x)
  !   last%node => pos%node
  !   pos%node => this%node
    
  !   CALL list_insert_range(pos,first,last)
  ! END SUBROUTINE list_construct_copy

  ! SUBROUTINE list_destruct(this)
  !   TYPE(List), intent(inout) :: this
  !   CALL list_clear(this)
  ! END SUBROUTINE list_destruct

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

  ! ! list_assign_range
  ! SUBROUTINE assign_range__(t_this, first, last)
  !   ! the new content of list are elements constructed from each of the
  !   ! elements in the range between first and last, in the same
  !   ! order. First and last could belong to a different list.
  !   TYPE(List), INTENT(inout) :: t_this
  !   TYPE(ListIterator), INTENT(in) :: first, last

  !   TYPE(ListIterator) :: f1,f2,l1,l2
  !   f1 = t_this%begin()
  !   l1 = t_this%end()
  !   f2 = first
  !   l2 = last

  !   DO WHILE (f1 /= l1 .AND. f2 /= l2)
  !      CALL list_node_copy_data(f1%node,f2%node)
  !      CALL f1%prev()
  !      CALL f2%prev()
  !   END DO
  !   IF (f2 == l2) THEN
  !      CALL stl_erase(f1,l1)
  !   ELSE
  !      CALL stl_insert(l1,f2,l2)
  !   END IF
  ! END SUBROUTINE assign_range__

  ! ! list_assign_fill
  ! SUBROUTINE assign_fill__(t_this,n,val)
  !   ! the new contents are n elements, each initialized to a copy of
  !   ! val.
  !   TYPE(List), INTENT(inout) :: t_this
  !   INTEGER, INTENT(in)           :: n
  !   TYPE(value_type), INTENT(in)   :: val

  !   TYPE(ListIterator) :: it
  !   INTEGER             :: m
  !   TYPE(list_node)     :: dummy
  !   it = t_this%begin()
  !   m = n
  !   dummy%data_t = val

  !   DO WHILE(it /= t_this%end() .AND. m > 0)
  !      CALL list_node_copy_data(it%node,dummy)
  !      CALL it%prev()
  !      m = m - 1
  !   END DO
  !   IF (m > 0) THEN
  !      CALL stl_insert(t_this%end(),m,val)
  !   ELSE
  !      CALL stl_erase(it,t_this%end())
  !   END IF
  ! END SUBROUTINE assign_fill__

  ! SUBROUTINE assign_list__(x,y)
  !   TYPE(List), INTENT(inout) :: x
  !   TYPE(List), INTENT(in) :: y

  !   CALL stl_assign(x,y%begin(),y%end())
  ! END SUBROUTINE assign_list__


  ! list_insert_element
  ! adds single element with value val before pos
  ! input pos :: iterator
  ! input val :: TYPE(value_type)
  SUBROUTINE insert_element__(t_position, t_data)
    TYPE(ListIterator), VALUE      :: t_position
    CLASS(*)          , INTENT(in) :: t_data
    TYPE(ListNode)    , POINTER    :: node
    ALLOCATE(node)
    CALL stl_new(node%data_t,val)
    CALL node%hook(t_position%tp_node_)
  END SUBROUTINE insert_element__

  ! ! list_insert_fill
  ! SUBROUTINE insert_fill__(pos,n,val)
  !   ! Adds n times the value val before position pos
  !   ! input pos :: iterator
  !   ! input n   :: integer
  !   ! input val :: TYPE(value_type)

  !   TYPE(ListIterator) :: pos
  !   TYPE(value_type), INTENT(in) :: val
  !   INTEGER, INTENT(in) :: n
  !   INTEGER :: IDX

  !   DO IDX=1,n
  !      ! we do not need to increment pos as the new element remains
  !      ! pointing at the same node and all elements are inserted
  !      ! before that on.
  !      CALL list_insert_element(pos,val)
  !   END DO
  ! END SUBROUTINE insert_fill__

  ! ! list_insert_range
  ! SUBROUTINE insert_range__(pos,first,last)
  !   ! Adds the elements from [first,last) before pos
  !   ! element last is excluded

  !   TYPE(ListIterator), INTENT(inout) :: pos
  !   TYPE(ListIterator), INTENT(in) :: first,last
  !   TYPE(ListIterator)             :: tmp

  !   tmp%node => first%node
  !   DO WHILE (tmp /= last)
  !      CALL list_insert_element(pos,tmp%node%data_t)
  !      CALL tmp%prev()
  !   END DO
  ! END SUBROUTINE insert_range__

  ! list_erase_element
  ! Removes from the list container a single element located at pos.
  ! After the erase is done, pos points to the next element
  SUBROUTINE erase_element__(t_position,DestroyElement)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), INTENT(inout) :: t_position
    OPTIONAL                          :: DestroyElement
    INTERFACE
       SUBROUTINE DestroyElement(t_data)
         CLASS(*), INTENT(inout) :: t_data
       END SUBROUTINE DestroyElement
    END INTERFACE
    ! --- Declaration of variables -------------------------------------
    TYPE(ListIterator)                :: t_tmp
    ! --- Executable Code ----------------------------------------------
    t_tmp = t_position
    CALL t_tmp%next()
    IF (PRESENT(DestroyElement)) THEN
       CALL DestroyElement(t_position%tp_node_%ta_data_)
    END IF
    CALL t_position%tp_node_%unhook()
    DEALLOCATE(t_position%tp_node_)
    NULLIFY(t_position%tp_node_)
    t_position = t_tmp
  END SUBROUTINE erase_element__

  ! list_erase_range
  ! Removes from the list container all elements between
  ! [first,last), excluding last
  SUBROUTINE erase_range__(first,last, DestroyElement)
    ! --- Declaration of arguments -------------------------------------
    TYPE(ListIterator), VALUE :: first,last
    OPTIONAL                  :: DestroyElement
    INTERFACE
       SUBROUTINE DestroyElement(t_data)
         CLASS(*), INTENT(inout) :: t_data
       END SUBROUTINE DestroyElement
    END INTERFACE
    ! --- Executable Code ----------------------------------------------
    IF (PRESENT(DestroyElement)) THEN
       DO WHILE (first /= last)
          CALL erase_element__(first)
       END DO
    ELSE
       DO WHILE (first /= last)
          CALL erase_element__(first)
       END DO
    END IF
  END SUBROUTINE erase_range__


  ! ! list_push_front 
  ! SUBROUTINE push_front__(t_this,val)
  !   ! Inserts a new element at the beginning of the list, right before
  !   ! its current first element. The content of val is copied (or
  !   ! moved) to the inserted element.
  !   ! input list  : TYPE(List)
  !   ! input val   : TYPE(value_type)

  !   TYPE(List), INTENT(inout) :: t_this
  !   TYPE(value_type), INTENT(in) :: val

  !   CALL list_insert_element(t_this%begin(),val)
  ! END SUBROUTINE push_front__

  ! ! list_push_back
  ! SUBROUTINE push_back__(t_this,val)
  !   ! Adds a new element at the end of the list container, after its
  !   ! current last element. The content of val is copied (or moved) to
  !   ! the new element.
  !   ! input list  : TYPE(List)
  !   ! input val   : TYPE(value_type)

  !   TYPE(List), INTENT(inout) :: t_this
  !   TYPE(value_type), INTENT(in) :: val

  !   CALL list_insert_element(t_this%end(),val)
  ! END SUBROUTINE push_back__

  ! ! list_pop_front
  ! SUBROUTINE pop_front__(t_this)
  !   ! Removes the first element in the list container, effectively
  !   ! reducing its size by one. T_this destroys the removed element.
  !   ! input list : TYPE(List)

  !   TYPE(List), intent(inout) :: t_this
  !   TYPE(ListIterator) :: tmp
  !   tmp = t_this%begin()

  !   CALL stl_erase(tmp)
  ! END SUBROUTINE pop_front__

  ! ! list_pop_back
  ! SUBROUTINE pop_back__(t_this)
  !   ! Removes the last element in the list container, effectively
  !   ! reducing its size by one. This destroys the removed element.
  !   ! input list : TYPE(List)

  !   TYPE(List), intent(inout) :: t_this
  !   TYPE(ListIterator) :: tmp
  !   tmp = t_this%end()

  !   CALL list_iterator_dec(tmp)
  !   CALL stl_erase(tmp)

  ! END SUBROUTINE pop_back__

  ! ! list_clear
  ! SUBROUTINE clear__(t_this)
  !   ! Removes all elements from the list container (which are
  !   ! destroyed), and leaving the container with a size of 0.
  !   ! input list : TYPE(List)

  !   TYPE(List), INTENT(inout) :: t_this

  !   CALL list_erase_range(t_this%begin(),t_this%end())
  ! END SUBROUTINE clear__

  ! ! list_swap
  ! SUBROUTINE swap__(lstx,lsty)
  !   ! Exchanges the content of the container x by the content of y,
  !   ! which is another list of the same TYPE. Sizes may differ.  After
  !   ! the call to this routine, the elements in y are those which were
  !   ! in x before the call, and the elements of x are those which were
  !   ! in y. All iterators, references and pointers remain valid for
  !   ! the swapped objects.
  !   TYPE(List), INTENT(inout) :: lstx, lsty
  !   CALL list_node_swap(lstx%node,lsty%node)
  ! END SUBROUTINE swap__

  ! ! list_resize
  ! SUBROUTINE resize__(t_this,new_size,val)
  !   ! Resizes the container so that it contains n elements. If n is
  !   ! smaller than the current container size, the content is reduced
  !   ! to its first n elements, removing those beyond (and destroying
  !   ! them). If n is greater than the current container size, the
  !   ! content is expanded by inserting at the end as many elements as
  !   ! needed to reach a size of n. If val is specified, the new
  !   ! elements are initialized as copies of val, otherwise, they are
  !   ! value-initialized. Notice that this function changes the actual
  !   ! content of the container by inserting or erasing elements from
  !   ! it.

  !   ! input list : TYPE(List)
  !   ! input new_size : integer
  !   ! input val      : value
    
  !   TYPE(List), INTENT(inout) :: t_this
  !   INTEGER, INTENT(in)       :: new_size
  !   TYPE(value_type), INTENT(in), OPTIONAL :: val
    
  !   TYPE(ListIterator) :: tmp
  !   INTEGER             :: len
  !   TYPE(value_type), POINTER :: vpt

  !   tmp = t_this%begin()
  !   len = 0
  !   NULLIFY(vpt)

  !   DO WHILE (tmp /= t_this%end() .AND. len < new_size)
  !      CALL tmp%prev()
  !      len = len + 1
  !   END DO
  !   IF (len == new_size) THEN
  !      CALL stl_erase(tmp,t_this%end())
  !   ELSE
  !      IF (PRESENT(val)) THEN
  !         CALL stl_insert(tmp,new_size-len,val)
  !      ELSE
  !         ALLOCATE(vpt)
  !         CALL stl_new(vpt)
  !         CALL stl_insert(tmp,new_size-len,vpt)
  !         CALL stl_delete(vpt)
  !         DEALLOCATE(vpt)
  !         NULLIFY(vpt)
  !      END IF
  !   END IF
  ! END SUBROUTINE resize__

  ! !===================================================================
  ! ! 
  ! ! list operations
  ! !
  ! ! list_splice_list    :: Transfer elements from list to list (whole)
  ! ! list_splice_element :: Transfer elements from list to list (element)
  ! ! list_splice_range   :: Transfer elements from list to list (range)
  ! !
  ! ! list_merge          :: Merge two sorted lists
  ! !
  ! !===================================================================

  ! SUBROUTINE list_splice_list(pos,x)
  !   ! Insert contents of another list x The elements of x are inserted
  !   ! in constant time in front of the element referenced by position
  !   ! pos. x becomes an empty list.

  !   TYPE(ListIterator), intent(in) :: pos
  !   TYPE(List), INTENT(inout) :: x
  !   TYPE(list_node), POINTER :: node,first,last
  !   TYPE(ListIterator) :: tmp

  !   IF (.NOT.list_empty(x)) THEN
  !      node => pos%node
  !      tmp = list_begin(x)
  !      first => tmp%node
  !      tmp = list_end(x)
  !      last => tmp%node
  !      CALL list_node_transfer(node,first,last)
  !   END IF
  ! END SUBROUTINE list_splice_list

  ! SUBROUTINE list_splice_element(pos,itr)
  !   ! Insert element from another list. Removes the element in list x
  !   ! referenced by it and inserts it into the current list before
  !   ! position pos.

  !   TYPE(ListIterator), intent(in) :: pos
  !   TYPE(ListIterator), intent(in) :: itr
  !   TYPE(list_node), POINTER :: node,first,last

  !   TYPE(ListIterator) :: j

  !   j = itr
  !   j = j%next()
  !   IF (pos == itr .OR. pos == j) RETURN
  !   node  => pos%node
  !   first => itr%node
  !   last  => j%node
  !   CALL list_node_transfer(node,first,last)
  ! END SUBROUTINE list_splice_element


  ! SUBROUTINE list_splice_range(pos,first,last)
  !   ! Insert range from another. Removes elements in the range
  !   ! [first,last) and inserts them before position in constant time.
  !   ! Undefined if position pos is in [first,last).
  !   TYPE(ListIterator), INTENT(in) :: pos, first,last
  !   TYPE(list_node), POINTER :: n,f,l
  !   IF (first /= last) THEN
  !      n => pos%node
  !      f => first%node
  !      l => last%node
  !      CALL list_node_transfer(n,f,l)
  !   END IF
  ! END SUBROUTINE list_splice_range

  ! SUBROUTINE list_remove(this,val,pred)
  !   ! Removes every element in the list for which pred(elem,val) ==
  !   ! true. Remaining elements stay in list order. Note that this
  !   ! function only erases the elements, and that if the elements
  !   ! themselves are pointers, the pointed-to memory is not touched in
  !   ! any way.  Managing the pointer is the user's responsibility.

  !   TYPE(List), INTENT(inout),TARGET  :: this
  !   TYPE(value_type), INTENT(inout),TARGET :: val
  !   INTERFACE
  !      LOGICAL FUNCTION pred(val1,val2)
  !        IMPORT :: value_type
  !        TYPE(value_type), INTENT(in) :: val1,val2
  !      END FUNCTION pred
  !   END INTERFACE

  !   TYPE(value_type), POINTER :: p1,p2
  !   TYPE(ListIterator) :: first,last,extra,next
    
  !   CALL stl_assign(first,stl_begin(this))
  !   CALL stl_assign(last,stl_end(this))
  !   CALL stl_assign(extra,last)
    
  !   DO WHILE (first /= last)
  !      CALL stl_assign(next,first)
  !      next = next%next()
  !      IF (pred(first%node%data_t,val)) THEN
  !         p1 => val
  !         p2 => first%node%data_t
  !         IF (ASSOCIATED(p1,p2)) THEN
  !            CALL stl_assign(extra,first)
  !         ELSE
  !            CALL stl_erase(first)
  !         END IF
  !      END IF
  !      CALL stl_assign(first,next)
  !   END DO
  !   IF (extra /= last) THEN
  !      CALL stl_erase(extra)
  !   END IF
  ! END SUBROUTINE list_remove

  ! SUBROUTINE list_remove_if(this,pred)
  !   ! Removes every element in the list for which the predicate
  !   ! pred(elem) returns true.  Remaining elements stay in list order.
  !   ! Note that this function only erases the elements, and that if
  !   ! the elements themselves are pointers, the pointed-to memory is
  !   ! not touched in any way.  Managing the pointer is the user's
  !   ! responsibility.

  !   TYPE(List), INTENT(inout),TARGET  :: this
  !   INTERFACE
  !      LOGICAL FUNCTION pred(val)
  !        IMPORT :: value_type
  !        TYPE(value_type), INTENT(in) :: val
  !      END FUNCTION pred
  !   END INTERFACE
    
  !   TYPE(ListIterator) :: first,last,next
    
  !   CALL stl_assign(first,stl_begin(this))
  !   CALL stl_assign(last,stl_end(this))
    
  !   DO WHILE (first /= last)
  !      CALL stl_assign(next,first)
  !      next = next%next()
  !      IF (pred(first%node%data_t)) THEN
  !         CALL stl_erase(first)
  !      END IF
  !      CALL stl_assign(first,next)
  !   END DO
  ! END SUBROUTINE list_remove_if

  ! SUBROUTINE list_unique(this,pred)
  !   ! For each consecutive set of elements [first,last) that satisfy
  !   ! pred(first,i) where i is an iterator in [first,last),
  !   ! remove all but the first one all but the first one. Remaining
  !   ! elements stay in list order.  Note that this function only
  !   ! erases the elements, and that if the elements themselves are
  !   ! pointers, the pointed-to memory is not touched in any
  !   ! way. Managing the pointer is the user's responsibility.

  !   TYPE(List), INTENT(inout) :: this
  !   INTERFACE
  !      LOGICAL FUNCTION pred(val1,val2)
  !        IMPORT :: value_type
  !        TYPE(value_type), INTENT(in) :: val1,val2
  !      END FUNCTION pred
  !   END INTERFACE

  !   TYPE(ListIterator) :: first, last, next
  !   CALL stl_assign(first,list_begin(this))
  !   CALL stl_assign(last,list_end(this))

  !   IF (first /= last) THEN
  !      next = stl_inc(first)
  !      DO WHILE (next /= last)
  !         IF (pred(first%node%data_t,next%node%data_t)) THEN
  !            CALL stl_erase(next)
  !         ELSE
  !            first = next
  !         END IF
  !         next = stl_inc(first)
  !      END DO
  !   END IF
  ! END SUBROUTINE list_unique

  ! SUBROUTINE list_merge(x,y,comp)
  !   ! Merges y into the list x by transferring all of its elements at
  !   ! their respective ordered positions into the container (both
  !   ! containers shall already be ordered). This effectively removes
  !   ! all the elements in y (which becomes empty), and inserts them
  !   ! into their ordered position within list x (which expands in size
  !   ! by the number of elements transferred). The operation is
  !   ! performed without constructing nor destroying any element: they
  !   ! are transferred.

  !   TYPE(List), INTENT(inout) :: x,y
  !   INTERFACE
  !      LOGICAL FUNCTION comp(val1,val2)
  !        IMPORT :: value_type
  !        TYPE(value_type), INTENT(in) :: val1,val2
  !      END FUNCTION comp
  !   END INTERFACE

  !   TYPE(ListIterator) :: first1,first2,last1,last2
  !   TYPE(ListIterator) :: next

  !   IF (.NOT.ASSOCIATED(x%node%next%prev,y%node%next%prev)) THEN
  !      first1 = list_begin(x)
  !      last1  = list_end(x)
  !      first2 = list_begin(y)
  !      last2  = list_end(y)

  !      DO WHILE (first1 /= last1 .AND. first2 /= last2)
  !         IF (comp(first2%data_t,first1%data_t)) THEN
  !            next = first2
  !            next = stl_inc(next)
  !            CALL list_iterator_transfer(first1,first2,next)
  !            first2 = next
  !         ELSE
  !            first1 = stl_inc(first1)
  !         END IF
  !      END DO
  !      IF (first2 /= last2) THEN
  !         CALL list_iterator_transfer(last1,first2,last2)
  !      END IF
  !   END IF
  ! END SUBROUTINE list_merge

  ! SUBROUTINE list_reverse(this)
  !   ! Reverse the order of the list this
  !   TYPE(List), INTENT(inout) :: this
  !   CALL list_node_reverse(this%node)
  ! END SUBROUTINE list_reverse


  ! SUBROUTINE list_sort(this,comp)
  !   ! Sorts the elements in the list, altering their position within
  !   ! the container.
  !   !
  !   ! The sorting is performed by applying an algorithm that uses comp
  !   ! to compare elements. This comparison shall produce a strict weak
  !   ! ordering of the elements (i.e., a consistent transitive
  !   ! comparison, without considering its reflexiveness).
  !   !
  !   ! The resulting order of equivalent elements is stable: i.e.,
  !   ! equivalent elements preserve the relative order they had before
  !   ! the call.
  !   !
  !   ! The entire operation does not involve the construction,
  !   ! destruction or copy of any element object. Elements are moved
  !   ! within the container.
  !   !
  !   ! Sorts the elements of this list in NlogN time.
  !   ! Algorithm : n-way merge sort

  !   TYPE(List), INTENT(inout) :: this
  !   INTERFACE
  !      LOGICAL FUNCTION comp(val1,val2)
  !        IMPORT :: value_type
  !        TYPE(value_type), INTENT(in) :: val1,val2
  !      END FUNCTION comp
  !   END INTERFACE

  !   TYPE(List) :: carry
  !   TYPE(List), DIMENSION(64) :: tmp
  !   INTEGER :: fill, counter, i

  !   CALL stl_new(carry)
  !   DO  i=1,64
  !      CALL stl_new(tmp(i))
  !   END DO

  !   ! Do nothing if the list has length 0 or 1
  !   IF (.NOT.ASSOCIATED(this%node%next,this%node%next%prev) .AND. &
  !        .NOT.ASSOCIATED(this%node%next%next,this%node%next%prev)) THEN
  !      fill = 1

  !      DO
  !         CALL stl_splice(list_begin(carry),list_begin(this))
  !         counter = 1
  !         DO WHILE (counter /= fill .AND. &
  !              .NOT.list_empty(tmp(counter)))
  !            CALL list_merge(tmp(counter),carry,comp)
  !            CALL list_swap(carry,tmp(counter))
  !            counter = counter + 1
  !         END DO
  !         CALL list_swap(carry,tmp(counter))
  !         IF (counter == fill) fill = fill + 1
  !         IF (list_empty(this)) EXIT
  !      END DO
  !      counter = 2
  !      DO WHILE (counter  /= fill)
  !         CALL list_merge(tmp(counter),tmp(counter-1),comp)
  !         counter = counter + 1
  !      END DO

  !      CALL list_swap(this,tmp(fill-1))

  !   END IF
  ! END SUBROUTINE list_sort


  
END MODULE gfcl_list

