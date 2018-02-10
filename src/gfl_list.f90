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

module gfcl_list
  !--- USE statements ------------------------------------------------
  use gfcl_list_node
  use gfcl_list_iterator
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Private/public section ----------------------------------------
  private
  public :: swap, size
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
  type, public :: List
     !--- Component part
     private
     type(ListNode) :: t_node_

   contains
     private
     !--- Type-bound-procedure part
     ! initialisors ====================================================
     procedure         :: initialise_void__
     procedure         :: initialise_copy__
     procedure         :: initialise_fill__
     procedure         :: initialise_array__
     procedure         :: initialise_range__
     generic  , public :: initialise     => initialise_void__,  &
                                            initialise_copy__,  &
                                            initialise_fill__,  &
                                            initialise_array__, &
                                            initialise_range__

     ! iterators =======================================================
     procedure, public :: begin          => begin__
     procedure, public :: end            => end__

     ! capacity ========================================================
     procedure, public :: empty          => empty__
     procedure, public :: size           => size__

     ! modifiers =======================================================
     procedure         :: assign_list__
     procedure         :: assign_fill__
     procedure         :: assign_array__
     procedure         :: assign_range__
     generic  , public :: assign         => assign_list__,  &
                                            assign_fill__,  &
                                            assign_array__, &
                                            assign_range__
     generic  , public :: assignment(=)  => assign_list__,  &
                                            assign_array__

     procedure, nopass :: insert_element__
     procedure, nopass :: insert_fill__
     procedure, nopass :: insert_array__
     procedure, nopass :: insert_range__
     generic  , public :: insert         => insert_element__, &
                                            insert_fill__,    &
                                            insert_array__,   &
                                            insert_range__

     procedure, nopass :: erase_element__
     procedure, nopass :: erase_range__
     generic  , public :: erase          => erase_element__, &
                                            erase_range__

     procedure, public :: push_front     => push_front__
     procedure, public :: push_back      => push_back__
     procedure, public :: pop_front      => pop_front__
     procedure, public :: pop_back       => pop_back__
     procedure, public :: clear          => clear__
     procedure, public :: swap           => swap__
     procedure, public :: resize         => resize__

     procedure, nopass :: splice_list__
     procedure, nopass :: splice_element__
     procedure, nopass :: splice_range__
     generic  , public :: splice         => splice_list__,    &
                                            splice_element__, &
                                            splice_range__

     procedure, public :: remove         => remove_value__
     procedure, public :: remove_if      => remove_if__
     procedure, public :: remove_unalloc => remove_unallocated__
     procedure, public :: unique         => unique__
     procedure, public :: merge          => merge__
     procedure, public :: reverse        => reverse__
     procedure, public :: sort           => sort__
  end type List

  !--- Interfaces ----------------------------------------------------
  interface swap
     module procedure swap__
  end interface swap

  interface size
     module procedure size__
  end interface size

contains

  ! =================================================================
  ! Initialisation and finalisation section
  ! =================================================================

  ! initialise_empty__
  ! Constructs an empty container, with no elements.
  subroutine initialise_void__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout), target :: t_this
    ! --- Executable Code ----------------------------------------------
    if (associated(t_this%t_node_%tp_next_)) call clear__(t_this)
    t_this%t_node_%tp_next_ => t_this%t_node_
    t_this%t_node_%tp_prev_ => t_this%t_node_
  end subroutine initialise_void__

  ! initialise_fill__
  ! Constructs a container filled with count values
  subroutine initialise_fill__(t_this,i_count,t_value)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    integer    , intent(in)    :: i_count
    class(*)   , intent(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    call initialise_void__(t_this)
    call insert_fill__(t_this%end(),i_count,t_value)
  end subroutine initialise_fill__

  subroutine initialise_array__(t_this,td_array)
    ! --- Declaration of arguments -------------------------------------
    class(List),               intent(inout) :: t_this
    class(*)   , dimension(:), intent(in)    :: td_array
    ! --- Executable Code ----------------------------------------------
    call initialise_void__(t_this)
    call insert_array__(t_this%end(),td_array)
  end subroutine initialise_array__

  ! list_construct_range
  subroutine initialise_range__(t_this,t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    class(List)        , intent(inout) :: t_this
    type(ListIterator) , intent(in)    :: t_first, t_last
    ! --- Executable Code ----------------------------------------------
    call initialise_void__(t_this)
    call insert_range__(t_this%end(),t_first,t_last)
  end subroutine initialise_range__

  subroutine initialise_copy__(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    type(List) , intent(in)    :: t_that
    ! --- Executable Code ----------------------------------------------
    call initialise_void__(t_this)
    call insert_range__(t_this%end(),t_that%begin(),t_that%end())
  end subroutine initialise_copy__

  subroutine finalise__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    call clear__(t_this)
  end subroutine finalise__

  !===================================================================
  ! 
  ! General list iterator querys
  !
  ! list_begin :: returns iterator that points to the first element
  ! list_end   :: returns iterator that points to one past the end
  !
  !===================================================================

  ! list_begin
  function begin__(t_this) result(iterator)
    ! --- Declaration of arguments -------------------------------------
    class(List)       , intent(in), target :: t_this
    type(ListIterator)                     :: iterator
    ! --- Executable Code ----------------------------------------------
    iterator = ListIterator(t_this%t_node_%tp_next_)
  end function begin__

  ! list_end
  function end__(t_this) result(iterator)
    ! --- Declaration of arguments -------------------------------------
    class(List)       , intent(in), target :: t_this
    type(ListIterator)                     :: iterator
    ! --- Executable Code ----------------------------------------------
    iterator = ListIterator(t_this%t_node_)
  end function end__

  !===================================================================
  ! 
  ! list capacity
  !
  ! list_size  :: returns the list size
  ! list_empty :: returns if the list is empty or not
  !
  !===================================================================

  ! Returns the number of elements in the list container.
  function size__(t_this) result(i_size)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(in) :: t_this
    integer                 :: i_size
    ! --- Declaration of variables -------------------------------------
    type(ListIterator)      :: t_iterator
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%begin()
    i_size = 0

    do while (t_iterator /= t_this%end())
       i_size = i_size + 1
       call t_iterator%next()
    end do
  end function size__

  ! Returns whether the list container is empty (i.e. whether its
  ! size is 0).
  function empty__(t_this) result(b)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(in), target :: t_this
    logical                         :: b
    ! --- Executable Code ----------------------------------------------
    b = associated(t_this%t_node_%tp_next_,t_this%t_node_)
  end function empty__

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

  subroutine assign_range__(t_this, t_first2, t_last2)
    ! --- Declaration of arguments -------------------------------------
    class(List)       , intent(inout) :: t_this
    type(ListIterator), value         :: t_first2, t_last2
    ! --- Declaration of variables -------------------------------------
    type(ListIterator)                :: t_first1, t_last1
    ! --- Executable Code ----------------------------------------------
    t_first1 = t_this%begin()
    t_last1  = t_this%end()

    do while (t_first1 /= t_last1 .and. t_first2 /= t_last2)
       call t_first1%set(t_first2%get())
       call t_first1%next()
       call t_first2%next()
    end do
    if (t_first2 == t_last2) then
       call erase_range__(t_first1,t_last1)
    else
       call insert_range__(t_last1,t_first2, t_last2)
    end if
  end subroutine assign_range__

  ! list_assign_fill
  ! the new contents are n elements, each initialized to a copy of
  ! val.
  subroutine assign_fill__(t_this,i_count,t_value)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    integer    , value         :: i_count
    class(*)   , intent(in)    :: t_value
    ! --- Declaration of variables -------------------------------------
    type(ListIterator)         :: t_iterator
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%begin()

    do while(t_iterator /= t_this%end() .and. i_count > 0)
       call t_iterator%set(t_value)
       call t_iterator%next()
       i_count = i_count - 1
    end do
    if (i_count > 0) then
       call insert_fill__(t_this%end(),i_count,t_value)
    else
       call erase_range__(t_iterator,t_this%end())
    end if
  end subroutine assign_fill__

  ! list_assign_array
  ! the new contents are the elements of an array
  subroutine assign_array__(this,array)
    ! --- Declaration of arguments -------------------------------------
    class(List),               intent(inout) :: this
    class(*)   , dimension(:), intent(in)    :: array
    ! --- Declaration of variables -------------------------------------
    type(ListIterator)                       :: first,last
    integer                                  :: start,stop
    ! --- Executable Code ----------------------------------------------
    first = this%begin(); last=this%end();
    start = 1           ; stop=size(array) + 1

    do while(first /= last .and. start /= stop)
       call first%set( array(start) )
       call first%next()
       start=start+1
    end do
    if (start == stop) then
       call erase_range__(first,last)
    else
       call insert_array_range__(last,array,start,stop)
    end if
  end subroutine assign_array__


  subroutine assign_list__(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    type(List) , intent(in)    :: t_that
    ! --- Executable Code ----------------------------------------------
    call assign_range__(t_this,t_that%begin(),t_that%end())
  end subroutine assign_list__


  ! list_insert_element
  ! adds single element with value val before pos
  ! input pos :: iterator
  ! input val :: TYPE(value_type)
  subroutine insert_element__(t_position, t_value)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(in) :: t_position
    class(*)          , intent(in) :: t_value
    ! --- Declaration of variables -------------------------------------
    type(ListNode)    , pointer    :: tp_node
    ! --- Executable Code ----------------------------------------------
    allocate(tp_node)
    call tp_node%set(t_value)
    call tp_node%hook(t_position%tp_node_)
  end subroutine insert_element__

  ! list_insert_fill
  ! Adds n times the value val before position pos
  ! input pos :: iterator
  ! input n   :: integer
  ! input val :: TYPE(value_type)
  subroutine insert_fill__(t_position,i_count,t_value)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(in) :: t_position
    integer           , value      :: i_count
    class(*)          , intent(in) :: t_value
    ! --- Executable Code ----------------------------------------------
    ! we do not need to increment pos as the new element remains
    ! pointing at the same node and all elements are inserted
    ! before that on.
    do while (i_count > 0)
       call insert_element__(t_position, t_value)
       i_count = i_count - 1
    end do
  end subroutine insert_fill__

  ! list_insert_array
  ! Adds n times the value val before position pos
  ! input pos :: iterator
  ! input n   :: integer
  ! input val :: TYPE(value_type)
  subroutine insert_array_range__(position, array, start, stop)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator),               intent(in) :: position
    class(*)          , dimension(:), intent(in) :: array
    integer           , value                    :: start, stop
    ! --- Executable Code ----------------------------------------------
    do while (start /= stop)
       call insert_element__(position,array(start))
       start = start + 1
    end do
  end subroutine insert_array_range__

  ! list_insert_array
  ! Adds n times the value val before position pos
  ! input pos :: iterator
  ! input n   :: integer
  ! input val :: TYPE(value_type)
  subroutine insert_array__(position, array)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator),               intent(in) :: position
    class(*)          , dimension(:), intent(in) :: array
    ! --- Executable Code ----------------------------------------------
    ! we do not need to increment pos as the new element remains
    ! pointing at the same node and all elements are inserted
    ! before that one.
    call insert_array_range__(position,array,1,size(array)+1)
  end subroutine insert_array__

  ! list_insert_range
  ! Adds the elements from [first,last) before pos
  ! element last is excluded
  subroutine insert_range__(t_position, t_first, t_last)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(in) :: t_position
    type(ListIterator), value      :: t_first
    type(ListIterator), intent(in) :: t_last
    ! --- Executable Code ----------------------------------------------
    do while (t_first /= t_last)
       call insert_element__(t_position,t_first%get())
       call t_first%next()
    end do
  end subroutine insert_range__

  ! list_erase_element
  ! Removes from the list container a single element located at pos.
  ! After the erase is done, pos points to the next element
  subroutine erase_element__(t_position)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(inout) :: t_position
    ! --- Declaration of variables -------------------------------------
    type(ListIterator)                :: t_tmp
    ! --- Executable Code ----------------------------------------------
    t_tmp = t_position
    call t_tmp%next()
    call t_position%tp_node_%unhook()
    deallocate(t_position%tp_node_)
    nullify(t_position%tp_node_)
    t_position = t_tmp
  end subroutine erase_element__

  ! list_erase_range
  ! Removes from the list container all elements between
  ! [first,last), excluding last
  subroutine erase_range__(t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), value :: t_first, t_last
    ! --- Executable Code ----------------------------------------------
    do while (t_first /= t_last)
       call erase_element__(t_first)
    end do
  end subroutine erase_range__

  ! list_push_front 
  ! Inserts a new element at the beginning of the list, right before
  ! its current first element. The content of val is copied (or
  ! moved) to the inserted element.
  ! input list  : TYPE(List)
  ! input val   : TYPE(value_type)
  subroutine push_front__(t_this,t_value)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    class(*)   , intent(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    call insert_element__(t_this%begin(),t_value)
  end subroutine push_front__

  ! list_push_back
  ! Adds a new element at the end of the list container, after its
  ! current last element. The content of val is copied (or moved) to
  ! the new element.
  ! input list  : CLASS(List)
  ! input val   : TYPE(value_type)
  subroutine push_back__(t_this,t_value)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    class(*)   , intent(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    call insert_element__(t_this%end(),t_value)
  end subroutine push_back__

  ! list_pop_front
  ! Removes the first element in the list container, effectively
  ! reducing its size by one. T_this destroys the removed element.
  ! input list : CLASS(List)
  subroutine pop_front__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    ! --- Declaration of variables -------------------------------------
    type(ListIterator) :: t_iterator
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%begin()
    call erase_element__(t_iterator)
  end subroutine pop_front__

  ! list_pop_back
  ! Removes the last element in the list container, effectively
  ! reducing its size by one. This destroys the removed element.
  ! input list : CLASS(List)
  subroutine pop_back__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    ! --- Declaration of variables -------------------------------------
    type(ListIterator) :: t_iterator
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%end()
    call t_iterator%prev()
    call erase_element__(t_iterator)
  end subroutine pop_back__

  ! list_clear
  ! Removes all elements from the list container (which are
  ! destroyed), and leaving the container with a size of 0.
  ! input list : CLASS(List)

  subroutine clear__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    call erase_range__(t_this%begin(),t_this%end())
  end subroutine clear__

  ! list_swap
  ! Exchanges the content of the container x by the content of y,
  ! which is another list of the same TYPE. Sizes may differ.  After
  ! the call to this routine, the elements in y are those which were
  ! in x before the call, and the elements of x are those which were
  ! in y. All iterators, references and pointers remain valid for
  ! the swapped objects.
  subroutine swap__(t_listx, t_listy)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_listx, t_listy
    ! --- Executable Code ----------------------------------------------
    call t_listx%t_node_%swap(t_listy%t_node_)
  end subroutine swap__

  ! list_resize
  ! Resizes the container so that it contains n elements. If n is
  ! smaller than the current container size, the content is reduced to
  ! its first n elements, removing those beyond (and destroying
  ! them). If n is greater than the current container size, the
  ! content is expanded by inserting at the end as many elements as
  ! needed to reach a size of n. Notice that this function changes the
  ! actual content of the container by inserting or erasing elements
  ! from it.

  subroutine resize__(t_this,i_size,t_value)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    integer    , intent(in)    :: i_size
    class(*)   , intent(in)    :: t_value
    ! --- Declaration of variables -------------------------------------
    type(ListIterator) :: t_iterator
    integer            :: i_counter
    ! --- Executable Code ----------------------------------------------
    t_iterator = t_this%begin()
    i_counter  = 0

    do while (t_iterator /= t_this%end() .and. i_counter < i_size)
       call t_iterator%next()
       i_counter = i_counter + 1
    end do
    if (i_counter == i_size) then
       call erase_range__(t_iterator,t_this%end())
    else
       call insert_fill__(t_iterator,i_size - i_counter,t_value)
    end if
  end subroutine resize__

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
  subroutine splice_list__(t_position,t_listx)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(in)    :: t_position
    type(List)        , intent(inout) :: t_listx
    ! --- Declaration of variables -------------------------------------
    type(ListIterator)                :: first, last
    ! --- Executable Code ----------------------------------------------
    if ( .not. t_listx%empty() ) then
       first = t_listx%begin()
       last  = t_listx%end()
       call t_position%tp_node_%transfer(first%tp_node_, last%tp_node_)
    end if
  end subroutine splice_list__

  ! Insert element from another list. Removes the element in list x
  ! referenced by it and inserts it into the current list before
  ! position pos.
  subroutine splice_element__(t_position, t_iterator)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(in) :: t_position
    type(ListIterator), intent(in) :: t_iterator
    ! --- Declaration of variables -------------------------------------
    type(ListIterator) :: t_iterator_next
    ! --- Executable Code ----------------------------------------------
    t_iterator_next = t_iterator
    call t_iterator_next%next()
    if (t_position == t_iterator .or. t_position == t_iterator_next) return
    call t_position%tp_node_%transfer(t_iterator     %tp_node_, &
                                      t_iterator_next%tp_node_  )
  end subroutine splice_element__


  ! Insert range from another. Removes elements in the range
  ! [first,last) and inserts them before position in constant time.
  ! Undefined if position pos is in [first,last).
  subroutine splice_range__(t_position, t_first, t_last)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(in) :: t_position, t_first, t_last
    ! --- Executable Code ----------------------------------------------
    if (t_first /= t_last) then
       call t_position%tp_node_%transfer(t_first%tp_node_, &
                                         t_last %tp_node_  )
    end if
  end subroutine splice_range__

  ! Removes every element in the list for which pred(elem,val) ==
  ! true. Remaining elements stay in list order. Note that this
  ! function only erases the elements, and that if the elements
  ! themselves are pointers, the pointed-to memory is not touched in
  ! any way.  Managing the pointer is the user's responsibility.
  subroutine remove_value__(t_this,t_value,BinaryPredicate)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout), target :: t_this
    class(*)   , intent(in)   , target :: t_value
    interface
       logical function BinaryPredicate(x,y)
         class(*), intent(in) :: x,y
       end function BinaryPredicate
    end interface
    ! --- Declaration of variables -------------------------------------
    class(*) , pointer :: p1,p2
    type(ListIterator) :: first,last,extra,next
    ! --- Executable Code ----------------------------------------------
    first = t_this%begin()
    last  = t_this%end()
    extra = last
    
    do while (first /= last)
       next = first
       call next%next()

       if (BinaryPredicate(first%tp_node_%ta_data_,t_value)) then
          ! We do this in case we passed a pointer of a listvalue to
          ! the t_value argument
          p1 => t_value
          p2 => first%tp_node_%ta_data_
          if (associated(p1,p2)) then
             extra = first
          else
             call erase_element__(first)
          end if
       end if
       first = next
    end do
    if (extra /= last) then
       call erase_element__(extra)
    end if
  end subroutine remove_value__

  ! Removes every element in the list for which the predicate
  ! pred(elem) returns true.  Remaining elements stay in list order.
  ! Note that this function only erases the elements, and that if
  ! the elements themselves are pointers, the pointed-to memory is
  ! not touched in any way.  Managing the pointer is the user's
  ! responsibility.
  subroutine remove_if__(t_this,UnaryPredicate)
  ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout), target :: t_this
    interface
       logical function UnaryPredicate(x)
         class(*), intent(in) :: x
       end function UnaryPredicate
    end interface
    ! --- Declaration of variables -------------------------------------
    type(ListIterator) :: first,last,next
    ! --- Executable Code ----------------------------------------------
    first = t_this%begin()
    last  = t_this%end()
    
    do while (first /= last)
       next = first
       call next%next()
       if (UnaryPredicate(first%tp_node_%ta_data_)) then
          call erase_element__(first)
       end if
       first = next
    end do
  end subroutine remove_if__

  ! Removes every element in the list for which the predicate
  ! pred(elem) returns true.  Remaining elements stay in list order.
  ! Note that this function only erases the elements, and that if
  ! the elements themselves are pointers, the pointed-to memory is
  ! not touched in any way.  Managing the pointer is the user's
  ! responsibility.
  subroutine remove_unallocated__(t_this)
  ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout), target :: t_this
    ! --- Declaration of variables -------------------------------------
    type(ListIterator) :: first,last,next
    ! --- Executable Code ----------------------------------------------
    first = t_this%begin()
    last  = t_this%end()

    do while (first /= last)
       next = first
       call next%next()
       if (.not. allocated(first%tp_node_%ta_data_)) then
          call erase_element__(first)
       end if
       first = next
    end do
  end subroutine remove_unallocated__
  
  ! For each consecutive set of elements [first,last) that satisfy
  ! pred(first,i) where i is an iterator in [first,last), remove all
  ! but the first from every consecutive group of equal elements in
  ! the container. Remaining elements stay in list order.  Note that
  ! this function only erases the elements, and that if the elements
  ! themselves are pointers, the pointed-to memory is not touched in
  ! any way. Managing the pointer is the user's responsibility.
  subroutine unique__(t_this,BinaryPredicate)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    interface
       logical function BinaryPredicate(x,y)
         class(*), intent(in) :: x,y
       end function BinaryPredicate
    end interface
    ! --- Declaration of variables -------------------------------------
    type(ListIterator) :: first, last, next
    ! --- Executable Code ----------------------------------------------
    first = t_this%begin()
    last  = t_this%end()
    if (first /= last) then
       next = first
       call next%next()
       do while (next /= last)
          if (BinaryPredicate(first%tp_node_%ta_data_, &
                              next %tp_node_%ta_data_  )) then
             call erase_element__(next)
          else
             first = next
          end if
          next = first
          call next%next()
       end do
    end if
  end subroutine unique__

  ! Merges y into the list x by transferring all of its elements at
  ! their respective ordered positions into the container (both
  ! containers shall already be ordered). This effectively removes
  ! all the elements in y (which becomes empty), and inserts them
  ! into their ordered position within list x (which expands in size
  ! by the number of elements transferred). The operation is
  ! performed without constructing nor destroying any element: they
  ! are transferred.

  subroutine merge__(t_this, t_that, BinaryPredicate)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this, t_that
    interface
       logical function BinaryPredicate(x,y)
         class(*), intent(in) :: x,y
       end function BinaryPredicate
    end interface
    ! --- Declaration of variables -------------------------------------
    type(ListIterator) :: first1, last1
    type(ListIterator) :: first2, last2
    type(ListIterator) :: next
    ! --- Executable Code ----------------------------------------------
    if (.not.associated(t_this%t_node_%tp_next_%tp_prev_, &
                        t_that%t_node_%tp_next_%tp_prev_)) then
       first1 = t_this%begin()
       last1  = t_this%end()
       first2 = t_that%begin()
       last2  = t_that%end()

       do while (first1 /= last1 .and. first2 /= last2)
          if (BinaryPredicate(first2%tp_node_%ta_data_,&
                              first1%tp_node_%ta_data_ )) then
             next = first2
             call next%next()
             call first1%tp_node_%transfer(first2%tp_node_, &
                                           next%tp_node_)
!             CALL splice_range__(first1,first2,next)
             first2 = next
          else
             call first1%next()
          end if
       end do
       if (first2 /= last2) then
          call last1%tp_node_%transfer(first2%tp_node_, &
                                       last2%tp_node_ )
!         CALL splice_range__(last1,first2,last2)
       end if
    end if
  end subroutine merge__
  
  ! Reverse the order of the list this
  subroutine reverse__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    call t_this%t_node_%reverse()
  end subroutine reverse__


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
  subroutine sort__(t_this,BinaryPredicate)
    ! --- Declaration of arguments -------------------------------------
    class(List), intent(inout) :: t_this
    interface
       logical function BinaryPredicate(x,y)
         class(*), intent(in) :: x,y
       end function BinaryPredicate
    end interface
    ! --- Declaration of variables -------------------------------------
    type(List)                :: carry
    type(List), dimension(64) :: tmp
    integer                   :: fill, counter, i
    ! --- Executable Code ----------------------------------------------
    call initialise_void__(carry)
    do i = 1,64
       call initialise_void__(tmp(i))
    end do

    ! Do nothing if the list has length 0 or 1
    if (.not.associated(t_this%t_node_%tp_next_        , &
                        t_this%t_node_%tp_next_%tp_prev_ ) .and. &

         .not.associated(t_this%t_node_%tp_next_%tp_next_, &
                         t_this%t_node_%tp_next_%tp_prev_  )       ) then
       fill = 1
       do
          call splice_element__(carry%begin(),t_this%begin())
          counter = 1
          do while (counter /= fill .and. &
               .not. tmp(counter)%empty() )
             call merge__(tmp(counter),carry,BinaryPredicate)
             call swap__(carry,tmp(counter))
             counter = counter + 1
          end do
          call swap__(carry,tmp(counter))
          if (counter == fill) fill = fill + 1
          if (t_this%empty()) exit
       end do
       counter = 2
       do while (counter  /= fill)
          call merge__(tmp(counter),tmp(counter-1),BinaryPredicate)
          counter = counter + 1
       end do

       call swap__(t_this,tmp(fill-1))

    end if
  end subroutine sort__


  
end module gfcl_list

