module gfcl_list_iterator
  !--- USE statements ------------------------------------------------
  use gfcl_iterators
  use gfcl_list_node
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Private/public section ----------------------------------------
  private
  public :: advance
  public :: ListIterator
  public :: ForwardIterator
  public :: BidirectionalIterator
  !--- Data types ----------------------------------------------------
  ! a list iterator used to tranverse the list and point to objects to
  ! insert or delete.
  type, extends(BidirectionalIterator) :: ListIterator
     !--- Component part
     type(ListNode), pointer :: tp_node_ => null()
     
   contains
     private
     !--- Type-bound-procedure part
     ! initialisors ====================================================
     procedure         :: initialise_void__
     procedure         :: initialise_copy__
     generic  , public :: initialise       => initialise_void__, &
                                              initialise_copy__

     ! comparators =====================================================
     procedure :: equal_    => equal__
     procedure :: notequal_ => notequal__

     ! modifiers =======================================================
     procedure :: next_     => next__
     procedure :: prev_     => prev__

     procedure, public :: get     => get__
     procedure, public :: set     => set__
  end type ListIterator

  !--- Interfaces ------------------------------------------------------
  interface assignment(=)
     module procedure assign, assign_node
  end interface

contains

  ! creates an empty iterator
  subroutine initialise_void__(this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    nullify(this%tp_node_)
  end subroutine initialise_void__

  ! creates a copy from an iterator
  subroutine initialise_copy__(this,other)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout)         :: this
    class(ListIterator), intent(in)   , target :: other
    ! --- Executable Code ----------------------------------------------
    this%tp_node_ => other%tp_node_
  end subroutine initialise_copy__

  subroutine final__(this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    nullify(this%tp_node_)
  end subroutine final__

  ! next
  subroutine next__(this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    this%tp_node_ => this%tp_node_%tp_next_
  end subroutine next__

  ! dec
  subroutine prev__(this)
    ! --- Declaration of arguments -------------------------------------
    ! decrements the iterator it and returns it
    class(ListIterator), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    this%tp_node_ => this%tp_node_%tp_prev_
  end subroutine prev__

  ! eq
  function equal__(this,other) result(b)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    class(ListIterator)   , intent(in) :: this
    class(ForwardIterator), intent(in) :: other
    logical                            :: b
    ! --- Executable Code ----------------------------------------------
    select type (other)
    type is (ListIterator)
       b = associated(this%tp_node_,other%tp_node_)
    class default
       b = .false.
    end select
  end function equal__

  ! neq
  function notequal__(this,other) result(b)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    class(ListIterator)   , intent(in) :: this
    class(ForwardIterator), intent(in) :: other
    logical                            :: b
    ! --- Executable Code ----------------------------------------------
    select type (other)
    type IS (ListIterator)
       b = .not.associated(this%tp_node_,other%tp_node_)
    class default
       b = .true.
    end select
  end function notequal__

  ! assign
  subroutine assign(this,other)
    ! --- Declaration of arguments -------------------------------------
    ! make this and other equal iterators
    type(ListIterator), intent(out) :: this
    type(ListIterator), intent(in)  :: other
    ! --- Executable Code ----------------------------------------------
    this%tp_node_ => other%tp_node_
  end subroutine assign

  ! assign_node
  subroutine assign_node(this,node)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(out)        :: this
    type(ListNode)    , intent(in), target :: node
    ! --- Executable Code ----------------------------------------------
    this%tp_node_ => node
  end subroutine assign_node

  ! get_at
  function get__(this) result(value)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(in) :: this
    class(*)           , pointer    :: value
    ! --- Executable Code ----------------------------------------------
    value => this%tp_node_%ta_data_
  end function get__

  ! set_at
  subroutine set__(this,value)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: this
    class(*)           , intent(in)    :: value
    ! --- Executable Code ----------------------------------------------
    call this%tp_node_%set(value)
  end subroutine set__

  ! transfer
  subroutine transfer(this,first,last)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(inout) :: this,first,last
    ! --- Executable Code ----------------------------------------------
    call this%tp_node_%transfer(first%tp_node_,last%tp_node_)
  end subroutine transfer
  
  ! distance
  function distance(first,last) result(n)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), value :: first,last
    integer                   :: n
    ! --- Executable Code ----------------------------------------------
    n = 0
    do while (first /= last)
       n = n + 1
       call first%next()
    end do
  end function distance

end module gfcl_list_iterator
