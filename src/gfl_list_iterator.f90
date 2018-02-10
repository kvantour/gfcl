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
  subroutine initialise_void__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    nullify(t_this%tp_node_)
  end subroutine initialise_void__

  ! creates a copy from an iterator
  subroutine initialise_copy__(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout)         :: t_this
    class(ListIterator), intent(in)   , target :: t_that
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_that%tp_node_
  end subroutine initialise_copy__

  subroutine final__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    nullify(t_this%tp_node_)
  end subroutine final__

  ! next
  subroutine next__(t_this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_this%tp_node_%tp_next_
  end subroutine next__

  ! dec
  subroutine prev__(t_this)
    ! --- Declaration of arguments -------------------------------------
    ! decrements the iterator it and returns it
    class(ListIterator), intent(inout) :: t_this
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_this%tp_node_%tp_prev_
  end subroutine prev__

  ! eq
  function equal__(t_this,t_that) result(b)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    class(ListIterator)   , intent(in) :: t_this
    class(ForwardIterator), intent(in) :: t_that
    logical                            :: b
    ! --- Executable Code ----------------------------------------------
    select type (t_that)
    type is (ListIterator)
       b = associated(t_this%tp_node_,t_that%tp_node_)
    class default
       b = .false.
    end select
  end function equal__

  ! neq
  function notequal__(t_this,t_that) result(b)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    class(ListIterator)   , intent(in) :: t_this
    class(ForwardIterator), intent(in) :: t_that
    logical                            :: b
    ! --- Executable Code ----------------------------------------------
    select type (t_that)
    type IS (ListIterator)
       b = .not.associated(t_this%tp_node_,t_that%tp_node_)
    class default
       b = .true.
    end select
  end function notequal__

  ! assign
  subroutine assign(t_this,t_that)
    ! --- Declaration of arguments -------------------------------------
    ! make t_this and t_that equal iterators
    type(ListIterator), intent(out) :: t_this
    type(ListIterator), intent(in)  :: t_that
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_that%tp_node_
  end subroutine assign

  ! assign_node
  subroutine assign_node(t_this,t_node)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(out)        :: t_this
    type(ListNode)    , intent(in), target :: t_node
    ! --- Executable Code ----------------------------------------------
    t_this%tp_node_ => t_node
  end subroutine assign_node

  ! get_at
  function get__(t_this) result(tp_value)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(in) :: t_this
    class(*)           , pointer    :: tp_value
    ! --- Executable Code ----------------------------------------------
    tp_value => t_this%tp_node_%ta_data_
  end function get__

  ! set_at
  subroutine set__(t_this,t_value)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: t_this
    class(*)           , intent(in)    :: t_value
    ! --- Executable Code ----------------------------------------------
    call t_this%tp_node_%set(t_value)
  end subroutine set__

  ! transfer
  subroutine transfer(t_this,t_first,t_last)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), intent(inout) :: t_this,t_first,t_last
    ! --- Executable Code ----------------------------------------------
    call t_this%tp_node_%transfer(t_first%tp_node_,t_last%tp_node_)
  end subroutine transfer
  
  ! distance
  function distance(t_first,t_last) result(n)
    ! --- Declaration of arguments -------------------------------------
    type(ListIterator), value :: t_first,t_last
    integer                   :: n
    ! --- Executable Code ----------------------------------------------
    n = 0
    do while (t_first /= t_last)
       n = n + 1
       call t_first%next()
    end do
  end function distance

end module gfcl_list_iterator
