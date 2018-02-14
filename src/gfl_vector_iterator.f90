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
  public :: RandomAccessIterator
  public :: 
  !--- Data types ----------------------------------------------------
  ! a list iterator used to tranverse the list and point to objects to
  ! insert or delete.
  type, extends(RandomAccessIterator) :: VectorIterator
     !--- Component part
     class(*), dimension(:), pointer :: pd_pointer_ => null()
     integer                         :: i_index_
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


contains
  ! creates an empty iterator
  subroutine initialise_void__(this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    nullify(this%pd_pointer_)
    i_index_ = 0
  end subroutine initialise_void__

  ! creates a copy from an iterator
  subroutine initialise_copy__(this,other)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout)         :: this
    class(ListIterator), intent(in)   , target :: other
    ! --- Executable Code ----------------------------------------------
    this%pd_pointer_ => other%pd_pointer_
    this%i_index_    =  other%i_index_
  end subroutine initialise_copy__

  subroutine finalise__(this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    nullify(this%pd_pointer_)
  end subroutine finalise__

  ! next
  subroutine next__(this)
    ! --- Declaration of arguments -------------------------------------
    class(ListIterator), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    this%i_index_ = this%i_index_+1
  end subroutine next__

  ! dec
  subroutine prev__(this)
    ! --- Declaration of arguments -------------------------------------
    ! decrements the iterator it and returns it
    class(ListIterator), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    this%i_index_ = this%i_index_-1
  end subroutine prev__


  function equal__(this,other) result(b)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    class(ListIterator)   , intent(in) :: this
    class(ForwardIterator), intent(in) :: other
    logical                            :: b
    ! --- Executable Code ----------------------------------------------
    select type (other)
    type is (ListIterator)
       b = associated(this%pd_pointer_,other%pd_pointer_)
       b = this%i_index_ == other%i_index_ .and. b
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
    type is (ListIterator)
       b = .not.associated(this%pd_pointer_,other%pd_pointer_)
       b = this%i_index_/= other%i_index_ .and. b
    class default
       b = .true.
    end select
  end function notequal__

  subroutine add_integer_(this,n)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    class(VectorIterator) , intent(inout) :: this
    integer                               :: n
    ! --- Executable Code ----------------------------------------------
    this%i_index_ = this%i_index_ + n
  end function notequal__

  subroutine subtract_integer_(this,n)
    ! --- Declaration of arguments -------------------------------------
    ! returns true if it1 and it2 point to the same node
    class(VectorIterator) , intent(inout) :: this
    integer                               :: n
    ! --- Executable Code ----------------------------------------------
    this%i_index_ = this%i_index_ - n
  end function notequal__


end module gfcl_list_iterator
