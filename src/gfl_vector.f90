!===================================================================
!
! PROJECT : Generic Fortran Container Library
!
! NAME : variable sized arrays (Vectors)
!
! SLT_VECTOR :
!
! Vectors are sequence containers representing arrays that can
! change in size.
!
! Just like arrays, vectors use contiguous storage locations for
! their elements, which means that their elements can also be
! accessed using offsets on regular pointers to its elements, and
! just as efficiently as in arrays. But unlike arrays, their size
! can change dynamically, with their storage being handled
! automatically by the container.
!
! Internally, vectors use a dynamically allocated array to store
! their elements. This array may need to be reallocated in order to
! grow in size when new elements are inserted, which implies
! allocating a new array and moving all elements to it. This is a
! relatively expensive task in terms of processing time, and thus,
! vectors do not reallocate each time an element is added to the
! container.
!
! Instead, vector containers may allocate some extra storage to
! accommodate for possible growth, and thus the container may have
! an actual capacity greater than the storage strictly needed to
! contain its elements (i.e., its size). Libraries can implement
! different strategies for growth to balance between memory usage
! and reallocations, but in any case, reallocations should only
! happen at logarithmically growing intervals of size so that the
! insertion of individual elements at the end of the vector can be
! provided with amortized constant time complexity (see push_back).
!
! Therefore, compared to arrays, vectors consume more memory in
! exchange for the ability to manage storage and grow dynamically in
! an efficient way.
!
! Compared to the other dynamic sequence containers (deques, lists
! and forward_lists), vectors are very efficient accessing its
! elements (just like arrays) and relatively efficient adding or
! removing elements from its end. For operations that involve
! inserting or removing elements at positions other than the end,
! they perform worse than the others, and have less consistent
! iterators and references than lists and forward_lists.
!
! USAGE
!
! This file is used as an include file for a linked list module
! specific to a data type. The user should provide this data type
! into the module and this file includes the generic code for the
! lists. The conditions for the data_type are the following :
!
! - provides an interface function stl_new which represents the
!   default constructor and a copy constructor.
! - provides an interface function stl_delete which represents the
!   data_types destructor.
!
! REFERENCES : c++ standard library
!            : http://www.cplusplus.com
!            : gnu libstdc++
!
!===================================================================

module gfcl_vector
  !--- USE statements ------------------------------------------------
  use gfcl_vector_iterator
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Private/public section ----------------------------------------
  private
  !--- Public and protected statements -------------------------------
  ! types
  public :: vector_iterator
  public :: stl_vector
  !--- TYPE declarations ---------------------------------------------

  !=== stl-vector ====================================================
  ! A standard container which offers fixed time access to individual
  ! elements in any order.
  
  type, public :: Vector
     !--- Component part
     private
     class(*), dimension(:), allocatable :: ta_data_
     integer                             :: i_start_          = 0
     integer                             :: i_finish_         = 0
     integer                             :: i_end_of_storage_ = 0
     
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
     procedure, public :: reserve        => reserve__
     procedure, public :: capacity       => capacity__
     procedure, public :: shrink_to_fit  => shrink_to_fit__
     
  end type Vector

contains
  subroutine initialise_void__(this)
    ! --- Declaration of arguments -------------------------------------
    class(Vector), intent(inout) :: this
    ! --- Executable Code ----------------------------------------------
    if (allocated(this%ta_data_)) call clear__(this)
    this%i_start_          = 0
    this%i_finish_         = 0
    this%i_end_of_storage_ = 0
  end subroutine initialise_void__

  ! initialise_fill__
  ! Constructs a container filled with count values
  subroutine initialise_fill__(this,count,value,mold)
    ! --- Declaration of arguments -------------------------------------
    class(Vector), intent(inout)           :: this
    integer      , intent(in)              :: count
    class(*)     , intent(in)   , optional :: value
    class(*)     , intent(in)   , optional :: mold
    ! --- Executable Code ----------------------------------------------
    call initialise_void__(this)
    if (present(value)) call insert_fill__(this%end(),count,value)
    else call reserve__(this,count,mold)
  end subroutine initialise_fill__

end module gfcl_vector
