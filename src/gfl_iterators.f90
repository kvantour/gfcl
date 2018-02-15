module gfcl_iterators
  !--- Implicit statement --------------------------------------------
  implicit none
  !--- Data types ----------------------------------------------------

  ! Forward iterators are iterators that can be used to access the
  ! sequence of elements in a range in the direction that goes from
  ! its beginning towards its end.

  type, abstract :: ForwardIterator
!     INTEGER, ALLOCATABLE :: ICE_dummy
   contains
     private
     procedure(ai_fi_compare), deferred, pass :: equal_
     procedure(ai_fi_compare), deferred, pass :: notequal_
     procedure(ai_fi_next)   , deferred, pass :: next_

     procedure(ai_fi_get), deferred, pass, public :: get
     procedure(ai_fi_set), deferred, pass, public :: set

     generic, public :: operator(==) => equal_
     generic, public :: operator(/=) => notequal_

     generic, public :: next => next_
  end type ForwardIterator

  ! Bidirectional iterators are iterators that can be used to access
  ! the SEQUENCE of elements in a range in both directions (towards
  ! the END and towards the beginning).

  type, extends(ForwardIterator), abstract :: BidirectionalIterator
   contains
     private
     procedure(ai_bi_prev), deferred, pass :: prev_

     generic, public :: prev => prev_
  end type BidirectionalIterator

  ! Random-access iterators are iterators that can be used to access
  ! elements at an arbitrary offset position relative to the element
  ! they point to, offering the same functionality as pointers.
  !`
  ! Random-access iterators are the most complete iterators in terms
  ! of functionality.

  type, extends(BidirectionalIterator), abstract :: RandomAccessIterator

   contains
     private
     procedure(ai_ri_compare)   , deferred, pass(this) :: lower_than_
     procedure(ai_ri_compare)   , deferred, pass(this) :: lower_than_equal_
     procedure(ai_ri_compare)   , deferred, pass(this) :: greater_than_
     procedure(ai_ri_compare)   , deferred, pass(this) :: greater_than_equal_

     procedure(ai_ri_prevnext)  , deferred, pass(this) :: next_
     procedure(ai_ri_prevnext)  , deferred, pass(this) :: prev_

     procedure(ai_ri_arithmetic), deferred, pass(this) :: add_integer_
     procedure(ai_ri_arithmetic), deferred, pass(this) :: subtract_integer_

     generic, public :: operator(<)  => lower_than_
     generic, public :: operator(<=) => lower_than_equal_
     generic, public :: operator(>)  => greater_than_
     generic, public :: operator(>=) => greater_than_equal_

     generic, public :: next => add_integer_
     generic, public :: prev => subtract_integer_
     
  end type RandomAccessIterator


  abstract interface

     function ai_fi_compare(this, other) result(b)
       import :: ForwardIterator
       class(ForwardIterator), intent(in) :: this, other
       logical                            :: b
     end function ai_fi_compare
     
     subroutine ai_fi_next(this)
       import :: ForwardIterator
       class(ForwardIterator), intent(inout)  :: this
     end subroutine ai_fi_next

     function ai_fi_get(this) result(value)
       import :: ForwardIterator
       class(ForwardIterator), intent(in) :: this
       class(*),               pointer    :: value
     end function ai_fi_get

     subroutine ai_fi_set(this,value)
       import :: ForwardIterator
       class(ForwardIterator), intent(inout) :: this
       class(*),               intent(in)    :: value
     end subroutine ai_fi_set

     subroutine ai_bi_prev(this)
       import :: BidirectionalIterator
       class(BidirectionalIterator), intent(inout) :: this
     end subroutine ai_bi_prev



     function ai_ri_compare(this, other) result(b)
       import :: RandomAccessIterator
       class(RandomAccessIterator), intent(in) :: this, other
       logical                                 :: b
     end function ai_ri_compare

     subroutine ai_ri_prevnext(this)
       import :: RandomAccessIterator
       class(RandomAccessIterator), intent(inout)  :: this
     end subroutine ai_ri_prevnext

     subroutine ai_ri_arithmetic(this, n)
       import :: RandomAccessIterator
       class(RandomAccessIterator), intent(inout)  :: this
       integer                    , intent(in)     :: n
     end subroutine ai_ri_arithmetic

  end interface

contains

  subroutine advance(iterator,n)
    ! --- Declaration of arguments -------------------------------------
    class(ForwardIterator), intent(inout) :: iterator
    integer, value                        :: n
    ! --- executable code ----------------------------------------------
    select type(iterator)
    class is (RandomAccessIterator)
       call iterator%next(n)
    class is (BidirectionalIterator)
       if (n > 0) then
          do while (n /= 0)
              call iterator%next()
             n = n-1
          end do
       else
          do while (n /= 0)
             call iterator%next()
             n = n+1
          end do
       end if
    class is (ForwardIterator)
       if (n < 0) ERROR stop "iterator can only advance forward"
       do while (n /= 0)
          call iterator%next()
          n = n-1
       end do
    end select
  end subroutine advance


  
end module gfcl_iterators
