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
     procedure(op1_fi), deferred, pass :: equal_
     procedure(op1_fi), deferred, pass :: notequal_
     procedure(op2_fi), deferred, pass :: next_

     procedure(op3_fi), deferred, pass, public :: get
     procedure(op4_fi), deferred, pass, public :: set

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
     procedure(op2_bi), deferred, pass :: prev_

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
     procedure(op1_ri), deferred, pass(t_this) :: lower_than_
     procedure(op1_ri), deferred, pass(t_this) :: lower_than_equal_
     procedure(op1_ri), deferred, pass(t_this) :: greater_than_
     procedure(op1_ri), deferred, pass(t_this) :: greater_than_equal_

     procedure(op2_ri), deferred, pass(t_this) :: next_
     procedure(op2_ri), deferred, pass(t_this) :: prev_

     procedure(op3_ri), deferred, pass(t_this) :: add_integer_
     procedure(op3_ri), deferred, pass(t_this) :: subtract_integer_
     procedure(op4_ri), deferred, pass(t_this) :: add_integer_pre_

     procedure(op5_ri), deferred, pass(t_this) :: subtract_iterator_

     generic, public :: operator(<)  => lower_than_
     generic, public :: operator(<=) => lower_than_equal_
     generic, public :: operator(>)  => greater_than_
     generic, public :: operator(>=) => greater_than_equal_

     generic, public :: next => add_integer_
     generic, public :: prev => subtract_integer_
     
  end type RandomAccessIterator


  abstract interface

     function op1_fi(t_this, t_that) result(b)
       import :: ForwardIterator
       class(ForwardIterator), intent(in) :: t_this, t_that
       logical                            :: b
     end function op1_fi

     subroutine op2_fi(t_this)
       import :: ForwardIterator
       class(ForwardIterator), intent(inout)  :: t_this
     end subroutine op2_fi

     function op3_fi(t_this) result(tp_value)
       import :: ForwardIterator
       class(ForwardIterator), intent(in) :: t_this
       class(*),               pointer    :: tp_value
     end function op3_fi

     subroutine op4_fi(t_this,t_value)
       import :: ForwardIterator
       class(ForwardIterator), intent(inout) :: t_this
       class(*),               intent(in)    :: t_value
     end subroutine op4_fi

     function op1_bi(t_this, t_that) result(b)
       import :: BidirectionalIterator
       class(BidirectionalIterator), intent(in) :: t_this, t_that
       logical                             :: b
     end function op1_bi
     
     subroutine op2_bi(t_this)
       import :: BidirectionalIterator
       class(BidirectionalIterator), intent(inout)  :: t_this
     end subroutine op2_bi



     function op1_ri(t_this, t_that) result(b)
       import :: RandomAccessIterator
       class(RandomAccessIterator), intent(in) :: t_this, t_that
       logical                                 :: b
     end function op1_ri

     subroutine op2_ri(t_this)
       import :: RandomAccessIterator
       class(RandomAccessIterator), intent(inout)  :: t_this
     end subroutine op2_ri

     subroutine op3_ri(t_this, n)
       import :: RandomAccessIterator
       class(RandomAccessIterator), intent(inout)  :: t_this
       integer                    , intent(in)     :: n
     end subroutine op3_ri

     function op4_ri(n, t_this) result(itr)
       import :: RandomAccessIterator
       class(RandomAccessIterator), intent(in)  :: t_this
       integer                    , intent(in)  :: n
       class(RandomAccessIterator), allocatable :: itr
     end function op4_ri
     
     function op5_ri(t_this, t_that) result(itr)
       import :: RandomAccessIterator
       class(RandomAccessIterator), intent(in)  :: t_this, t_that
       class(RandomAccessIterator), allocatable :: itr
     end function op5_ri
  end interface

contains

  subroutine advance(forward_iterator,n)
    ! --- Declaration of arguments -------------------------------------
    class(ForwardIterator), intent(inout) :: forward_iterator
    integer, value                        :: n
    ! --- executable code ----------------------------------------------
    select type(forward_iterator)
    class IS (RandomAccessIterator)
       call forward_iterator%next(n)
    class IS (BidirectionalIterator)
       if (n > 0) then
          do while (n /= 0)
              call forward_iterator%next()
             n = n-1
          end do
       else
          do while (n /= 0)
             call forward_iterator%next()
             n = n+1
          end do
       end if
    class IS (ForwardIterator)
       if (n < 0) ERROR stop "forward_iterator can only advance forward"
       do while (n /= 0)
          call forward_iterator%next()
          n = n-1
       end do
    end select
  end subroutine advance


  
end module gfcl_iterators
