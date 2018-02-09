MODULE gfcl_iterators

  ! Forward iterators are iterators that can be used to access the
  ! sequence of elements in a range in the direction that goes from
  ! its beginning towards its end.

  TYPE, ABSTRACT :: ForwardIterator
!     INTEGER, ALLOCATABLE :: ICE_dummy
   CONTAINS
     PRIVATE
     PROCEDURE(op1_fi), DEFERRED, PASS :: equal_
     PROCEDURE(op1_fi), DEFERRED, PUBLIC, PASS :: notequal_
     PROCEDURE(op2_fi), DEFERRED, PASS :: next_

     PROCEDURE(op3_fi), DEFERRED, PASS, PUBLIC :: get
     PROCEDURE(op4_fi), DEFERRED, PASS, PUBLIC :: set

     GENERIC, PUBLIC :: OPERATOR(==) => equal_
     GENERIC, PUBLIC :: OPERATOR(/=) => notequal_

     GENERIC, PUBLIC :: next => next_
  END TYPE ForwardIterator

  ! Bidirectional iterators are iterators that can be used to access
  ! the SEQUENCE of elements in a range in both directions (towards
  ! the END and towards the beginning).

  TYPE, EXTENDS(ForwardIterator), ABSTRACT :: BidirectionalIterator
   CONTAINS
     PRIVATE
     PROCEDURE(op2_bi), DEFERRED, PASS :: prev_

     GENERIC, PUBLIC :: prev => prev_
  END TYPE BidirectionalIterator

  ! Random-access iterators are iterators that can be used to access
  ! elements at an arbitrary offset position relative to the element
  ! they point to, offering the same functionality as pointers.
  !`
  ! Random-access iterators are the most complete iterators in terms
  ! of functionality.

  TYPE, EXTENDS(BidirectionalIterator), ABSTRACT :: RandomAccessIterator

   CONTAINS
     PRIVATE
     PROCEDURE(op1_ri), DEFERRED, PASS(t_this) :: lower_than_
     PROCEDURE(op1_ri), DEFERRED, PASS(t_this) :: lower_than_equal_
     PROCEDURE(op1_ri), DEFERRED, PASS(t_this) :: greater_than_
     PROCEDURE(op1_ri), DEFERRED, PASS(t_this) :: greater_than_equal_

     PROCEDURE(op2_ri), DEFERRED, PASS(t_this) :: next_
     PROCEDURE(op2_ri), DEFERRED, PASS(t_this) :: prev_

     PROCEDURE(op3_ri), DEFERRED, PASS(t_this) :: add_integer_
     PROCEDURE(op3_ri), DEFERRED, PASS(t_this) :: subtract_integer_
     PROCEDURE(op4_ri), DEFERRED, PASS(t_this) :: add_integer_pre_

     PROCEDURE(op5_ri), DEFERRED, PASS(t_this) :: subtract_iterator_

     GENERIC, PUBLIC :: OPERATOR(<)  => lower_than_
     GENERIC, PUBLIC :: OPERATOR(<=) => lower_than_equal_
     GENERIC, PUBLIC :: OPERATOR(>)  => greater_than_
     GENERIC, PUBLIC :: OPERATOR(>=) => greater_than_equal_

     GENERIC, PUBLIC :: next => add_integer_
     GENERIC, PUBLIC :: prev => subtract_integer_
     
  END TYPE RandomAccessIterator


  ABSTRACT INTERFACE

     FUNCTION op1_fi(t_this, t_that) RESULT(b)
       IMPORT :: ForwardIterator
       CLASS(ForwardIterator), INTENT(in) :: t_this, t_that
       LOGICAL                            :: b
     END FUNCTION op1_fi

     SUBROUTINE op2_fi(t_this)
       IMPORT :: ForwardIterator
       CLASS(ForwardIterator), INTENT(inout)  :: t_this
     END SUBROUTINE op2_fi

     FUNCTION op3_fi(t_this) RESULT(tp_value)
       IMPORT :: ForwardIterator
       CLASS(ForwardIterator), INTENT(in) :: t_this
       CLASS(*),               POINTER    :: tp_value
     END FUNCTION op3_fi

     SUBROUTINE op4_fi(t_this,t_value)
       IMPORT :: ForwardIterator
       CLASS(ForwardIterator), INTENT(inout) :: t_this
       CLASS(*),               INTENT(in)    :: t_value
     END SUBROUTINE op4_fi

     FUNCTION op1_bi(t_this, t_that) RESULT(b)
       IMPORT :: BidirectionalIterator
       CLASS(BidirectionalIterator), INTENT(in) :: t_this, t_that
       LOGICAL                             :: b
     END FUNCTION op1_bi
     
     SUBROUTINE op2_bi(t_this)
       IMPORT :: BidirectionalIterator
       CLASS(BidirectionalIterator), INTENT(inout)  :: t_this
     END SUBROUTINE op2_bi



     FUNCTION op1_ri(t_this, t_that) RESULT(b)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(in) :: t_this, t_that
       LOGICAL                                 :: b
     END FUNCTION op1_ri

     SUBROUTINE op2_ri(t_this)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(inout)  :: t_this
     END SUBROUTINE op2_ri

     SUBROUTINE op3_ri(t_this, n)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(inout)  :: t_this
       INTEGER                    , INTENT(in)     :: n
     END SUBROUTINE op3_ri

     FUNCTION op4_ri(n, t_this) RESULT(itr)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(in)  :: t_this
       INTEGER                    , INTENT(in)  :: n
       CLASS(RandomAccessIterator), ALLOCATABLE :: itr
     END FUNCTION op4_ri
     
     FUNCTION op5_ri(t_this, t_that) RESULT(itr)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(in)  :: t_this, t_that
       CLASS(RandomAccessIterator), ALLOCATABLE :: itr
     END FUNCTION op5_ri
  END INTERFACE

CONTAINS

  SUBROUTINE advance(forward_iterator,n)
    ! --- Declaration of arguments -------------------------------------
    CLASS(ForwardIterator), INTENT(inout) :: forward_iterator
    INTEGER, VALUE                        :: n
    ! --- executable code ----------------------------------------------
    SELECT TYPE(forward_iterator)
    CLASS IS (RandomAccessIterator)
       CALL forward_iterator%next(n)
    CLASS IS (BidirectionalIterator)
       IF (n > 0) THEN
          DO WHILE (n /= 0)
              CALL forward_iterator%next()
             n = n-1
          END DO
       ELSE
          DO WHILE (n /= 0)
             CALL forward_iterator%next()
             n = n+1
          END DO
       END IF
    CLASS IS (ForwardIterator)
       IF (n < 0) ERROR STOP "forward_iterator can only advance forward"
       DO WHILE (n /= 0)
          CALL forward_iterator%next()
          n = n-1
       END DO
    END SELECT
  END SUBROUTINE advance


  
END MODULE gfcl_iterators
