MODULE gfcl_iterators

  ! Forward iterators are iterators that can be used to access the
  ! sequence of elements in a range in the direction that goes from
  ! its beginning towards its end.

  TYPE, ABSTRACT :: ForwardIterator
   CONTAINS
     PRIVATE
     PROCEDURE(op1_fi), DEFERRED, PASS :: equal_
     PROCEDURE(op1_fi), DEFERRED, PASS :: notequal_
     PROCEDURE(op2_fi), DEFERRED, PASS :: next_

     GENERIC, PUBLIC :: OPERATOR(==) => equal_
     GENERIC, PUBLIC :: OPERATOR(/=) => notequal_

     GENERIC, PUBLIC :: next => next_
  END TYPE ForwardIterator

  ! Bidirectional iterators are iterators that can be used to access
  ! the SEQUENCE of elements in a range in both directions (towards
  ! the END and towards the beginning).

  TYPE, EXTENDS(forwardIterator), ABSTRACT :: BidirectionalIterator
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
     PROCEDURE(op1_ri), DEFERRED, PASS(t_this) :: add_integer_
     PROCEDURE(op1_ri), DEFERRED, PASS(t_this) :: subtract_integer_
     PROCEDURE(op2_ri), DEFERRED, PASS(t_this) :: add_integer_pre
     PROCEDURE(op3_ri), DEFERRED, PASS(t_this) :: subtractIterator
     PROCEDURE(op4_ri), DEFERRED, PASS(t_this) :: lower_than_
     PROCEDURE(op4_ri), DEFERRED, PASS(t_this) :: lower_than_equal_
     PROCEDURE(op4_ri), DEFERRED, PASS(t_this) :: greater_than_
     PROCEDURE(op4_ri), DEFERRED, PASS(t_this) :: greater_than_equal_

     GENERIC, PUBLIC :: OPERATOR(<)  => lower_than_
     GENERIC, PUBLIC :: OPERATOR(<=) => lower_than_equal_
     GENERIC, PUBLIC :: OPERATOR(>)  => greater_than_
     GENERIC, PUBLIC :: OPERATOR(>=) => greater_than_equal_

     GENERIC, PUBLIC :: next => next_, add_integer_
     GENERIC, PUBLIC :: prev => prev_, subtract_integer_
     
  END TYPE RandomAccessIterator


  ABSTRACT INTERFACE

     FUNCTION op1_fi(t_this, t_that) RESULT(b)
       IMPORT :: ForwardIterator
       CLASS(forwardIterator), INTENT(in) :: t_this, t_that
       LOGICAL                             :: b
     END FUNCTION op1_fi

     SUBROUTINE op2_fi(t_this)
       IMPORT :: ForwardIterator
       CLASS(forwardIterator), INTENT(inout)  :: t_this
     END SUBROUTINE op2_fi

     FUNCTION op1_bi(t_this, t_that) RESULT(b)
       IMPORT :: BidirectionalIterator
       CLASS(BidirectionalIterator), INTENT(in) :: t_this, t_that
       LOGICAL                             :: b
     END FUNCTION op1_bi
     
     SUBROUTINE op2_bi(t_this)
       IMPORT :: BidirectionalIterator
       CLASS(BidirectionalIterator), INTENT(inout)  :: t_this
     END SUBROUTINE op2_bi

     SUBROUTINE op1_ri(t_this, n)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(inout)  :: t_this
       INTEGER                      , INTENT(in)     :: n
     END SUBROUTINE op1_ri

     FUNCTION op2_ri(n, t_this) RESULT(itr)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(in)  :: t_this
       INTEGER                    , INTENT(in)  :: n
       CLASS(RandomAccessIterator), ALLOCATABLE :: itr
     END FUNCTION op2_ri
     
     FUNCTION op3_ri(t_this, t_that) RESULT(itr)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(in)  :: t_this, t_that
       CLASS(RandomAccessIterator), ALLOCATABLE :: itr
     END FUNCTION op3_ri

     FUNCTION op4_ri(t_this, t_that) RESULT(b)
       IMPORT :: RandomAccessIterator
       CLASS(RandomAccessIterator), INTENT(in) :: t_this, t_that
       LOGICAL                                 :: b
     END FUNCTION op4_ri
  END INTERFACE

CONTAINS

  SUBROUTINE advance(forward_iterator,n)
    ! --- Declaration of arguments -------------------------------------
    CLASS(forwardIterator), INTENT(inout) :: forward_iterator
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
    CLASS IS (forwardIterator)
       IF (n < 0) ERROR STOP "forward_iterator can only advance forward"
       DO WHILE (n /= 0)
          CALL forward_iterator%next()
          n = n-1
       END DO
    END SELECT
  END SUBROUTINE advance


  
END MODULE gfcl_iterators
