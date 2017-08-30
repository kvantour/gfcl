MODULE gfcl_iterators

  ! Forward iterators are iterators that can be used to access the
  ! sequence of elements in a range in the direction that goes from
  ! its beginning towards its end.

  TYPE, ABSTRACT :: forward_iterator
   CONTAINS
     PRIVATE
     PROCEDURE(op1_fi), DEFERRED, PASS :: equal__
     PROCEDURE(op1_fi), DEFERRED, PASS :: notequal__
     PROCEDURE(op2_fi), DEFERRED, PASS :: next__

     GENERIC, PUBLIC :: OPERATOR(==) => equal__
     GENERIC, PUBLIC :: OPERATOR(/=) => notequal__

     GENERIC, PUBLIC :: next => next__
  END TYPE forward_iterator

  ! Bidirectional iterators are iterators that can be used to access
  ! the SEQUENCE of elements in a range in both directions (towards
  ! the END and towards the beginning).

  TYPE, EXTENDS(forward_iterator), ABSTRACT :: bidirectional_iterator
   CONTAINS
     PRIVATE
     PROCEDURE(op2_bi), DEFERRED, PASS :: prev__

     GENERIC, PUBLIC :: prev => prev__
  END TYPE bidirectional_iterator

  ! Random-access iterators are iterators that can be used to access
  ! elements at an arbitrary offset position relative to the element
  ! they point to, offering the same functionality as pointers.
  !`
  ! Random-access iterators are the most complete iterators in terms
  ! of functionality.

  TYPE, EXTENDS(bidirectional_iterator), ABSTRACT :: random_access_iterator

   CONTAINS
     PRIVATE
     PROCEDURE(op1_ri), DEFERRED, PASS(c_this) :: add_integer__
     PROCEDURE(op1_ri), DEFERRED, PASS(c_this) :: subtract_integer__
     PROCEDURE(op2_ri), DEFERRED, PASS(c_this) :: add_integer_pre
     PROCEDURE(op3_ri), DEFERRED, PASS(c_this) :: subtract_iterator
     PROCEDURE(op4_ri), DEFERRED, PASS(c_this) :: lower_than__
     PROCEDURE(op4_ri), DEFERRED, PASS(c_this) :: lower_than_equal__
     PROCEDURE(op4_ri), DEFERRED, PASS(c_this) :: greater_than__
     PROCEDURE(op4_ri), DEFERRED, PASS(c_this) :: greater_than_equal__

     GENERIC, PUBLIC :: OPERATOR(<)  => lower_than__
     GENERIC, PUBLIC :: OPERATOR(<=) => lower_than_equal__
     GENERIC, PUBLIC :: OPERATOR(>)  => greater_than__
     GENERIC, PUBLIC :: OPERATOR(>=) => greater_than_equal__

     GENERIC, PUBLIC :: next => next__, add_integer__
     GENERIC, PUBLIC :: prev => prev__, subtract_integer__
     
  END TYPE random_access_iterator


  ABSTRACT INTERFACE

     FUNCTION op1_fi(c_this, c_that) RESULT(b)
       IMPORT :: forward_iterator
       CLASS(forward_iterator), INTENT(in) :: c_this, c_that
       LOGICAL                             :: b
     END FUNCTION op1_fi

     SUBROUTINE op2_fi(c_this)
       IMPORT :: forward_iterator
       CLASS(forward_iterator), INTENT(inout)  :: c_this
     END SUBROUTINE op2_fi

     FUNCTION op1_bi(c_this, c_that) RESULT(b)
       IMPORT :: bidirectional_iterator
       CLASS(bidirectional_iterator), INTENT(in) :: c_this, c_that
       LOGICAL                             :: b
     END FUNCTION op1_bi
     
     SUBROUTINE op2_bi(c_this)
       IMPORT :: bidirectional_iterator
       CLASS(bidirectional_iterator), INTENT(inout)  :: c_this
     END SUBROUTINE op2_bi

     SUBROUTINE op1_ri(c_this, n)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(inout)  :: c_this
       INTEGER                      , INTENT(in)     :: n
     END SUBROUTINE op1_ri

     FUNCTION op2_ri(n, c_this) RESULT(itr)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(in)  :: c_this
       INTEGER                      , INTENT(in)  :: n
       CLASS(random_access_iterator), ALLOCATABLE :: itr
     END FUNCTION op2_ri
     
     FUNCTION op3_ri(c_this, c_that) RESULT(itr)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(in)  :: c_this, c_that
       CLASS(random_access_iterator), ALLOCATABLE :: itr
     END FUNCTION op3_ri

     FUNCTION op4_ri(c_this, c_that) RESULT(b)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(in) :: c_this, c_that
       LOGICAL                                   :: b
     END FUNCTION op4_ri
  END INTERFACE

CONTAINS

  SUBROUTINE advance(ForwardIterator,n)
    CLASS(forward_iterator), INTENT(inout) :: ForwardIterator
    INTEGER, VALUE                         :: n
    SELECT TYPE(ForwardIterator)
    CLASS IS (random_access_iterator)
       CALL ForwardIterator%next(n)
    CLASS IS (bidirectional_iterator)
       IF (n > 0) THEN
          DO WHILE (n /= 0)
              CALL ForwardIterator%next()
             n = n-1
          END DO
       ELSE
          DO WHILE (n /= 0)
             CALL ForwardIterator%next()
             n = n+1
          END DO
       END IF
    CLASS IS (forward_iterator)
       IF (n < 0) ERROR STOP "forward_iterator can only advance forward"
       DO WHILE (n /= 0)
          CALL ForwardIterator%next()
          n = n-1
       END DO
    END SELECT
  END SUBROUTINE advance


  
END MODULE gfcl_iterators
