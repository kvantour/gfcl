MODULE gfcl_iterators

  ! Forward iterators are iterators that can be used to access the
  ! sequence of elements in a range in the direction that goes from
  ! its beginning towards its end.

  TYPE, ABSTRACT :: forward_iterator
   CONTAINS
     PRIVATE
     PROCEDURE(op1_fi), DEFERRED, PASS :: equal
     PROCEDURE(op1_fi), DEFERRED, PASS :: notequal

     PROCEDURE(op2_fi), PUBLIC, DEFERRED, PASS :: next

     GENERIC, PUBLIC :: OPERATOR(==) => equal
     GENERIC, PUBLIC :: OPERATOR(/=) => notequal
  END TYPE forward_iterator

  ! Bidirectional iterators are iterators that can be used to access
  ! the SEQUENCE of elements in a range in both directions (towards
  ! the END and towards the beginning).

  TYPE, EXTENDS(forward_iterator), ABSTRACT :: bidirectional_iterator
   CONTAINS
!     PROCEDURE(op1_bi), DEFERRED, PASS(c_this) :: equal
!     PROCEDURE(op1_bi), DEFERRED, PASS(c_this) :: notequal
     PRIVATE
     PROCEDURE(op2_bi), PUBLIC, DEFERRED, PASS :: prev
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
     PROCEDURE(op1_ri), DEFERRED, PASS(c_this) :: add_integer_post
     PROCEDURE(op1_ri), DEFERRED, PASS(c_this) :: subtract_integer_post
     PROCEDURE(op2_ri), DEFERRED, PASS(c_this) :: add_integer_pre
     PROCEDURE(op3_ri), DEFERRED, PASS(c_this) :: subtract_iterator
     PROCEDURE(op4_ri), DEFERRED, PASS(c_this) :: lt
     PROCEDURE(op4_ri), DEFERRED, PASS(c_this) :: gt
     PROCEDURE(op4_ri), DEFERRED, PASS(c_this) :: gte
     PROCEDURE(op4_ri), DEFERRED, PASS(c_this) :: lte

     GENERIC, PUBLIC :: OPERATOR(+)  => add_integer_post, add_integer_pre
     GENERIC, PUBLIC :: OPERATOR(-)  => subtract_integer_post, subtract_iterator
     GENERIC, PUBLIC :: OPERATOR(<)  => lt
     GENERIC, PUBLIC :: OPERATOR(<=) => lte
     GENERIC, PUBLIC :: OPERATOR(>)  => gt
     GENERIC, PUBLIC :: OPERATOR(>=) => gte
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

     FUNCTION op1_ri(c_this, n) RESULT(itr)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(in)  :: c_this
       INTEGER                      , INTENT(in)  :: n
       CLASS(random_access_iterator), ALLOCATABLE :: itr
     END FUNCTION op1_ri

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
  
END MODULE gfcl_iterators
