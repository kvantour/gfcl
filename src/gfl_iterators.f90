MODULE gfcl_iterators

  ! Forward iterators are iterators that can be used to access the
  ! sequence of elements in a range in the direction that goes from
  ! its beginning towards its end.

  TYPE, ABSTRACT :: forward_iterator
   CONTAINS
     PRIVATE
     PROCEDURE(op1_fi), DEFERRED, PASS :: equal
     PROCEDURE(op1_fi), DEFERRED, PASS :: notequal

     PROCEDURE(op2_fi), PUBLIC, DEFERRED :: next

     GENERIC, PUBLIC :: OPERATOR(==) => equal
     GENERIC, PUBLIC :: OPERATOR(/=) => notequal
  END TYPE forward_iterator

  ! Bidirectional iterators are iterators that can be used to access
  ! the SEQUENCE of elements in a range in both directions (towards
  ! the END and towards the beginning).

  TYPE, EXTENDS(forward_iterator), ABSTRACT :: bidirectional_iterator
   CONTAINS
     PRIVATE
     PROCEDURE(op1_bi), PUBLIC, DEFERRED :: prev
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
     PROCEDURE(op1_ri), DEFERRED, PASS(this) :: add_integer_post
     PROCEDURE(op1_ri), DEFERRED, PASS(this) :: subtract_integer_post
     PROCEDURE(op2_ri), DEFERRED, PASS(this) :: add_integer_pre
     PROCEDURE(op3_ri), DEFERRED, PASS(this) :: subtract_iterator
     PROCEDURE(op4_ri), DEFERRED, PASS(this) :: lt
     PROCEDURE(op4_ri), DEFERRED, PASS(this) :: gt
     PROCEDURE(op4_ri), DEFERRED, PASS(this) :: gte
     PROCEDURE(op4_ri), DEFERRED, PASS(this) :: lte

     GENERIC, PUBLIC :: OPERATOR(+)  => add_integer_post, add_integer_pre
     GENERIC, PUBLIC :: OPERATOR(-)  => subtract_integer_post, subtract_iterator
     GENERIC, PUBLIC :: OPERATOR(<)  => lt
     GENERIC, PUBLIC :: OPERATOR(<=) => lte
     GENERIC, PUBLIC :: OPERATOR(>)  => gt
     GENERIC, PUBLIC :: OPERATOR(>=) => gte
  END TYPE random_access_iterator


  ABSTRACT INTERFACE

     FUNCTION op1_fi(this, that) RESULT(b)
       IMPORT :: forward_iterator
       CLASS(forward_iterator), INTENT(in) :: this, that
       LOGICAL                             :: b
     END FUNCTION op1_fi

     SUBROUTINE op2_fi(this)
       IMPORT :: forward_iterator
       CLASS(forward_iterator), INTENT(inout)  :: this
     END SUBROUTINE op2_fi
     
     SUBROUTINE op1_bi(this)
       IMPORT :: bidirectional_iterator
       CLASS(bidirectional_iterator), INTENT(inout)  :: this
     END SUBROUTINE op1_bi

     FUNCTION op1_ri(this, n) RESULT(itr)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(in)  :: this
       INTEGER                      , INTENT(in)  :: n
       CLASS(random_access_iterator), ALLOCATABLE :: itr
     END FUNCTION op1_ri

     FUNCTION op2_ri(n, this) RESULT(itr)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(in)  :: this
       INTEGER                      , INTENT(in)  :: n
       CLASS(random_access_iterator), ALLOCATABLE :: itr
     END FUNCTION op2_ri
     
     FUNCTION op3_ri(this, that) RESULT(itr)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(in)  :: this, that
       CLASS(random_access_iterator), ALLOCATABLE :: itr
     END FUNCTION op3_ri

     FUNCTION op4_ri(this, that) RESULT(b)
       IMPORT :: random_access_iterator
       CLASS(random_access_iterator), INTENT(in) :: this, that
       LOGICAL                                   :: b
     END FUNCTION op4_ri
  END INTERFACE
  
END MODULE gfcl_iterators
