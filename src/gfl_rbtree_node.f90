MODULE gfcl_rbtree_node
  !--- USE statements ------------------------------------------------
  !--- Implicit statement --------------------------------------------
  IMPLICIT NONE
  !--- Private/public section ----------------------------------------
  PRIVATE
  !--- Data types ----------------------------------------------------
  ! An actual rbtree node, containing the rbtree element, and pointers to
  ! the previous and next node in the rbtree.
  TYPE, PUBLIC :: RbTreeNode
     !--- Component part
     LOGICAL                        :: l_color_
     CLASS(*)         , ALLOCATABLE :: ta_data_
     CLASS(RbTreeNode), POINTER     :: tp_parent_ => NULL()
     CLASS(RbTreeNode), POINTER     :: tp_left_   => NULL()
     CLASS(RbTreeNode), POINTER     :: tp_right_  => NULL()

   CONTAINS
     !--- Type-bound-procedure part
     PROCEDURE :: hook     => hook__
     PROCEDURE :: unhook   => unhook__
     PROCEDURE :: swap     => swap__
     PROCEDURE :: transfer => transfer__
     PROCEDURE :: reverse  => reverse__

     PROCEDURE :: next => next__
     PROCEDURE :: prev => prev__
     PROCEDURE :: get  => get__
     PROCEDURE :: set  => set__

     FINAL :: final___
  END TYPE RbTreeNode
  !--- Interfaces ----------------------------------------------------               
CONTAINS

  FUNCTION minimum__ (t_node) RESULT(tp)
    ! --- Declaration of arguments -------------------------------------
    CLASS(RbTreeNode), INTENT(in) :: t_node
    CLASS(RbTreeNode), POINTER    :: tp
    ! --- Executable Code ----------------------------------------------
    tp => t_node
    DO WHILE (ASSOCIATED(tp%tp_left_))
       tp => tp%tp_left_
    END DO
  END FUNCTION minimum__

  FUNCTION maximum__ (t_node) RESULT(tp_x)
    ! --- Declaration of arguments -------------------------------------
    CLASS(RbTreeNode), INTENT(in) :: t_node
    CLASS(RbTreeNode), POINTER    :: tp_x
    ! --- Executable Code ----------------------------------------------
    tp_x => t_node
    DO WHILE (ASSOCIATED(tp_x%tp_right_))
       tp_x => tp_x%tp_right_
    END DO
  END FUNCTION maximum__

  SUBROUTINE increment__(t_node) RESULT(tp_x)
    ! --- Declaration of arguments -------------------------------------
    CLASS(RbTreeNode), INTENT(in) :: t_node
    CLASS(RbTreeNode), POINTER    :: tp_x
    ! --- Declaration of variables -------------------------------------
    CLASS(RbTreeNode), POINTER    :: tp_y
    ! --- Executable Code ----------------------------------------------
    tp_x => t_node
    IF (ASSOCIATED(tp_x%tp_right_)) THEN
       tp_x => tp_x%tp_right_
       DO WHILE (ASSOCIATED(tp_x%tp_left_)) THEN
          tp_x => tp_x%tp_left_
       END DO
    ELSE
       tp_y => tp_x%tp_parent_
       DO WHILE (ASSOCIATED(tp_x,tp_y%tp_right_))
          tp_x => tp_y
          tp_y => tp_y%tp_parent_
       END DO
       IF ( ASSOCIATED(tp_x%tp_right_,tp_y) ) THEN
          tp_x => tp_y
       END IF
    END IF
  END SUBROUTINE increment__

  SUBROUTINE decrement__(t_node) RESULT(tp_x)
    ! --- Declaration of arguments -------------------------------------
    CLASS(RbTreeNode), INTENT(in) :: t_node
    CLASS(RbTreeNode), POINTER    :: tp_x
    ! --- Declaration of variables -------------------------------------
    CLASS(RbTreeNode), POINTER    :: tp_y
    ! --- Executable Code ----------------------------------------------
    tp_x => t_node
    IF ( tp_x%color_ .EQV. l_red &
         .AND. ASSOCIATED(tp_x%tp_parent_%tp_parent_,tp_x) ) THEN
       tp_x => tp_x%tp_right_
    ELSE IF ( associated(tp_x%tp_left_) THEN
       tp_y => tp_x%tp_left_
       DO WHILE ( ASSOCIATED(tp_y%tp_right_) )
          tp_y => tp_y%tp_right_
       END DO
       tp_x => tp_y
    ELSE
       tp_y => tp_x%tp_parent_
       DO WHILE (ASSOCIATED(tp_x,tp_y%tp_left_))
          tp_x => tp_y
          tp_y => tp_y%tp_parent_
       END DO
       tp_x => tp_y
    END IF
  END SUBROUTINE decrement__

  SUBROUTINE final___(t_node)
    ! --- Declaration of arguments -------------------------------------
    TYPE(RbTreeNode), INTENT(inout) :: t_node
    ! --- Executable Code ----------------------------------------------
    IF (ALLOCATED(t_node%ta_data_)) DEALLOCATE(t_node%ta_data_)
  END SUBROUTINE final___

  
END MODULE gfcl_rbtree_node
