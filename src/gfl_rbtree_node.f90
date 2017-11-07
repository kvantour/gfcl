MODULE gfcl_rbtree_node
  !--- USE statements ------------------------------------------------
  !--- Implicit statement --------------------------------------------
  IMPLICIT NONE
  !--- Private/public section ----------------------------------------
  PRIVATE
  !--- Parameters ----------------------------------------------------
  LOGICAL, PARAMETER :: l_red(.TRUE.)
  LOGICAL, PARAMETER :: l_black(.FALSE.)
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

  SUBROUTINE rotate_left__(tp_node, tp_root)
    ! --- Declaration of arguments -------------------------------------
    CLASS(RbTreeNode), POINTER, INTENT(in)    :: tp_node
    CLASS(RbTreeNode), POINTER, INTENT(inout) :: tp_root
    ! --- Declaration of variables -------------------------------------
    CLASS(RbTreeNode), POINTER                :: y(tp_node%tp_right_)
    ! --- Executable Code ----------------------------------------------
    tp_node%tp_right_ => y%tp_left_;
    IF ASSOCIATED(y%tp_left_) y%tp_left_%tp_parent_ => tp_node;
    y%tp_parent_ => tp_node%tp_parent_;
    IF ASSOCIATED(tp_node,tp_root) THEN
       tp_root => y;
    ELSE IF ASSOCIATED(tp_node,tp_node%tp_parent_%tp_left_) THEN
       tp_node%tp_parent_%tp_left_ => y;
    ELSE
       tp_node%tp_parent_%tp_right_ => y;
    END IF
    y%tp_left_ => tp_node;
    tp_node%tp_parent_ => y;
  END SUBROUTINE rotate_left__

  SUBROUTINE rotate_right__(tp_node, tp_root)
    ! --- Declaration of arguments -------------------------------------
    CLASS(RbTreeNode), POINTER, INTENT(in)    :: tp_node
    CLASS(RbTreeNode), POINTER, INTENT(inout) :: tp_root
    ! --- Declaration of variables -------------------------------------
    CLASS(RbTreeNode), POINTER                :: y(t_node%tp_left_)
    ! --- Executable Code ----------------------------------------------
    tp_node%tp_left_ = y%tp_right_;
    IF ASSOCIATED(y%tp_right_) y%tp_right_%tp_parent_ => tp_node;
    y%tp_parent_ => tp_node%tp_parent_;
    IF ASSOCIATED(tp_node,tp_root) THEN
       tp_root => y;
    ELSE IF ASSOCIATED(tp_node,tp_node%tp_parent_%tp_right_)
       tp_node%tp_parent_%tp_right_ => y;
    ELSE
       tp_node%tp_parent_%tp_left_ => y;
    END IF
    y%tp_right_ => tp_node;
    tp_node%tp_parent_ => y;
  END SUBROUTINE rotate_right__

  void
  SUBROUTINE insert_and_rebalance__(l_insert_left, tp_x, tp_p, t_header)
    ! --- Declaration of arguments -------------------------------------
    LOGICAL, INTENT(in) :: l_insert_left
    CLASS(RbTreeNode), POINTER, INTENT(in)    :: tp_x
    CLASS(RbTreeNode), POINTER, INTENT(in)    :: tp_p
    CLASS(RbTreeNode), TARGET , INTENT(inout) :: t_header
    ! --- Declaration of variables -------------------------------------
    CLASS(RbTreeNode), POINTER :: tp_root, tp_xpp, tp_y
    ! --- Executable Code ----------------------------------------------
    tp_root => t_header%tp_parent_ ;
    
    ! Initialize fields in new node to insert.
    tp_x%tp_parent_ => tp_p;
    NULLIFY(tp_x%tp_left_)
    NULLIFY(tp_x%tp_right_)
    tp_x%tp_color_ = l_red;
    
    ! Insert.
    ! Make new node child of parent and maintain root, leftmost and
    ! rightmost nodes.
    ! N.B. First node is always inserted left.
    IF (l_insert_left) THEN
       ! also makes leftmost => tp_x when tp_p == &t_header
       tp_p%tp_left_ => tp_x;
       
       IF ASSOCIATED(tp_p,t_header) THEN
          t_header%tp_parent_ => tp_x;
          t_header%tp_right_ => tp_x;
       ELSE IF ASSOCIATED(tp_p,t_header%tp_left_) THEN
          ! maintain leftmost pointing to min node
          t_header%tp_left_ => tp_x;
       END IF
       
    ELSE
       tp_p%tp_right_ => tp_x;
       
       IF ASSOCIATED(tp_p,t_header%tp_right_) THEN
          ! maintain rightmost pointing to max node
          t_header%tp_right_ => tp_x;
       END IF
    END IF
    ! Rebalance.
    DO WHILE (.NOT.ASSOCIATED(tp_x,tp_root) &
         .AND. tp_x%tp_parent_%tp_color_ == l_red)

       tp_xpp => tp_x%tp_parent_%tp_parent_;

       IF ASSOCIATED(tp_x%tp_parent_,tp_xpp%tp_left_) THEN
          
          tp_y => tp_xpp%tp_right_;
          IF (tp_y && tp_y%tp_color_ == l_red) THEN
             
             tp_x%tp_parent_%tp_color_ = l_black;
             tp_y%tp_color_ = l_black;
             tp_xpp%tp_color_ = l_red;
             tp_x => tp_xpp;
             
          ELSE
             
             IF ASSOCIATED(tp_x,tp_x%tp_parent_%tp_right_) THEN
                tp_x => tp_x%tp_parent_;
                CALL rotate_left__(tp_x, tp_root);
             END IF
             
             tp_x%tp_parent_%tp_color_ = l_black;
             tp_xpp%tp_color_ = l_red;
             CALL rotate_right__(tp_xpp, tp_root);
          END IF
       ELSE
          tp_y => tp_xpp%tp_left_;
          IF (ASSOCIATED(tp_y) .AND. tp_y%tp_color_ == l_red) THEN
             tp_x%tp_parent_%tp_color_ = l_black;
             tp_y%tp_color_ = l_black;
             tp_xpp%tp_color_ = l_red;
             tp_x => tp_xpp;
          ELSE
             IF ASSOCIATED(tp_x,tp_x%tp_parent_%tp_left_) THEN
                tp_x => tp_x%tp_parent_;
                CALL rotate_right__(tp_x, tp_root);
             END IF
             tp_x%tp_parent_%tp_color_ = l_black;
             tp_xpp%tp_color_ = l_red;
             CALL rotate_left__(tp_xpp, tp_root);
          END IF
       END IF
    END DO
    tp_root%tp_color_ = l_black;
  END SUBROUTINE insert_and_rebalance__

  _Rb_tree_node_base*
  _Rb_tree_rebalance_for_erase(_Rb_tree_node_base* const __z,
			       _Rb_tree_node_base& t_header) throw ()
  {
    _Rb_tree_node_base *& tp_root = t_header%tp_parent_ ;
    _Rb_tree_node_base *& __leftmost = t_header%tp_left_ ;
    _Rb_tree_node_base *& __rightmost = t_header%tp_right_ ;
    _Rb_tree_node_base* __y = __z;
    _Rb_tree_node_base* __x = 0;
    _Rb_tree_node_base* __x_parent = 0;

    if (__y%tp_left_ == 0)     ! __z has at most one non-null child. y == z.
      __x = __y%tp_right_;     ! __x might be null.
    else
      if (__y%tp_right_ == 0)  ! __z has exactly one non-null child. y == z.
	__x = __y%tp_left_;    ! __x is not null.
      else
	{
	  ! __z has two non-null children.  Set __y to
	  __y = __y%tp_right_;   !   __z's successor.  __x might be null.
	  while (__y%tp_left_ != 0)
	    __y = __y%tp_left_;
	  __x = __y%tp_right_;
	}
    if (__y != __z)
      {
	! relink y in place of z.  y is z's successor
	__z%tp_left_%tp_parent_ => __y;
	__y%tp_left_ => __z%tp_left_;
	if (__y != __z%tp_right_)
	  {
	    __x_parent = __y%tp_parent_;
	    if (__x) __x%tp_parent_ => __y%tp_parent_;
	    __y%tp_parent_%tp_left_ => __x;   ! __y must be a child of%tp_left_ 
	    __y%tp_right_ => __z%tp_right_;
	    __z%tp_right_%tp_parent_ => __y;
	  }
	else
	  __x_parent = __y;
	if ASSOCIATED(tp_root,__z)
	  tp_root = __y;
	else if ASSOCIATED(__z%tp_parent_%tp_left_,__z)
	  __z%tp_parent_%tp_left_ => __y;
	else
	  __z%tp_parent_%tp_right_ => __y;
	__y%tp_parent_ => __z%tp_parent_;
	std::swap(__y%tp_color_, __z%tp_color_);
	__y = __z;
	! __y now points to node to be actually deleted
      }
    else
      {                        ! __y == __z
	__x_parent = __y%tp_parent_;
	if (__x)
	  __x%tp_parent_ => __y%tp_parent_;
	if ASSOCIATED(tp_root,__z)
	  tp_root = __x;
	else
	  if ASSOCIATED(__z%tp_parent_%tp_left_,__z)
	    __z%tp_parent_%tp_left_ => __x;
	  else
	    __z%tp_parent_%tp_right_ => __x;
	if ASSOCIATED(__leftmost,__z)
	  {
	    if (__z%tp_right_ == 0)        ! __z%tp_left_ must be null also
	      __leftmost = __z%tp_parent_;
	    ! makes __leftmost == _M_header if __z == tp_root
	    else
	      __leftmost = _Rb_tree_node_base::_S_minimum(__x);
	  }
	if ASSOCIATED(__rightmost,__z)
	  {
	    if (__z%tp_left_ == 0)         ! __z%tp_right_ must be null also
	      __rightmost = __z%tp_parent_;
	    ! makes __rightmost == _M_header if __z == tp_root
	    else                      ! __x == __z%tp_left_
	      __rightmost = _Rb_tree_node_base::_S_maximum(__x);
	  }
      }
    if (__y%tp_color_ != l_red)
      {
	while (__x != tp_root && (__x == 0 || __x%tp_color_ == l_black))
	  if ASSOCIATED(__x,__x_parent%tp_left_)
	    {
	      _Rb_tree_node_base* __w = __x_parent%tp_right_;
	      if ASSOCIATED(__w%tp_color_,l_red)
		{
		  __w%tp_color_ = l_black;
		  __x_parent%tp_color_ = l_red;
		  local_Rb_tree_rotate_left(__x_parent, tp_root);
		  __w = __x_parent%tp_right_;
		}
	      if ((__w%tp_left_ == 0 ||
		   __w%tp_left_%tp_color_ == l_black) &&
		  (__w%tp_right_ == 0 ||
		   __w%tp_right_%tp_color_ == l_black))
		{
		  __w%tp_color_ = l_red;
		  __x = __x_parent;
		  __x_parent = __x_parent%tp_parent_;
		}
	      else
		{
		  if (__w%tp_right_ == 0
		      || __w%tp_right_%tp_color_ == l_black)
		    {
		      __w%tp_left_%tp_color_ = l_black;
		      __w%tp_color_ = l_red;
		      local_Rb_tree_rotate_right(__w, tp_root);
		      __w = __x_parent%tp_right_;
		    }
		  __w%tp_color_ = __x_parent%tp_color_;
		  __x_parent%tp_color_ = l_black;
		  if (__w%tp_right_)
		    __w%tp_right_%tp_color_ = l_black;
		  local_Rb_tree_rotate_left(__x_parent, tp_root);
		  break;
		}
	    }
	  else
	    {
	      ! same as above, with%tp_right_  <->%tp_left_ .
	      _Rb_tree_node_base* __w = __x_parent%tp_left_;
	      if ASSOCIATED(__w%tp_color_,l_red)
		{
		  __w%tp_color_ = l_black;
		  __x_parent%tp_color_ = l_red;
		  local_Rb_tree_rotate_right(__x_parent, tp_root);
		  __w = __x_parent%tp_left_;
		}
	      if ((__w%tp_right_ == 0 ||
		   __w%tp_right_%tp_color_ == l_black) &&
		  (__w%tp_left_ == 0 ||
		   __w%tp_left_%tp_color_ == l_black))
		{
		  __w%tp_color_ = l_red;
		  __x = __x_parent;
		  __x_parent = __x_parent%tp_parent_;
		}
	      else
		{
		  if (__w%tp_left_ == 0 || __w%tp_left_%tp_color_ == l_black)
		    {
		      __w%tp_right_%tp_color_ = l_black;
		      __w%tp_color_ = l_red;
		      local_Rb_tree_rotate_left(__w, tp_root);
		      __w = __x_parent%tp_left_;
		    }
		  __w%tp_color_ = __x_parent%tp_color_;
		  __x_parent%tp_color_ = l_black;
		  if (__w%tp_left_)
		    __w%tp_left_%tp_color_ = l_black;
		  local_Rb_tree_rotate_right(__x_parent, tp_root);
		  break;
		}
	    }
	if (__x) __x%tp_color_ = l_black;
      }
    return __y;
  }

  FUNCTION count__(tp_node, tp_root) RESULT(sum)
    ! --- Declaration of arguments -------------------------------------
    TYPE(RbTreeNode), POINTER, INTENT(in) :: tp_node
    TYPE(RbTreeNode), POINTER, INTENT(in) :: tp_root
    INTEGER                               :: sum
    ! --- Declaration of variables -------------------------------------
    TYPE(RbTreeNode), POINTER             :: tp
    ! --- Executable Code ----------------------------------------------
    sum = 0
    IF ASSOCIATED(tp_node) RETURN
    tp => tp_node
    DO 
       IF (tp%tp_color_ == l_black) sum = sum + 1
       IF ASSOCIATED(tp,tp_root) EXIT
       tp => tp%tp_parent_;
    END DO
  END FUNCTION count__
  
  SUBROUTINE final___(t_node)
    ! --- Declaration of arguments -------------------------------------
    TYPE(RbTreeNode), INTENT(inout) :: t_node
    ! --- Executable Code ----------------------------------------------
    IF (ALLOCATED(t_node%ta_data_)) DEALLOCATE(t_node%ta_data_)
  END SUBROUTINE final___

  
END MODULE gfcl_rbtree_node
