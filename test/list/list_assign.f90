PROGRAM list_assign
  !--- USE statements ------------------------------------------------
  USE gfcl_list
  USE gfcl_list_iterator
  !--- Implicit statement --------------------------------------------
  IMPLICIT NONE
  !--- Declaration of variables  -------------------------------------
  TYPE(List) :: nums1, nums2, nums3
  !--- Initialise variables ------------------------------------------
  CALL nums1%initialise( [ 3, 1, 4, 6, 5, 9 ] )
  CALL nums2%initialise()
  CALL nums3%initialise()
  !--- Executable code -----------------------------------------------
  PRINT *, "Initially:"
  CALL display_sizes(nums1,nums2,nums3)
  nums2 = nums1
  PRINT *, "After assigment:" 
  CALL display_sizes(nums1,nums2,nums3)

  
  CALL swap(nums1,nums3)
  PRINT *, "After Swap:" 
  CALL display_sizes(nums1,nums2,nums3)

CONTAINS
  SUBROUTINE display_sizes(nums1,nums2,nums3)
    !--- Declaration of variables  -------------------------------------
    TYPE(List), INTENT(in) :: nums1,nums2,nums3
    !--- Executable code -----------------------------------------------
    PRINT '("nums1:",I2,1X,"nums2:",I2,1X,"nums3:",I2)', SIZE(nums1),SIZE(nums2),SIZE(nums3)
  END SUBROUTINE display_sizes
END PROGRAM list_assign
