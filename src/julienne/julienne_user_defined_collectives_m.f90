!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
module julienne_user_defined_collectives_m
  !! User-defined collective subroutines.
  implicit none

  interface

    impure elemental module subroutine co_all(boolean)
      !! If any image in a team calls this subroutine, then every image in the 
      !! the same team must call this subroutine.  This subroutine sets the
      !! "boolean" argument .true. if it is true in all participating images
      !! upon entry and .false. otherwise.
      implicit none
      logical, intent(inout) :: boolean
    end subroutine

  end interface


contains

  module procedure co_all
#if HAVE_MULTI_IMAGE_SUPPORT
    call co_reduce(boolean, both)
#endif
  contains
    pure function both(lhs,rhs) result(lhs_and_rhs)
      logical, intent(in) :: lhs,rhs
      logical lhs_and_rhs
      lhs_and_rhs = lhs .and. rhs
    end function
  end procedure

end module julienne_user_defined_collectives_m
