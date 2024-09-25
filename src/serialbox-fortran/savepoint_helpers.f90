! For use with Serialbox only
module savepoint_helpers
    implicit none
    public ser_set_idx_d1,ser_set_idx_d2,ser_set_idx_d3,ser_set_idx_d4
    public ser_set_D1,ser_set_D2,ser_set_D3,ser_set_D4
    integer :: ser_idx_d1 = 1
    integer :: ser_idx_d2 = 1
    integer :: ser_idx_d3 = 1
    integer :: ser_idx_d4 = 1
    integer :: ser_D1 = 1
    integer :: ser_D2 = 1
    integer :: ser_D3 = 1
    integer :: ser_D4 = 1
    contains
    subroutine ser_set_indices(idx_d1, idx_d2, idx_d3, idx_d4)
      integer, intent(in):: idx_d1, idx_d2, idx_d3, idx_d4
      ser_idx_d1 = idx_d1
      ser_idx_d2 = idx_d2
      ser_idx_d3 = idx_d3
      ser_idx_d4 = idx_d4
    end subroutine
    subroutine ser_set_idx_d1(in_idx_d1)
      integer, intent(in):: in_idx_d1
      ser_idx_d1 = in_idx_d1
    end subroutine ser_set_idx_d1
    subroutine ser_set_idx_d2(in_idx_d2)
      integer, intent(in):: in_idx_d2
      ser_idx_d2 = in_idx_d2
    end subroutine ser_set_idx_d2
    subroutine ser_set_idx_d3(in_idx_d3)
      integer, intent(in):: in_idx_d3
      ser_idx_d3 = in_idx_d3
    end subroutine ser_set_idx_d3
    subroutine ser_set_idx_d4(in_idx_d4)
      integer, intent(in):: in_idx_d4
      ser_idx_d4 = in_idx_d4
    end subroutine ser_set_idx_d4
    
    subroutine ser_set_Dx(d1, d2, d3, d4)
      integer, intent(in):: d1, d2, d3, d4
      ser_D1 = d1
      ser_D2 = d2
      ser_D3 = d3
      ser_D4 = d4
    end subroutine
    subroutine ser_set_D1(in_D1)
      integer, intent(in):: in_D1
      ser_D1 = in_D1
    end subroutine ser_set_D1
    subroutine ser_set_D2(in_D2)
      integer, intent(in):: in_D2
      ser_D2 = in_D2
    end subroutine ser_set_D2
    subroutine ser_set_D3(in_D3)
      integer, intent(in):: in_D3
      ser_D3 = in_D3
    end subroutine ser_set_D3
    subroutine ser_set_D4(in_D4)
      integer, intent(in):: in_D4
      ser_D4 = in_D4
    end subroutine ser_set_D4
      
end module savepoint_helpers
  