! For use with Serialbox only
module savepoint_helpers
    implicit none
    public ser_set_i,ser_set_j,ser_set_k
    public ser_set_nx,ser_set_ny,ser_set_nz
    integer :: ser_i,ser_j,ser_k
    integer :: ser_nx,ser_ny,ser_nz
    contains
    subroutine ser_set_i(in_i)
      integer, intent(in):: in_i
      ser_i = in_i
    end subroutine ser_set_i
    subroutine ser_set_j(in_j)
      integer, intent(in):: in_j
      ser_j = in_j
    end subroutine ser_set_j
    subroutine ser_set_k(in_k)
      integer, intent(in):: in_k
      ser_k = in_k
    end subroutine ser_set_k
    
    subroutine ser_set_nx(in_nx)
      integer, intent(in):: in_nx
      ser_nx = in_nx
    end subroutine ser_set_nx
    subroutine ser_set_ny(in_ny)
      integer, intent(in):: in_ny
      ser_ny = in_ny
    end subroutine ser_set_ny
    subroutine ser_set_nz(in_nz)
      integer, intent(in):: in_nz
      ser_nz = in_nz
    end subroutine ser_set_nz
      
end module savepoint_helpers
  