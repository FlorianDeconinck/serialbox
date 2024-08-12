! For use with Serialbox only
module ijk_checkpoint
    implicit none
    public set_i,set_j,set_k,get_i,get_j,get_k
    public set_nx,set_ny,set_nz,get_nx,get_ny,get_nz
    integer :: i,j,k,nx,ny,nz
    contains
    subroutine set_i(in_i)
      integer, intent(in):: in_i
      i = in_i
    end subroutine set_i
    subroutine set_j(in_j)
      integer, intent(in):: in_j
      j = in_j
    end subroutine set_j
    subroutine set_k(in_k)
      integer, intent(in):: in_k
      k = in_k
    end subroutine set_k
   
    subroutine get_i(out_i)
      integer, intent(out) ::out_i
      out_i = i
    end subroutine get_i
    subroutine get_j(out_j)
      integer, intent(out) ::out_j
      out_j = j
    end subroutine get_j
    subroutine get_k(out_k)
      integer, intent(out) ::out_k
      out_k = k
    end subroutine get_k
    
    subroutine set_nx(in_nx)
      integer, intent(in):: in_nx
      nx = in_nx
    end subroutine set_nx
    subroutine set_ny(in_ny)
      integer, intent(in):: in_ny
      ny = in_ny
    end subroutine set_ny
    subroutine set_nz(in_nz)
      integer, intent(in):: in_nz
      nz = in_nz
    end subroutine set_nz
  
    subroutine get_nx(out_nx)
      integer, intent(out) ::out_nx
      out_nx = nx
    end subroutine get_nx
    subroutine get_ny(out_ny)
      integer, intent(out) ::out_ny
      out_ny = ny
    end subroutine get_ny
    subroutine get_nz(out_nz)
      integer, intent(out) ::out_nz
      out_nz = nz
    end subroutine get_nz
    
end module ijk_checkpoint
  