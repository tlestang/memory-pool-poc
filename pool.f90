module pool_module
  implicit none

  private
  public :: get_memory_block, release_memory_block

  real, target :: memblock(256)

contains

  function get_memory_block() result(handle)
    real, pointer :: handle(:)
    handle => memblock
  end function get_memory_block

  subroutine release_memory_block(handle)
    real, pointer :: handle(:)
    nullify(handle)
  end subroutine release_memory_block
end module pool_module
