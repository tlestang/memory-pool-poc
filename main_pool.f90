program main_pool
  use pool_module

  implicit none

  type(memory_block_t), pointer :: first

  first => init_memory_pool()
  call print_freelist(first)

end program main_pool
