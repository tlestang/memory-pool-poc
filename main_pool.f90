program main_pool
  use pool_module

  implicit none

  type(memory_block_t), pointer :: first, memblock

  first => init_memory_pool(3, 16)
  call print_freelist(first)
  write(*,*) '--------'
  memblock => get_memory_block(first)
  call print_freelist(first)
  write(*,*) '--------'
  call release_memory_block(first, memblock)
  call print_freelist(first)
  write(*,*) '--------'

end program main_pool
