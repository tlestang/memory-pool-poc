program main_pool
  use pool_module

  implicit none

  type(memory_block_t), pointer :: memblock

  call init_memory_pool(3, 16)
  call print_freelist()
  write(*,*) '--------'
  memblock => get_memory_block()
  call print_freelist()
  write(*,*) '--------'
  call release_memory_block(memblock)
  call print_freelist()
  write(*,*) '--------'
  call finalise_memory_pool()

end program main_pool
