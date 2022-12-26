program test_memory_block
  use iso_fortran_env, only: stderr => error_unit
  use pool_module
  implicit none

  type(memory_block_t), pointer :: block_ptr, block_2_ptr
  integer :: expected_length
  integer, allocatable :: expected_ids

  logical :: allpass

  allpass = .true.

  call init_memory_pool(4, 16)

  expected_length = 4
  if (size(get_block_ids()) /= 4) then
     allpass = .false.
     write(stderr, '(a)') 'Free list has the correct length... failed'
  else
     write(stderr, '(a)') 'Free list has the correct length... passed'
  end if

  block_ptr => get_memory_block()

  if (.not. all((get_block_ids() == [3, 2, 1]))) then
     allpass = .false.
     write(stderr, '(a)') 'Head block is chopped off upon allocation ... failed'
  else
     write(stderr, '(a)') 'Head block is chopped off upon allocation ... passed'
  end if

  if(block_ptr%id /= 4) then
     allpass = .false.
     write(stderr, '(a)') 'Block pointer points to correct block ... failed'
  else
     write(stderr, '(a)') 'Block pointer points to correct block ... passed'
  end if

  block_2_ptr => get_memory_block()
  call release(block_ptr)

  if (.not. all((get_block_ids() == [4, 2, 1]))) then
     allpass = .false.
     write(stderr, '(a)') 'Block is correctly released ... failed'
  else
     write(stderr, '(a)') 'Block is correctly released ... passed'
  end if

  call release(block_2_ptr)
  call finalise_memory_pool()

  call init_memory_pool(2, 16)

  block_ptr => get_memory_block()
  block_2_ptr => get_memory_block()

  if (.not. all((get_block_ids() == [4, 3]))) then
     allpass = .false.
     write(stderr, '(a)') 'Memory pool is correctly extended ... failed'
  else
     write(stderr, '(a)') 'Memory pool is correctly extended ... passed'
  end if

end program test_memory_block
