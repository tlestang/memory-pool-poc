program main
  use field
  use pool_module

  type(field_t) :: u, v, w

  call init_memory_pool(5, 16)
  call print_freelist()
  write(*,*) 'Init u field'
  u = field_t(1.)
  write(*,*) 'U: ', u%data%id, u%data%refcount
  call print_freelist()
  write(*,*) 'Init v field'
  v = field_t(3.)
  write(*,*) 'V: ', v%data%id, v%data%refcount
  call print_freelist()
  write(*,*) 'Init w field'
  w = field_t()
  write(*,*) 'W: ', w%data%id, w%data%refcount
  call print_freelist()
  write(*,*) 'Assiginig w field'
  w = u + v
  call print_freelist()
  write(*,*) 'v%data%id = ', v%data%id
  write(*,*) 'u%data%id = ', u%data%id
  write(*,*) 'w%data%id = ', w%data%id

  write(*,*) 'w%data%segment = ', w%data%segment

  call finalise_memory_pool()

end program main
