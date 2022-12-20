program main
  use field
  use pool_module

  type(field_t) :: u, v

  call init_memory_pool(3, 16)
  call print_freelist()
  write(*,*) 'Init u field'
  u = field_t()
  write(*,*) 'U: ', u%data%id, u%data%refcount
  call print_freelist()
  write(*,*) 'Init v field'
  v = field_t()
  write(*,*) 'V: ', v%data%id, v%data%refcount
  call print_freelist()
  write(*,*) 'Assiginig v field'
  v = u
  call print_freelist()
  write(*,*) 'v%data%id = ', v%data%id
  write(*,*) 'u%data%id = ', u%data%id

  call finalise_memory_pool()

end program main
