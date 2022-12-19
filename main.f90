program main
  use field

  type(field_t) :: u, v
  u = field_t()
  call random_number(u%data)
  v = u

  write(*, *) sum(u%data)
  write(*, *) sum(v%data)

end program main
