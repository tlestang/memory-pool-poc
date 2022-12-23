program test_field
  use iso_fortran_env, only: stderr => error_unit
  use field_module
  use pool_module, only: init_memory_pool, finalise_memory_pool, get_block_ids
  implicit none

  type(field_t) :: u, v, w
  type(field_t), allocatable :: x
  integer :: current_list_len

  logical :: allpass
  real, parameter :: tol = 0.001

  allpass = .true.

  call init_memory_pool(5, 16)

  u = field_t(1.)

  if (u%data%refcount /= 1) then
     allpass = .false.
     write(stderr, '(a)') 'Freshly allocated block has refcount of 1 ... failed'
  else
     write(stderr, '(a)') 'Freshly allocated block has refcount of 1 ... passed'
  end if

  if (.not. all(abs(u%data%segment - 1.) < tol)) then
     allpass = .false.
     write(stderr, '(a)') 'Field instance constructed with correct values ... failed'
  else
     write(stderr, '(a)') 'Field instance constructed with correct values ... passed'
  end if

  v = field_t(2.)
  u = v

  if (u%data%refcount /= 2 .or. v%data%refcount /= 2) then
     allpass = .false.
     write(stderr, '(a)') 'Assignment correctly increases refcount ... failed'
  else
     write(stderr, '(a)') 'Assignment correctly increases refcount ... passed'
  end if

  ! Now modify u and check that v is also modified
  u%data%segment = 3.
  if (.not. all(abs(v%data%segment - 3.) < tol)) then
     allpass = .false.
     write(stderr, '(a)') 'Both field instances point to the same block ... failed'
  else
     write(stderr, '(a)') 'Both field instances point to the same block ... passed'
  end if

  v = field_t(2.)
  w = u + v
  if (u%data%refcount /= 1 .or. v%data%refcount /= 1 .or. w%data%refcount /= 1) then
     allpass = .false.
     write(stderr, '(a)') 'All + operands have refcoutn of 1 ... failed'
     write(*,*) w%data%refcount
  else
     write(stderr, '(a)') 'All + operands have refcoutn of 1 ... passed'
  end if

  if (u%data%id == v%data%id &
       & .or. u%data%id == w%data%id &
       & .or. v%data%id == w%data%id) then
     allpass = .false.
     write(stderr, '(a)') 'All + operands point to three different blocks ... failed'
  else
     write(stderr, '(a)') 'All + operands point to three different blocks ... passed'
  end if

  allocate(x)
  x = field_t(0.)
  current_list_len = size(get_block_ids())
  deallocate(x)

  if (.not. (size(get_block_ids()) == current_list_len + 1)) then
     allpass = .false.
     write(stderr, '(a)') 'Deallocating field_t correctly releases block ... failed'
  else
     write(stderr, '(a)') 'Deallocating field_t correctly releases block ... passed'
  end if

  call finalise_memory_pool()
end program test_field
