module pool_module
  implicit none

  type :: memory_block_t
     integer :: size
     real, allocatable :: segment(:)
     integer :: refcount
     type(memory_block_t), pointer :: next
  end type memory_block_t

  interface memory_block_t
     module procedure memory_block_constructor
  end interface memory_block_t

  real, target :: memblock(256)

contains

  function memory_block_constructor(size, next) result(m)
    integer, intent(in) :: size
    type(memory_block_t), pointer, intent(in) :: next
    type(memory_block_t) :: m

    m%size = size
    allocate(m%segment(size))
    m%refcount = 0
    m%next => next
  end function memory_block_constructor

  function get_memory_block() result(handle)
    real, pointer :: handle(:)
    handle => memblock
  end function get_memory_block

  subroutine release_memory_block(handle)
    real, pointer :: handle(:)
    nullify(handle)
  end subroutine release_memory_block

  function init_memory_pool() result(first)
    type(memory_block_t), pointer :: first
    type(memory_block_t), pointer :: current
    integer :: i

    nullify(first)
    do i = 1, 3
       allocate(current)
       current = memory_block_t(16, first)
       first => current
    end do

  end function init_memory_pool

  subroutine print_freelist(ptr)
    type(memory_block_t), pointer, intent(in) :: ptr

    type(memory_block_t), pointer :: current

    current => ptr
    do
       if(.not. associated(current)) exit
       write(*,*) allocated(current%segment)
       write(*,*) size(current%segment)
       current => current%next
    end do

  end subroutine print_freelist
end module pool_module
