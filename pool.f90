module pool_module
  implicit none

  type :: memory_block_t
     integer :: size
     real, allocatable :: segment(:)
     integer :: refcount
     type(memory_block_t), pointer :: next
   contains
     final :: deallocate_memory_block_segment
  end type memory_block_t

  interface memory_block_t
     module procedure memory_block_constructor
  end interface memory_block_t

  type(memory_block_t), pointer :: first

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

  subroutine deallocate_memory_block_segment(self)
    type(memory_block_t), intent(inout) :: self
    deallocate(self%segment)
  end subroutine deallocate_memory_block_segment

  function get_memory_block() result(handle)
    type(memory_block_t), pointer :: handle
    handle => first
    first => first%next
    handle%next => null()
  end function get_memory_block

  subroutine release_memory_block(handle)
    type(memory_block_t), pointer :: handle, current
    handle%next => first
    first => handle
  end subroutine release_memory_block

  subroutine init_memory_pool(nblocks, size)
    !! Constructs a linked list of memory_block_t instances.
    !! Retuns a pointer to the first item in the list
    type(memory_block_t), pointer :: current
    integer :: i, size, nblocks

    nullify(first)
    do i = 1, nblocks
       allocate(current)
       current = memory_block_t(size, first)
       first => current
    end do
  end subroutine init_memory_pool

  subroutine finalise_memory_pool()
    type(memory_block_t), pointer :: current
    do
       if(.not. associated(first)) exit
       current => first
       first => first%next
       deallocate(current)
    end do
  end subroutine finalise_memory_pool

  subroutine print_freelist()
    type(memory_block_t), pointer :: current

    current => first
    do
       if(.not. associated(current)) exit
       write(*,*) allocated(current%segment)
       write(*,*) size(current%segment)
       current => current%next
    end do

  end subroutine print_freelist
end module pool_module
