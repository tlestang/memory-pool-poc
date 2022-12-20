module pool_module
  implicit none

  type :: memory_block_t
     integer :: size
     real, allocatable :: segment(:)
     integer :: refcount
     type(memory_block_t), pointer :: next
     integer :: id
   contains
     final :: deallocate_memory_block_segment
  end type memory_block_t

  interface memory_block_t
     module procedure memory_block_constructor
  end interface memory_block_t

  type(memory_block_t), pointer :: first

contains

  function memory_block_constructor(size, next, id) result(m)
    integer, intent(in) :: size, id
    type(memory_block_t), pointer, intent(in) :: next
    type(memory_block_t) :: m

    m%size = size
    allocate(m%segment(size))
    m%refcount = 0
    m%next => next
    m%id = id
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
    handle%refcount = handle%refcount + 1
  end function get_memory_block

  subroutine unbind_or_release(handle)
    type(memory_block_t), pointer :: handle
    ! TODO CLEANUP ASSERT
    write(*,*) 'Unbinding mem block with refcount', handle%refcount
    if (handle%refcount == 0) then
       stop 'Refcount cannot be zero here'
    end if
    handle%refcount = handle%refcount - 1
    if (handle%refcount == 0) then
       write(*,*) '    Releasing memory block to pool'
       ! Reattach memory block in front of free list
       handle%next => first
       first => handle
    end if
  end subroutine unbind_or_release

  subroutine init_memory_pool(nblocks, size)
    !! Constructs a linked list of memory_block_t instances.
    !! Retuns a pointer to the first item in the list
    type(memory_block_t), pointer :: current
    integer :: i, size, nblocks

    nullify(first)
    do i = 1, nblocks
       allocate(current)
       current = memory_block_t(size, first, id=i)
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
       write(*,*) current%id, allocated(current%segment)
       current => current%next
    end do

  end subroutine print_freelist
end module pool_module
