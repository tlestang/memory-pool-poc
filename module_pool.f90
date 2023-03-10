module pool_module
  !! This module defines a derived type memory_block_t and a memory
  !! pool as a list of memory_block_t instances.
  !!
  !! A memory_block_t object encapsulate data storage (in the form of
  !! an array of intrinsic type) and metadata.
  !!
  !! The memory pool is a linked list of memory blocks.  Storage for
  !! memory blocks is allocated when the the list is created and
  !! released when it is finalised.  Memory pool is created using
  !!
  !! call init_memory_pool(nblocks, size)
  !!
  !!
  implicit none

  private
  public :: memory_block_t, init_memory_pool, finalise_memory_pool, &
       get_memory_block, release, get_block_ids, print_freelist

  type :: memory_block_t
     integer :: size
     real, allocatable :: segment(:)
     integer :: refcount
     type(memory_block_t), pointer :: next
     integer :: id
   contains
     !> final procedures are called whenever type instance is passed
     !> as intent(out) or deallocated.
     final :: deallocate_memory_block_segment
  end type memory_block_t

  interface memory_block_t
     module procedure memory_block_constructor
  end interface memory_block_t

  type(memory_block_t), pointer :: first => null()
  integer :: pool_list_size = 0
  integer :: pool_blk_size = 0

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
    !> Returns a pointer to the first available memory block, i.e. the
    !> head of the free memory block list.
    !> Example
    !>     f%data => get_memory_block()
    type(memory_block_t), pointer :: handle
    ! If we're about to allocate the last block, extend the pool
    ! first.
    if(.not. associated(first%next)) then
       call extend_pool()
    end if
    handle => first
    first => first%next
    handle%next => null()
  end function get_memory_block

  subroutine extend_pool()
    !> Extend the memory pool twice by twice its size.  Construct a
    !> independant list of memory blocks and attach it to last block
    !> in original pool.
    type(memory_block_t), pointer :: tail, head, current
    integer :: id

    ! Make tail pointer point to last block in the free block list
    tail => first
    do
       if(.not. associated(tail%next)) exit ! It's the last block in the list
       tail => tail%next
    end do

    ! Construct another pool of size pool_list_size
    nullify(head)
    do id = pool_list_size + 1, 2 * pool_list_size
       allocate(current)
       !> Construct a memory_block_t. This effectively allocates
       !> storage space.
       current = memory_block_t(pool_blk_size, head, id=id)
       head => current
    end do
    ! Now attach newly created list to from pointer
    tail%next => head

    pool_list_size = 2 * pool_list_size
  end subroutine extend_pool

  subroutine release(handle)
    !> Release memory block pointed to by handle to the pool.  It is
    !> pushed to the front of the free memory block list.
    type(memory_block_t), pointer :: handle
    handle%next => first
    first => handle
  end subroutine release

  subroutine init_memory_pool(nblocks, size)
    !> Constructs a linked list of memory_block_t instances.  Module
    !> global pointer first always points to the first memory block on
    !> the list.
    type(memory_block_t), pointer :: current
    integer :: id, size, nblocks

    pool_list_size = nblocks
    pool_blk_size = size

    nullify(first)
    do id = 1, pool_list_size
       allocate(current)
       !> Construct a memory_block_t. This effectiveley allocates
       !> storage space.
       current = memory_block_t(pool_blk_size, first, id=id)
       first => current
    end do
  end subroutine init_memory_pool

  subroutine finalise_memory_pool()
    !> Destroy each memory block in the free memory block list, from
    !> head to tail.  Deallocation of a memory_block_t triggers all
    !> final (see def of memory_block_t type) procedures and therefore
    !> deallocation of the block's storage space.
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

  function get_block_ids()
    integer, allocatable :: get_block_ids(:)
    type(memory_block_t), pointer :: current

    current => first
    get_block_ids = [current%id]
    do
       if(.not. associated(current%next)) exit
       get_block_ids = [get_block_ids, current%next%id]
       current => current%next
    end do
  end function get_block_ids

end module pool_module
