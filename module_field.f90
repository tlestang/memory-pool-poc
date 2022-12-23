module field_module
  !> This module provides a field_t type that represents a scalar
  !> field.  A field_t holds a pointer to a memory_block_t instance
  !> that provides the storage required to store the field values.  .

  !> This module also provides defintion for intrinsic operators for
  !> field_t instances, as well as intrindic assignment =.

  !> type(field_t) :: u, v, w, x
  !> u = field_t(0.) ! Now u points to a memory block 1
  !> v = field_t(1.) ! Now v points to memory block 2
  !> w = u + v ! Now w points to memory block 3
  !> v = w ! Now v ponts to memory block 3, and memory block 2 is released to the pool.
  use pool_module

  implicit none

  type field_t
     type(memory_block_t), pointer :: data => null()
   contains
     !> final procedures are called whenever type instance is passed
     !> as intent(out) or deallocated.
     final :: field_destructor
  end type field_t

  interface field_t
     module procedure :: field_constructor, field_from_real
  end interface field_t

  interface assignment(=)
     module procedure field_from_field
  end interface assignment(=)

  interface operator(+)
     module procedure field_add_field
  end interface operator(+)

contains

  type(field_t) function field_constructor() result(f)
    !> Construct a field_t with data pointer pointing to newly
    !> allocated memory block from the memory pool.
    f%data => get_memory_block()
  end function field_constructor

  type(field_t) function field_from_real(a) result(f)
    real, intent(in) :: a
    f%data => get_memory_block()
    f%data%segment = a
  end function field_from_real

  subroutine field_destructor(self)
    !> This procedure is marked as final in the field_t type
    !> definition.  Is will be called each time a field_t is passed as
    !> intent(out) or deallocated.
    type(field_t), intent(inout) :: self
    if(associated(self%data)) then
       self%data%refcount = self%data%refcount - 1
       if (self%data%refcount == 0) then
          call release(self%data)
       end if
    end if
  end subroutine field_destructor

  subroutine field_from_field(a, b)
    !> Defines assignment =.
    type(field_t), intent(out) :: a
    type(field_t), intent(in) :: b
    a%data => b%data
    b%data%refcount = b%data%refcount + 1
  end subroutine field_from_field

  function field_add_field(a, b)
    !> Defines operator + for field_t objects.  Result is allocated a
    !> new memory block from the pool.
    type(field_t) :: field_add_field
    type(field_t), intent(in) :: a
    type(field_t), intent(in) :: b

    field_add_field%data => get_memory_block()
    field_add_field%data%segment = a%data%segment + b%data%segment
  end function field_add_field
end module field_module
