module field
  use pool_module

  implicit none

  type field_t
     type(memory_block_t), pointer :: data
   contains
     final :: field_destructor
  end type field_t

  interface field_t
     module procedure field_constructor
  end interface field_t

  interface assignment
     module procedure field_from_field
  end interface assignment

contains

  type(field_t) function field_constructor() result(f)
    f%data => get_memory_block()
  end function field_constructor

  subroutine field_destructor(self)
    type(field_t), intent(inout) :: self
    call unbind_or_release(self%data)
  end subroutine field_destructor

  subroutine field_from_field(a, b)
    type(field_t), intent(out) :: a
    type(field_t), intent(in) :: b
    call unbind_or_release(a%data)
    a%data => b%data
    b%data%refcount = b%data%refcount + 1
  end subroutine field_from_field

end module field
