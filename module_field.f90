module field
  use pool_module

  implicit none

  type field_t
     type(memory_block_t), pointer :: data => null()
   contains
     final :: field_destructor
  end type field_t

  interface field_t
     module procedure field_constructor
  end interface field_t

  interface assignment(=)
     module procedure field_from_field
  end interface
  interface operator(+)
     module procedure field_add_field
  end interface operator(+)

contains

  type(field_t) function field_constructor() result(f)
    f%data => get_memory_block()
  end function field_constructor

  subroutine field_destructor(self)
    type(field_t), intent(inout) :: self
    if(associated(self%data)) then
       call unbind_or_release(self%data)
    end if
  end subroutine field_destructor

  subroutine field_from_field(a, b)
    type(field_t), intent(out) :: a
    type(field_t), intent(in) :: b
    a%data => b%data
    b%data%refcount = b%data%refcount + 1
  end subroutine field_from_field

  function field_add_field(a, b)
    type(field_t) :: field_add_field
    type(field_t), intent(in) :: a
    type(field_t), intent(in) :: b

    field_add_field%data => a%data + b%data
  end function field_add_field
end module field
