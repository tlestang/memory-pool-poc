module field
  use pool_module

  implicit none

  type field_t
     type(memory_block_t), pointer :: data => null()
   contains
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
    f%data => get_memory_block()
  end function field_constructor

  type(field_t) function field_from_real(a) result(f)
    real, intent(in) :: a
    f%data => get_memory_block()
    f%data%segment = a
  end function field_from_real

  subroutine field_destructor(self)
    type(field_t), intent(inout) :: self
    if(associated(self%data)) then
       call unbind_or_release(self%data)
    end if
  end subroutine field_destructor

  subroutine field_from_field(a, b)
    type(field_t), intent(out) :: a
    type(field_t), intent(in) :: b
    a%data => bind_block(b%data)
  end subroutine field_from_field

  function field_add_field(a, b)
    type(field_t) :: field_add_field
    type(field_t), intent(in) :: a
    type(field_t), intent(in) :: b

    field_add_field%data => a%data + b%data
  end function field_add_field
end module field
