module field
  implicit none

  type field_t
     real, pointer :: data(:)
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
    use pool_module
    f%data => get_memory_block()
  end function field_constructor

  subroutine field_destructor(self)
    use pool_module
    type(field_t), intent(inout) :: self
    call release_memory_block(self%data)
  end subroutine field_destructor

  subroutine field_from_field(a, b)
    type(field_t), intent(out) :: a
    type(field_t), intent(in) :: b
    a%data => b%data
  end subroutine field_from_field

end module field
