module ISO_Fortran_binding_module
  use iso_c_binding, only : c_int8_t, c_int16_t, c_ptrdiff_t, c_size_t, c_ptr, c_int, c_loc, c_f_pointer
  implicit none

  integer(kind=kind(c_ptrdiff_t)), parameter :: CFI_index_t=c_ptrdiff_t
  integer(kind=kind(c_int8_t)), parameter :: CFI_rank_t=c_int8_t
  integer(kind=kind(c_int8_t)) , parameter :: CFI_attribute_t=c_int8_t
  integer(kind=kind(c_int16_t)), parameter  ::CFI_type_t=c_int16_t

  type, bind(C) :: CFI_dim_t
     integer(CFI_index_t) lower_bound
     integer(CFI_index_t) extent
     integer(CFI_index_t) sm
  end type CFI_dim_t

  type, bind(C) :: CFI_cdesc_t
    type(c_ptr) base_addr
    integer(c_size_t) elem_len
    integer(c_int) version
    integer(CFI_rank_t) rank
    integer(CFI_attribute_t) attribute
    integer(CFI_type_t) type
    integer(c_size_t) offset
    type(CFI_dim_t) :: dim(15)
  end type CFI_cdesc_t

contains

  function CFI_address(dv, subscripts) bind(C,name="CFI_address") result(address)
    !! C signature: void *CFI_address (const CFI_cdesc_t *dv, const CFI_index_t subscripts[])
    type(CFI_cdesc_t), intent(in), target :: dv
    integer(CFI_index_t), intent(in) :: subscripts(:)
    type(c_ptr) :: address
    integer(c_int), pointer :: array(:,:)

    call c_f_pointer(dv%base_addr,array,[2,1])
    address = c_loc(array(subscripts(1),subscripts(2)))
  end function

end module
