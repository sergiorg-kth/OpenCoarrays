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
    class(*), pointer :: array01D(:)
    class(*), pointer :: array02D(:,:)
    class(*), pointer :: array03D(:,:,:)
    class(*), pointer :: array04D(:,:,:,:)
    class(*), pointer :: array05D(:,:,:,:,:)
    class(*), pointer :: array06D(:,:,:,:,:,:)
    class(*), pointer :: array07D(:,:,:,:,:,:,:)
    class(*), pointer :: array08D(:,:,:,:,:,:,:,:)
    class(*), pointer :: array09D(:,:,:,:,:,:,:,:,:)
    class(*), pointer :: array10D(:,:,:,:,:,:,:,:,:,:)
    class(*), pointer :: array11D(:,:,:,:,:,:,:,:,:,:,:)
    class(*), pointer :: array12D(:,:,:,:,:,:,:,:,:,:,:,:)
    class(*), pointer :: array13D(:,:,:,:,:,:,:,:,:,:,:,:,:)
    class(*), pointer :: array14D(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    class(*), pointer :: array15D(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer :: i
    associate(s=>subscripts)
    select case (dv%rank)
      case(1)
        call c_f_pointer(dv%base_addr,array01D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array01D(s(1)))
      case(2)
        call c_f_pointer(dv%base_addr,array02D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array02D(s(1),s(2)))
      case(3)
        call c_f_pointer(dv%base_addr,array03D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array03D(s(1),s(2),s(3)))
      case(4)
        call c_f_pointer(dv%base_addr,array04D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array04D(s(1),s(2),s(3),s(4)))
      case(5)
        call c_f_pointer(dv%base_addr,array05D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array05D(s(1),s(2),s(3),s(4),s(5)))
      case(6)
        call c_f_pointer(dv%base_addr,array06D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array06D(s(1),s(2),s(3),s(4),s(5),s(6)))
      case(7)
        call c_f_pointer(dv%base_addr,array07D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array07D(s(1),s(2),s(3),s(4),s(5),s(6),s(7)))
      case(8)
        call c_f_pointer(dv%base_addr,array08D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array08D(s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8)))
      case(9)
        call c_f_pointer(dv%base_addr,array09D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array09D(s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9)))
      case(10)
        call c_f_pointer(dv%base_addr,array10D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array10D(s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10)))
      case(11)
        call c_f_pointer(dv%base_addr,array11D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array11D(s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11)))
      case(12)
        call c_f_pointer(dv%base_addr,array12D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array12D(s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12)))
      case(13)
        call c_f_pointer(dv%base_addr,array13D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array13D(s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13)))
      case(14)
        call c_f_pointer(dv%base_addr,array14D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array14D(s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14)))
      case(15)
        call c_f_pointer(dv%base_addr,array15D,[(dv%dim(i)%extent,i=1,dv%rank)])
        address = c_loc(array15D(s(1),s(2),s(3),s(4),s(5),s(6),s(7),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15)))
    end select
    end associate
  end function

end module
