program main
  use iso_c_binding, only : c_loc, c_int
  use  ISO_Fortran_binding_module, only : CFI_address, CFI_cdesc_t, CFI_index_t
  implicit none
  integer(c_int), target :: array(2,1)=reshape([3,4],[2,1])
  type(CFI_cdesc_t), target ::array_descriptor
  integer(CFI_index_t), parameter :: subscripts(*)=[1,1]
  array_descriptor%base_addr= c_loc(array)
  array_descriptor%rank = 1
  array_descriptor%dim(1)%lower_bound = 1
  array_descriptor%dim(1)%extent=1
  array_descriptor%dim(2)%lower_bound=1
  array_descriptor%dim(2)%extent=1
  print *,CFI_address(array_descriptor,subscripts)
end program
