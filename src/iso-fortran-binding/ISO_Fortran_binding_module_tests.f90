program main
  use iso_c_binding, only : c_loc, c_int, c_associated
  use  ISO_Fortran_binding_module, only : CFI_address_, CFI_cdesc_t, CFI_index_t
  implicit none
  integer(c_int), target :: array(1)=[99]
  type(CFI_cdesc_t), target ::array_descriptor
  integer(CFI_index_t), parameter :: subscripts(*)=[1]

  array_descriptor%base_addr= c_loc(array)
  array_descriptor%rank = 1
  array_descriptor%dim(1)%lower_bound = 1
  array_descriptor%dim(1)%extent=1
  array_descriptor%dim(2)%lower_bound=1
  array_descriptor%dim(2)%extent=1

  print*, c_associated( array_descriptor%base_addr , CFI_address_(array_descriptor,[1_CFI_index_t] ) )

end program
