program main
  use sac_io
  implicit none
  integer :: unt
  type(sac_typ) :: test_alpha, test_binary

  open(newunit=unt, file='alpha.sac')
  read(unt, fmt=*) test_alpha
  close(unt)

  open(newunit=unt, file='obspy.sac', access='stream')
  read(unt) test_binary
  close(unt)

end
