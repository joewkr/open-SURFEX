program test_netcdf
use :: netcdf
implicit none
integer :: ncid, status

status = nf90_open(path = "dummy", mode = NF90_NOWRITE, ncid = ncid)
status = nf90_close(ncid = ncid)
end program test_netcdf
