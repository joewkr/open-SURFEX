program test_grib_api
use :: grib_api
implicit none
integer :: ifile

call grib_open_file(ifile, "dummy", "R")
call grib_close_file(ifile)

end program test_grib_api
