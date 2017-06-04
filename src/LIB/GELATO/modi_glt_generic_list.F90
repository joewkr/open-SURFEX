!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!GLT_LIC The GELATO model is a seaice model used in stand-alone or embedded mode. 
!GLT_LIC  It has been developed by Meteo-France. The holder of GELATO is Meteo-France.
!GLT_LIC  
!GLT_LIC  This software is governed by the CeCILL-C license under French law and biding
!GLT_LIC  by the rules of distribution of free software. See the CeCILL-C_V1-en.txt
!GLT_LIC  (English) and CeCILL-C_V1-fr.txt (French) for details. The CeCILL is a free
!GLT_LIC  software license, explicitly compatible with the GNU GPL
!GLT_LIC  (see http://www.gnu.org/licenses/license-list.en.html#CeCILL)
!GLT_LIC  
!GLT_LIC  The CeCILL-C licence agreement grants users the right to modify and re-use the
!GLT_LIC  software governed by this free software license. The exercising of this right
!GLT_LIC  is conditional upon the obligation to make available to the community the
!GLT_LIC  modifications made to the source code of the software so as to contribute to
!GLT_LIC  its evolution.
!GLT_LIC  
!GLT_LIC  In consideration of access to the source code and the rights to copy, modify
!GLT_LIC  and redistribute granted by the license, users are provided only with a limited
!GLT_LIC  warranty and the software's author, the holder of the economic rights, and the
!GLT_LIC  successive licensors only have limited liability. In this respect, the risks
!GLT_LIC  associated with loading, using, modifying and/or developing or reproducing the
!GLT_LIC  software by the user are brought to the user's attention, given its Free
!GLT_LIC  Software status, which may make it complicated to use, with the result that its
!GLT_LIC  use is reserved for developers and experienced professionals having in-depth
!GLT_LIC  computer knowledge. Users are therefore encouraged to load and test the
!GLT_LIC  suitability of the software as regards their requirements in conditions enabling
!GLT_LIC  the security of their systems and/or data to be ensured and, more generally, to
!GLT_LIC  use and operate it in the same conditions of security. 
!GLT_LIC  
!GLT_LIC  The GELATO sofware is cureently distibuted with the SURFEX software, available at 
!GLT_LIC  http://www.cnrm.meteo.fr/surfex. The fact that you download the software deemed that
!GLT_LIC  you had knowledge of the CeCILL-C license and that you accept its terms.
!GLT_LIC  Attempts to use this software in a way not complying with CeCILL-C license
!GLT_LIC  may lead to prosecution. 
!GLT_LIC 
! glt_generic_list -- A Generic Linked List Implementation in Fortran 95
!
! Copyright (C) 2009 Jason R. Blevins
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.

module data
  implicit none

  private
  public :: data_t
  public :: data_ptr

  ! Data is stored in data_t
  type :: data_t
     real :: x
  end type data_t

  ! A container for storing data_t pointers
  type :: data_ptr
     type(data_t), pointer :: p
  end type data_ptr

end module data


module modi_glt_generic_list
  implicit none

  private
  public :: list_node_t, list_data
  public :: list_init, list_free
  public :: list_insert, list_put, list_get, list_next

  ! A public variable used as a MOLD for transfer()
  integer, dimension(:), allocatable :: list_data

  ! Linked list node
  type :: list_node_t
     private
     integer, dimension(:), pointer :: data => null()
     type(list_node_t), pointer :: next => null()
  end type list_node_t

contains

  ! Initialize a head node SELF and optionally store the provided DATA.
  subroutine list_init(self, data)
    type(list_node_t), pointer :: self
    integer, dimension(:), intent(in), optional :: data

    allocate(self)
    nullify(self%next)

    if (present(data)) then
       allocate(self%data(size(data)))
       self%data = data
    else
       nullify(self%data)
    end if
  end subroutine list_init

  ! Free the entire list and all data, beginning at SELF
  subroutine list_free(self)
    type(list_node_t), pointer :: self
    type(list_node_t), pointer :: current
    type(list_node_t), pointer :: next

    current => self
    do while (associated(current))
       next => current%next
       if (associated(current%data)) then
          deallocate(current%data)
          nullify(self%data)
       end if
       deallocate(current)
       nullify(current)
       current => next
    end do
  end subroutine list_free

  ! Insert a list node after SELF containing DATA (optional)
  subroutine list_insert(self, data)
    type(list_node_t), pointer :: self
    integer, dimension(:), intent(in), optional :: data
    type(list_node_t), pointer :: next

    allocate(next)

    if (present(data)) then
       allocate(next%data(size(data)))
       next%data = data
    else
       nullify(next%data)
    end if

    next%next => self%next
    self%next => next
  end subroutine list_insert

  ! Store the encoded DATA in list node SELF
  subroutine list_put(self, data)
    type(list_node_t), pointer :: self
    integer, dimension(:), intent(in) :: data

    if (associated(self%data)) then
       deallocate(self%data)
       nullify(self%data)
    end if
    self%data = data
  end subroutine list_put

  ! Return the DATA stored in the node SELF
  function list_get(self) result(data)
    type(list_node_t), pointer :: self
    integer, dimension(:), pointer :: data
    data => self%data
  end function list_get

  ! Return the next node after SELF
  function list_next(self)
    type(list_node_t), pointer :: self
    type(list_node_t), pointer :: list_next
    list_next => self%next
  end function list_next

end module modi_glt_generic_list


program test_list
  use modi_glt_generic_list
  use data
  implicit none

  type(list_node_t), pointer :: list => null()
  type(data_ptr) :: ptr

  ! Allocate a new data element
  allocate(ptr%p)
  ptr%p%x = 2.7183

  ! Initialize the list with the first data element
  call list_init(list, transfer(ptr, list_data))
  print *, 'Initializing list with data:', ptr%p

  ! Allocate a second data element
  allocate(ptr%p)
  ptr%p%x = 0.5772

  ! Insert the second into the list
  call list_insert(list, transfer(ptr, list_data))
  print *, 'Inserting node with data:', ptr%p

  ! Retrieve data from the second node and free memory
  ptr = transfer(list_get(list_next(list)), ptr)
  print *, 'Second node data:', ptr%p
  deallocate(ptr%p)

  ! Retrieve data from the head node and free memory
  ptr = transfer(list_get(list), ptr)
  print *, 'Head node data:', ptr%p
  deallocate(ptr%p)

  ! Free the list
  call list_free(list)
end program test_list
