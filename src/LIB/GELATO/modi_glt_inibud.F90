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
! =======================================================================
! ========================= MODULE modi_glt_inibud ==========================
! =======================================================================
!
! Goal:
! ----- 
!    This module contains a subroutine that initializes the energy 
! budget that can be optionally computed in GELATO.
!  
! Method:
! -------
!    Whenever the development of the glt_gelato code takes place, it is
! important to check that the model does not lose/gain any energy.
! A loss/gain often indicates there is a bug, even if the code seems
! to run properly.
!
! Created : 2001/08 (D. Salas y Melia) 
!           The routine computes only the energy budget for the time
!           being. Further development should also include the water
!           budget. 
!    
! ---------------------- BEGIN MODULE modi_glt_inibud -----------------------
!
!THXS_SFX!MODULE modi_glt_inibud
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_inibud(tpbud)
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  TYPE(t_bud), DIMENSION(:,:), INTENT(inout) ::  &
!THXS_SFX!        tpbud
!THXS_SFX!END SUBROUTINE glt_inibud
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_inibud
!
! ----------------------- END MODULE modi_glt_inibud ------------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE glt_inibud ---------------------------
!
SUBROUTINE glt_inibud(tpbud)
!
  USE modd_types_glt
  USE modd_glt_param
!
  IMPLICIT NONE 
!
  TYPE(t_bud), DIMENSION(:,:), INTENT(inout) ::                         &
        tpbud
!  
!  
! * Set all the components of the budget to zero (see definitions in 
! dmod_types.f90 module).
!
  tpbud(:,:)%eni = 0.
  tpbud(:,:)%enn = 0.
  tpbud(:,:)%bii = 0.
  tpbud(:,:)%nii = 0.
  tpbud(:,:)%nli = 0.
  tpbud(:,:)%hii = 0.
  tpbud(:,:)%hli = 0.
  tpbud(:,:)%hio = 0.
  tpbud(:,:)%hlo = 0.
  tpbud(:,:)%wii = 0.
  tpbud(:,:)%wli = 0.
  tpbud(:,:)%fwi = 0.
  tpbud(:,:)%fwn = 0.
  tpbud(:,:)%isi = 0.
  tpbud(:,:)%isn = 0.
!
END SUBROUTINE glt_inibud
! 
! ----------------------- END SUBROUTINE glt_inibud -------------------------
! -----------------------------------------------------------------------
