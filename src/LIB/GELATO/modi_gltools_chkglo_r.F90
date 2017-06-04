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
! ===================== MODULE modi_gltools_chkglo_r ====================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that prints sea ice surface,  
! extent and volume separately in both hemispheres. Note these results
! are printed only if nprinto flag is greater or equal to 2.
!   Note that mpp_sum (invoke all procs) cannot be used e.g. under 
! condition lp2==.TRUE. Hence we use nprinto.
!
! Created : 1999 (D. Salas y Melia)
!           Repeated code doing that throughout the model is written in
!           a more standard form and only once in the present routine. 
! Modified: 2009/06 (D. Salas y Melia) reduced grid
! Modified: 2012/07 (D. Salas y Melia) parallelism
!
! ------------------ BEGIN MODULE modi_gltools_chkglo_r -----------------

!THXS_SFX!MODULE modi_gltools_chkglo_r
!THXS_SFX!INTERFACE 
!THXS_SFX!
!THXS_SFX!SUBROUTINE gltools_chkglo_r(omsg,tpdom,tpsit)
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  CHARACTER(*), INTENT(in) ::  &
!THXS_SFX!        omsg
!THXS_SFX!  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
!THXS_SFX!        tpdom
!THXS_SFX!  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
!THXS_SFX!        tpsit
!THXS_SFX!END SUBROUTINE gltools_chkglo_r
!THXS_SFX!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_chkglo_r

! ------------------- END MODULE modi_gltools_chkglo_r ------------------


! -----------------------------------------------------------------------
! -------------------- SUBROUTINE gltools_chkglo_r ----------------------

! * Subroutine used to check global sea ice extent, area and volume in 
! both hemispheres.

SUBROUTINE gltools_chkglo_r(omsg,tpdom,tpsit)
!
  USE modd_glt_const_thm
  USE modd_types_glt
  USE modd_glt_param
#if ! defined in_arpege
  USE lib_mpp
#endif
  IMPLICIT NONE
!
  CHARACTER(*), INTENT(in) ::  &
        omsg
  TYPE(t_dom), DIMENSION(np), INTENT(in) ::  &
        tpdom
  TYPE(t_sit), DIMENSION(nt,np), INTENT(in) ::  &
        tpsit
!
  LOGICAL, DIMENSION(np) ::  &
        ghnorth,ghsouth
  REAL ::  &
        zlatn0,zlats0,zehn,zehs,zshn,zshs,zvhn,zvhs 
  REAL, DIMENSION(np) ::  &
        zfsit,zhsiw
!
!
!
! 1. Initializations
! ==================
!
  IF ( nprinto>=2 ) THEN
!
! .. Print message
!
      IF (lwg) THEN
        WRITE(noutlu,*) ' ' 
        WRITE(noutlu,*) ' **** gltools_chkglo_r ****' 
        WRITE(noutlu,*) omsg
      ENDIF
!
! .. Northern and southern boundaries (latitude)
!
      zlatn0 = 0.  ! 40.*pi/180.
      zlats0 = 0.  ! -40.*pi/180.
!
! .. Compute the total concentration of sea ice and its average thickness
! field.
!
      zfsit(:) = SUM( tpsit(:,:)%fsi, DIM=1 )
!
      zhsiw(:) = SUM( tpsit(:,:)%fsi*tpsit(:,:)%hsi, DIM=1 )
!
! .. Define the northern and southern domains 
!
      ghnorth(:) = .FALSE. 
      ghsouth(:) = .FALSE. 
!
      WHERE( tpdom(:)%lat>zlatn0 )
          ghnorth(:) = .TRUE.
      ENDWHERE
!
      WHERE( tpdom(:)%lat<zlats0 )
          ghsouth(:) = .TRUE. 
      ENDWHERE
!
!
! 
! 2. Compute global quantities (north and south)
! ==============================================
!
! 2.1. Sea ice extent (in millions of km2)
! ----------------------------------------
!
      zehn = SUM( tpdom(:)%srf, MASK=(ghnorth(:).AND.zfsit(:)>xfsic) ) / 1.e+12
      zehs = SUM( tpdom(:)%srf, MASK=(ghsouth(:).AND.zfsit(:)>xfsic) ) / 1.e+12
!
!
! 2.2. Sea ice area (in millions of km2)
! --------------------------------------
!
      zshn = SUM( tpdom(:)%srf*zfsit(:), MASK=ghnorth(:) ) / 1.e+12
      zshs = SUM( tpdom(:)%srf*zfsit(:), MASK=ghsouth(:) ) / 1.e+12
!
!
! 2.3. Sea ice volume  
! -------------------
!
      zvhn = SUM( tpdom(:)%srf*zhsiw(:), MASK=ghnorth(:) ) / 1.e+12
      zvhs = SUM( tpdom(:)%srf*zhsiw(:), MASK=ghsouth(:) ) / 1.e+12
!
!
!
! 3. Write totals to output file
! ==============================
!
#if ! defined in_arpege
      CALL mpp_sum( zshn )
      CALL mpp_sum( zshs )
      CALL mpp_sum( zehn )
      CALL mpp_sum( zehs )
      CALL mpp_sum( zvhn )
      CALL mpp_sum( zvhs )
!
      IF (lwg) THEN
        WRITE(noutlu,*) '                              North        South'
        WRITE(noutlu,1000) zshn,zshs
        WRITE(noutlu,1100) zehn,zehs
        WRITE(noutlu,1200) zvhn,zvhs
      ENDIF
#endif
!
  ENDIF 
!
!
!
! 4. Formats
! ==========
!
1000 FORMAT( 5X,"Ice surface (SISH.SIG)",2(4X,F9.5) )
1100 FORMAT( 5X,"Ice extent  (SIEH.SIG)",2(4X,F9.5) )
1200 FORMAT( 5X,"Ice volume  (SIVH.SIG)",2(4X,F9.5) ) 
!
END SUBROUTINE gltools_chkglo_r

! ---------------------- END SUBROUTINE gltools_chkglo_r ------------------------
! -----------------------------------------------------------------------
