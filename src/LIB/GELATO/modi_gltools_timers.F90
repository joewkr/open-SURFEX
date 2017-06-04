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
! ====================== MODULE modi_gltools_timers =======================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that allows to compute the
! elapsed time between a CALL to 'timers' routine and the previous
! CALL.
!
! Method:
! --------
!   This routine is based on the use of CPU_TIME fortran function.
!
! Input:
! ------
!   A character string (optional) to help you locating between which
! points the time has been evaluated.
!
! Created : 2012/05 (D. Salas y Melia)
!
! ------------------- BEGIN MODULE modi_gltools_timers --------------------
!
!THXS_SFX!MODULE modi_gltools_timers
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_timers( hlabel )
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  CHARACTER(LEN=*), OPTIONAL, INTENT(in) ::  &
!THXS_SFX!    hlabel 
!THXS_SFX!END SUBROUTINE gltools_timers
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_timers
!
! -------------------- END MODULE modi_gltools_timers ---------------------
!
!
! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE gltools_timers --------------------------
!
SUBROUTINE gltools_timers( hlabel )
!
  USE modd_glt_param
!
  IMPLICIT NONE
!
  CHARACTER(LEN=*), OPTIONAL, INTENT(in) ::  &
    hlabel 
!
! .. Local variables
!
  REAL ::  &
    ztime
  CHARACTER(LEN=80) ::   &
    ylabel
!
!
  IF ( ntimers==1 ) THEN
! 
! .. CALL number
      ntimnum = ntimnum + 1
!
! .. Get input label
      IF ( PRESENT(hlabel) ) THEN
        IF (LEN(hlabel)>80) THEN
          ylabel = hlabel(1:80)
        ELSE
          WRITE( ylabel,'(A80)' ) hlabel
        ENDIF
      ELSE 
        ylabel = ''
      ENDIF
!
! .. Get current time
      CALL CPU_TIME(ztime)
!
! .. Print message
      IF(lwg) WRITE(ntimlu,'("BETWEEN ",A," (CALL # ",I6,") AND ",A, &
   &    " (CALL # ",I6,")")')  &
   &    TRIM(ADJUSTL(clabel)),ntimnum-1,TRIM(ADJUSTL(ylabel)),  &
   &    ntimnum
      IF(lwg) WRITE(ntimlu,'(" --> exec time = ",F9.3," s", &
   &    " / total time = ",F9.3," s")')  &
   &    ztime-xtime,ztime
!      CALL flush(ntimlu)
!
! .. Save current label and time, for next CALL
      clabel = ylabel
      xtime = ztime
  ENDIF
!
END SUBROUTINE gltools_timers
!
! ----------------------- END SUBROUTINE gltools_timers -------------------------
! -----------------------------------------------------------------------
