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
! ====================== MODULE modi_gltools_strlower =====================
! =======================================================================
!
! Goal:
! ------
!   If an uppercase string is given to this routine, it returns it in
! lowercase.
!
! Created:
! ---------
!   07/2010 (D. Salas, Meteo-France)
!
! Modified:
! ----------
!   No.
!
! ------------------- BEGIN MODULE modi_gltools_strlower ------------------
!
!THXS_SFX!MODULE modi_gltools_strlower
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!FUNCTION gltools_strlower( hstring ) RESULT(hresult)
!THXS_SFX!  CHARACTER(LEN=*), INTENT(in) ::  &
!THXS_SFX!    hstring
!THXS_SFX!  CHARACTER(LEN=LEN(hstring)) ::  &
!THXS_SFX!    hresult
!THXS_SFX!END FUNCTION gltools_strlower
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_strlower
!
! ------------------- END MODULE modi_gltools_strlower ------------------
!
!
! ---------------------------------------------------------------------
! ------------------------- FUNCTION gltools_strlower -------------------------
! 
FUNCTION gltools_strlower( hstring ) RESULT(hresult)
!
  IMPLICIT NONE
!
  CHARACTER(LEN=*), INTENT(in) ::  &
    hstring
  CHARACTER(LEN=LEN(hstring)) ::  &
    hresult
!
  INTEGER ::  &
    ic,ji
!
!
!* Note that characters from A to Z have a code between 65 and 90, and 
! characters from a to z have a code between 97 and 122.
! (so a conversion from uppercase to lowercase amounts to adding 32 to
! the character code)
!
  DO ji = 1,LEN(hstring)
    ic = ICHAR( hstring(ji:ji) )
    IF ( ic>=65 .AND. ic<=90 ) THEN
        hresult(ji:ji) = CHAR(ic+32)
      ELSE
        hresult(ji:ji) = CHAR(ic)
    ENDIF
  END DO
!
END FUNCTION gltools_strlower
!
! ----------------------- END FUNCTION gltools_strlower -----------------------
! ---------------------------------------------------------------------
