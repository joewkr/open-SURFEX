!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE UPDATE_DATA_FRAC_n(PDATA_NATURE,PDATA_TOWN,PDATA_GARDEN,OGARDEN, &
                                    PDATA_BLD, PDATA_WALL_O_HOR                   )
!     #########################
!
!!**** *INI_DATA_FRAC* takes into account gardens into natural vegetation
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2011
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODI_ARRANGE_COVER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_NATURE
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_TOWN
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_GARDEN
LOGICAL,            INTENT(IN)  :: OGARDEN
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_BLD
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_WALL_O_HOR
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UPDATE_DATA_FRAC_n',0,ZHOOK_HANDLE)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_DATA_FRAC_n',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------

END SUBROUTINE UPDATE_DATA_FRAC_n
