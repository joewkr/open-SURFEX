!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CONVERT_COVER_FRAC (DTCO, &
                                        PCOVER, OCOVER,           &
                                         PSEA,PNATURE,PTOWN,PWATER   )  
!     ##############################################################
!
!!**** *CONVERT_COVER* convert surface cover classes into secondary
!!                     physiographic variables
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
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
!!    Original    01/2004
!     
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_AV_PGD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PCOVER
LOGICAL, DIMENSION(:), INTENT(IN)   :: OCOVER
REAL, DIMENSION(:),   INTENT(OUT)   :: PSEA
REAL, DIMENSION(:),   INTENT(OUT)   :: PNATURE
REAL, DIMENSION(:),   INTENT(OUT)   :: PTOWN
REAL, DIMENSION(:),   INTENT(OUT)   :: PWATER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------
!
!*    1.      cover main type fractions
!             -------------------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_COVER_FRAC',0,ZHOOK_HANDLE)
 CALL AV_PGD(DTCO, &
              PSEA    ,PCOVER(:,:),DTCO%XDATA_SEA    (:),'ALL','ARI',OCOVER(:))
 CALL AV_PGD(DTCO, &
              PTOWN   ,PCOVER(:,:),DTCO%XDATA_TOWN   (:),'ALL','ARI',OCOVER(:))
 CALL AV_PGD(DTCO, &
              PNATURE ,PCOVER(:,:),DTCO%XDATA_NATURE (:),'ALL','ARI',OCOVER(:))
 CALL AV_PGD(DTCO, &
              PWATER  ,PCOVER(:,:),DTCO%XDATA_WATER  (:),'ALL','ARI',OCOVER(:))

!
WHERE (PSEA   (:) == XUNDEF) PSEA   (:) = 0.
WHERE (PNATURE(:) == XUNDEF) PNATURE(:) = 0.
WHERE (PTOWN  (:) == XUNDEF) PTOWN  (:) = 0.
WHERE (PWATER (:) == XUNDEF) PWATER (:) = 0.
IF (LHOOK) CALL DR_HOOK('CONVERT_COVER_FRAC',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CONVERT_COVER_FRAC
