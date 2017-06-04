!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE AVERAGE2_OROGRAPHY (USS)
!     #########################
!
!!**** *AVERAGE2_OROGRAPHY* computes the cover fractions
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
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_PGDWORK,       ONLY : NSIZE, XSUMVAL, LSSQO, XSSQO, NSSO, XPREC
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
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!
TYPE(SSO_t), INTENT(INOUT) :: USS
!
REAL :: ZINT
INTEGER                  :: JL, JI
REAL,    DIMENSION(NSSO) :: ZMAXX
REAL,    DIMENSION(NSSO) :: ZMAXY
LOGICAL, DIMENSION(NSSO) :: GSEGX
LOGICAL, DIMENSION(NSSO) :: GSEGY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    1.     Mean orography
!            --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_OROGRAPHY',0,ZHOOK_HANDLE)
WHERE (NSIZE(:,1)/=0)
  USS%XAVG_ZS(:) = XSUMVAL(:,1)/NSIZE(:,1)
END WHERE
!
!-------------------------------------------------------------------------------
!
!*    2.     Standard deviation
!            ------------------
!
WHERE (NSIZE(:,1)/=0)
  USS%XSSO_STDEV(:) = SQRT( MAX(0.,XSUMVAL(:,2)/NSIZE(:,1) - USS%XAVG_ZS(:)*USS%XAVG_ZS(:)) )
END WHERE
!
!-------------------------------------------------------------------------------
!
!*    3.     Silhouette orography
!            --------------------
!
DO JL=1,SIZE(USS%XSIL_ZS)
  IF (NSIZE(JL,1)==0) CYCLE
  ZMAXX(:) = MAXVAL(XSSQO(JL,:,:),DIM=2)
  GSEGX(:) = ANY   (LSSQO(JL,:,:),DIM=2)
  ZMAXY(:) = MAXVAL(XSSQO(JL,:,:),DIM=1)
  GSEGY(:) = ANY   (LSSQO(JL,:,:),DIM=1)
  USS%XSIL_ZS(JL) =0.5*(  SUM(ZMAXX(:),MASK=GSEGX(:)) / COUNT(GSEGX(:)) &
                        + SUM(ZMAXY(:),MASK=GSEGY(:)) / COUNT(GSEGY(:)) )  
  
END DO
!
!
DO JI = 1,SIZE(USS%XAVG_ZS)

  IF (USS%XAVG_ZS(JI)/=XUNDEF) THEN

    ZINT = AINT(USS%XAVG_ZS(JI))
    IF (USS%XAVG_ZS(JI)/=ZINT) &
      USS%XAVG_ZS(JI) = ZINT + ANINT((USS%XAVG_ZS(JI)-ZINT)*XPREC)/XPREC

    ZINT = AINT(USS%XSSO_STDEV(JI))
    IF (USS%XSSO_STDEV(JI)/=ZINT) &
      USS%XSSO_STDEV(JI) = ZINT + ANINT((USS%XSSO_STDEV(JI)-ZINT)*XPREC)/XPREC

    ZINT = AINT(USS%XSIL_ZS(JI))
    IF (USS%XSIL_ZS(JI)/=ZINT) &
      USS%XSIL_ZS(JI) = ZINT + ANINT((USS%XSIL_ZS(JI)-ZINT)*XPREC)/XPREC

  ENDIF

ENDDO
!    
IF (LHOOK) CALL DR_HOOK('AVERAGE2_OROGRAPHY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE2_OROGRAPHY
