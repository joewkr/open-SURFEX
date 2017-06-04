!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE AVERAGE2_CTI
!     #######################
!
!!**** *AVERAGE2_CTI* computes the topo index stats
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
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2009
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_PGDWORK,       ONLY : NSIZE, XSUMVAL, XPREC, &
                               XMEAN_WORK, XSTD_WORK, XSKEW_WORK, &
                               XMIN_WORK, XMAX_WORK 
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
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(SIZE(NSIZE,1)) :: ZSIZE
!
REAL :: ZINT
INTEGER :: JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_CTI',0,ZHOOK_HANDLE)
ZSIZE(:)=REAL(NSIZE(:,1))
!
WHERE (NSIZE(:,1)>=36)
!
!----------------------------------------------------------------------------
!
!*    1.     Mean CTI
!            --------------
!
  XMEAN_WORK(:) = XSUMVAL(:,1)/ZSIZE(:)
!
!-------------------------------------------------------------------------------
!
!*    2.     Standard deviation
!            ------------------
!
  WHERE (XMAX_WORK(:)-XMIN_WORK(:)>=1.0) 
    XSTD_WORK(:) = SQRT( MAX(0.,XSUMVAL(:,2)/NSIZE(:,1) - XMEAN_WORK(:)*XMEAN_WORK(:)) )
  ELSEWHERE
    XSTD_WORK(:) = 0.0
  END WHERE
!
!-------------------------------------------------------------------------------
!
!*    3.     Skewness
!            --------
!
  WHERE(XSTD_WORK(:)>0.0)
!          
        XSKEW_WORK(:) = XSUMVAL(:,3)-ZSIZE(:)*XMEAN_WORK(:)*XMEAN_WORK(:)*XMEAN_WORK(:) &
                       -3.0*ZSIZE(:)*XMEAN_WORK(:)*XSTD_WORK(:)*XSTD_WORK(:)  
!
        XSKEW_WORK(:) = XSKEW_WORK(:)/(ZSIZE(:)*XSTD_WORK(:)*XSTD_WORK(:)*XSTD_WORK(:))
!
  END WHERE
!
END WHERE
!----------------------------------------------------------------------------
!
DO JI = 1,SIZE(XMEAN_WORK,1)

  IF (XMEAN_WORK(JI)/=XUNDEF) THEN

    ZINT = AINT(XMEAN_WORK(JI))
    IF (XMEAN_WORK(JI)/=ZINT) &
      XMEAN_WORK(JI) = ZINT + ANINT((XMEAN_WORK(JI)-ZINT)*XPREC)/XPREC

    ZINT = AINT(XMIN_WORK(JI))
    IF (XMIN_WORK(JI)/=ZINT) &
      XMIN_WORK(JI) = ZINT + ANINT((XMIN_WORK(JI)-ZINT)*XPREC)/XPREC

    ZINT = AINT(XMAX_WORK(JI))
    IF (XMAX_WORK(JI)/=ZINT) &      
      XMAX_WORK(JI) = ZINT + ANINT((XMAX_WORK(JI)-ZINT)*XPREC)/XPREC

    ZINT = AINT(XSTD_WORK(JI))
    IF (XSTD_WORK(JI)/=ZINT) &      
    XSTD_WORK(JI) = ZINT + ANINT((XSTD_WORK(JI)-ZINT)*XPREC)/XPREC

    ZINT = AINT(XSKEW_WORK(JI))
    IF (XSKEW_WORK(JI)/=ZINT) &      
    XSKEW_WORK(JI) = ZINT + ANINT((XSKEW_WORK(JI)-ZINT)*XPREC)/XPREC
!
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_CTI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE2_CTI
