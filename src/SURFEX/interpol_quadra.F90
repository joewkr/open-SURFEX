!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INTERPOL_QUADRA(PDAT,PNDAT,PVAL0,PVAL1,PVAL2,POUT)
!     #############################################################
!
!!**** *INTERPOL_QUADRA* Quadractic interpolation between 3 month, especially
!!                       relevant to conserv the SST (or other) monthly mean value.
!!
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
!!    Original    08/2009
!!    18-11-2010 by F. Chauvin  : bugfix for temporal interpolation coeff.
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
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
REAL,                    INTENT(IN)    :: PDAT    ! Present date in the current month
REAL,                    INTENT(IN)    :: PNDAT   ! Number of date in the current month
REAL, DIMENSION(:),      INTENT(IN)    :: PVAL0   ! Value of the precedent month
REAL, DIMENSION(:),      INTENT(IN)    :: PVAL1   ! Value of the current month
REAL, DIMENSION(:),      INTENT(IN)    :: PVAL2   ! Value of the next month
REAL, DIMENSION(:),      INTENT(OUT)   :: POUT    ! Interpolated value
!
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(SIZE(PVAL0)) :: ZMID1   ! Mid point between t-1 and t
REAL, DIMENSION(SIZE(PVAL0)) :: ZMID2   ! Mid point between t+1 and t
REAL, DIMENSION(SIZE(PVAL0)) :: ZA      ! Interpolation coef
REAL, DIMENSION(SIZE(PVAL0)) :: ZB      ! Interpolation coef
REAL, DIMENSION(SIZE(PVAL0)) :: ZC      ! Interpolation coef
!
REAL                         :: ZSCARRE ! Quadratic coef
REAL                         :: ZSUM    ! Quadratic coef
!
INTEGER                      :: JDAT, INDAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_QUADRA',0,ZHOOK_HANDLE)
!
!*    1.     Initialization
!            --------------
!
ZSCARRE = 0.0
ZSUM    = 0.0
!
INDAT = INT(PNDAT)
!
DO JDAT=1,INDAT
   ZSCARRE = ZSCARRE + REAL(JDAT*JDAT)
   ZSUM    = ZSUM    + REAL(JDAT)
ENDDO
!
!*    2.     Mid points
!            ----------
! 
ZMID1(:) = 0.5 * (PVAL1(:)+PVAL0(:))
ZMID2(:) = 0.5 * (PVAL1(:)+PVAL2(:))
!
!
!*    3.     Coef calculation
!            ----------------
!
ZA(:) = ((PVAL1(:)-ZMID1(:))*PNDAT - (ZMID2(:)-ZMID1(:))*(ZSUM-PNDAT)/PNDAT) &
      / ((ZSCARRE-PNDAT)-(ZSUM-PNDAT)*(PNDAT+2.0))
!
ZB(:) = ((ZMID2(:)-ZMID1(:)) - (PNDAT*(PNDAT+2.0) * ZA(:))) / PNDAT
!
ZC(:) = ZMID1(:) - ZA(:) - ZB(:)
!
!*    3.     Final calculation
!            -----------------
!
POUT(:) = ZA(:) * PDAT**2 + ZB(:) * PDAT + ZC(:)
!
!*    4.     End
!            ---
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_QUADRA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_QUADRA
