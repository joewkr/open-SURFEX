!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ####################
      SUBROUTINE TOPD_TO_ISBA_SLOPE (PSSO_SLOPE, KI)
!     ####################
!
!!****  *TOPD_TO_ISBA*  
!!
!!    PURPOSE
!!    -------
!    
!     
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    
!!    
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Vincendon    * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   12/11/2012
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TOPODYN,       ONLY : NNCAT, NNMC, XTANB
USE MODD_COUPLING_TOPD, ONLY : NMASKT,NNPIX
USE MODD_SURF_PAR,        ONLY : NUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(INOUT) :: PSSO_SLOPE
!
INTEGER, INTENT(IN)                 :: KI      ! Grid dimensions
!
!*      0.2    declarations of local variables
!
!
INTEGER                :: JCAT,JPIX,JJ          ! loop control 
REAL, DIMENSION(KI)    :: ZCOUNT                ! TOPO pixel number in an ISBA pixel
                                                ! on the full grid
REAL, DIMENSION(KI)    :: ZSSO_SLOPE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TOPD_TO_ISBA_SLOPE',0,ZHOOK_HANDLE)
!
!*        1.0  Compute Mean slope over each ISBA_MESH
!            ----------------------------------------------------------------------
!
!write(*,*) 'pente avt topmodel',MINVAL(XSSO_SLOPE),MAXVAL(XSSO_SLOPE),SUM(XSSO_SLOPE,MASK=XSSO_SLOPE/=XUNDEF)
!
ZSSO_SLOPE = PSSO_SLOPE
!
ZCOUNT(:) = REAL(NNPIX(:))

WHERE (ZCOUNT /= 0.0)
   ZSSO_SLOPE = 0.
ENDWHERE
!
DO JCAT=1,NNCAT
  DO JPIX=1,NNMC(JCAT)
    IF (NMASKT(JCAT,JPIX) /= NUNDEF) THEN
      ZSSO_SLOPE(NMASKT(JCAT,JPIX)) = ZSSO_SLOPE(NMASKT(JCAT,JPIX)) + XTANB(JCAT,JPIX)
    ENDIF
  ENDDO
ENDDO
!
WHERE (ZCOUNT /= 0.0)
   ZSSO_SLOPE = ZSSO_SLOPE / ZCOUNT
ENDWHERE
!
PSSO_SLOPE = ZSSO_SLOPE
!
!write(*,*) 'pente apres modification', &
!           MINVAL(XSSO_SLOPE),MAXVAL(XSSO_SLOPE),COUNT(ZCOUNT/=0.0),SUM(XSSO_SLOPE,MASK=XSSO_SLOPE/=XUNDEF)
!
IF (LHOOK) CALL DR_HOOK('TOPD_TO_ISBA_SLOPE',1,ZHOOK_HANDLE)
!
END SUBROUTINE TOPD_TO_ISBA_SLOPE
