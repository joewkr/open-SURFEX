!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE COMPUT_COLD_LAYERS_THICK(PDG,PTG,PALT,PFLT)
!     ###############################################################################
!
!!****  *COMPUT_COLD_LAYERS_THICK* - additional diagnostics for ISBA
!!
!!    PURPOSE
!!    -------
!! Comput active layer (ALT) and frozen layer (FLT) theaknesses 
!! using linear interpolation between two nodes :
!!       ALT = depth to zero centigrade isotherm in permafrost
!!       FLT = depth to zero centigrade isotherm in non-permafrost
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2014
!!
!!------------------------------------------------------------------
!
USE MODD_CSTS,     ONLY : XTT
USE MODD_SURF_PAR, ONLY : NUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:),  INTENT(IN)  :: PDG           ! soil layer depth
REAL, DIMENSION(:,:),  INTENT(IN)  :: PTG           ! soil temperature
REAL, DIMENSION(:),    INTENT(OUT) :: PALT          ! active layer theakness
REAL, DIMENSION(:),    INTENT(OUT) :: PFLT          ! frozen layer theakness
!
!*      0.2    declarations of local variables
!
REAL,    DIMENSION(SIZE(PDG,1),SIZE(PDG,2)) :: ZNODE
INTEGER, DIMENSION(SIZE(PDG,1))             :: IUP_ALT, IDOWN_ALT
INTEGER, DIMENSION(SIZE(PDG,1))             :: IUP_FLT, IDOWN_FLT
!
REAL    :: ZTG_UP, ZTG_DOWN
REAL    :: ZUP, ZDOWN
REAL    :: ZALT, ZFLT, ZSLOPE
!
INTEGER :: JI, JL, INI, INL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COMPUT_COLD_LAYERS_THICK',0,ZHOOK_HANDLE)
!
INI=SIZE(PDG,1)
INL=SIZE(PDG,2)
!
IUP_ALT  (:)=0
IDOWN_ALT(:)=0
IUP_FLT  (:)=0
IDOWN_FLT(:)=0
!
!Surface soil layer
!
ZNODE(:,1)=0.5*PDG(:,1)
WHERE(PTG(:,1)>XTT.AND.PTG(:,2)<=XTT.AND.PTG(:,3)<=XTT)
      IUP_ALT  (:)=1
      IDOWN_ALT(:)=2
ENDWHERE
WHERE(PTG(:,1)<XTT.AND.PTG(:,2)>=XTT.AND.PTG(:,3)>=XTT)
      IUP_FLT  (:)=1
      IDOWN_FLT(:)=2
ENDWHERE
!
!Middle soil layer
!
DO JL=2,INL-1
   DO JI=1,INI
      ZNODE(JI,JL)=0.5*(PDG(JI,JL)+PDG(JI,JL-1))
      IF(PTG(JI,JL-1)>XTT.AND.PTG(JI,JL)>XTT.AND.PTG(JI,JL+1)<=XTT)THEN
        IUP_ALT  (JI)=JL
        IDOWN_ALT(JI)=JL+1
      ENDIF
      IF(PTG(JI,JL-1)<XTT.AND.PTG(JI,JL)<XTT.AND.PTG(JI,JL+1)>=XTT)THEN
        IUP_FLT  (JI)=JL
        IDOWN_FLT(JI)=JL+1
      ENDIF      
   ENDDO
ENDDO
!
!Last soil layer
!
ZNODE(:,INL)=0.5*(PDG(:,INL)+PDG(:,INL-1))
WHERE(PTG(:,INL)>XTT)IDOWN_ALT(:)=NUNDEF
WHERE(PTG(:,INL)<XTT)IDOWN_FLT(:)=NUNDEF
!
DO JI=1,INI
!
   PALT(JI)=0.0
   IF(IDOWN_ALT(JI)>0.AND.IDOWN_ALT(JI)<=INL)THEN
     ZTG_UP    = PTG  (JI,IUP_ALT  (JI))
     ZTG_DOWN  = PTG  (JI,IDOWN_ALT(JI))
     ZUP       = ZNODE(JI,IUP_ALT  (JI))
     ZDOWN     = ZNODE(JI,IDOWN_ALT(JI))
     ZSLOPE    = (ZUP-ZDOWN)/(ZTG_UP-ZTG_DOWN)
     PALT(JI)  = ZDOWN+(XTT-ZTG_DOWN)*ZSLOPE
   ENDIF
!
   PFLT(JI)=0.0
   IF(IDOWN_FLT(JI)>0.AND.IDOWN_FLT(JI)<=INL)THEN
     ZTG_UP    = PTG  (JI,IUP_FLT  (JI))
     ZTG_DOWN  = PTG  (JI,IDOWN_FLT(JI))
     ZUP       = ZNODE(JI,IUP_FLT  (JI))
     ZDOWN     = ZNODE(JI,IDOWN_FLT(JI))
     ZSLOPE    = (ZUP-ZDOWN)/(ZTG_UP-ZTG_DOWN)
     PFLT(JI)  = ZDOWN+(XTT-ZTG_DOWN)*ZSLOPE
   ENDIF
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('COMPUT_COLD_LAYERS_THICK',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COMPUT_COLD_LAYERS_THICK

