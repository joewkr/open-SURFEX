!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_OCEAN_n (DGO, O, PSEABATHY) 
!     ###############################################################################
!
!!****  *DIAG_INLINE_SEAFLUX_n * - computes diagnostics during SEAFLUX time-step
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2008
!!      Modified    07/2012, P. Le Moigne : CMO1D phasing
!!------------------------------------------------------------------
!
!
USE MODD_DIAG_OCEAN_n, ONLY : DIAG_OCEAN_t
USE MODD_OCEAN_n, ONLY : OCEAN_t
!
USE MODD_OCEAN_CSTS
!
USE MODD_OCEAN_GRID
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!
TYPE(DIAG_OCEAN_t), INTENT(INOUT) :: DGO
TYPE(OCEAN_t), INTENT(INOUT) :: O
REAL, DIMENSION(:), INTENT(IN) :: PSEABATHY
!
REAL, DIMENSION(SIZE(O%XSEAT(:,1)),NOCKMIN:NOCKMAX) :: ZSEADENS
REAL, DIMENSION(SIZE(O%XSEAT(:,1))) :: ZRHO0,ZRHOCMO,ZDRHOX,ZTCMO
REAL, DIMENSION(SIZE(O%XSEAT(:,1))) :: ZTMOY,ZSMOY,ZUMOY,ZVMOY,ZDMOY
INTEGER, DIMENSION(SIZE(O%XSEAT(:,1))) :: IHMOLEVEL
INTEGER :: INBPTS, IK1, IK2
INTEGER :: J,JPT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_OCEAN_N',0,ZHOOK_HANDLE)
IF (DGO%LDIAG_OCEAN) THEN
!
   IK1=NOCKMIN+1
   IK2=NOCKMIN+2
!
   INBPTS=SIZE(O%XSEAT(:,1))
!
   ZSEADENS(:,:)=XRHOSWREF + &
          (O%XSEAT(:,:)-13.5)*(-0.19494-0.49038E-2*(O%XSEAT(:,:)-13.5))&
          +0.77475*(O%XSEAS(:,:)-32.6)  
!
   DO JPT=1,INBPTS
!
!       3.     Oceanic mixed layer depth calculation
!
!              -------------------------------------
     O%XSEAHMO(JPT)=-XZHOC(NOCKMIN+2)
     ZTCMO(JPT)=O%XSEAT(JPT,IK1)-0.5
     ZRHOCMO(JPT)=XRHOSWREF+DRHOCOMPUTE(ZTCMO(JPT),O%XSEAS(JPT,IK1))
     DO J=IK2,NOCKMAX-1
         ZRHO0(JPT)=XRHOSWREF+DRHOCOMPUTE(O%XSEAT(JPT,J),O%XSEAS(JPT,J))
         ZDRHOX(JPT)=ZRHO0(JPT)-ZRHOCMO(JPT)
         IF ((ZDRHOX(JPT)<0.).AND.(O%XSEABATH(JPT,J)/=0.))  THEN 
           O%XSEAHMO(JPT)=O%XSEAHMO(JPT)+XDZ1(J)
         ELSE 
           EXIT
         ENDIF
     ENDDO
     O%XSEAHMO(JPT)=MIN(O%XSEAHMO(JPT),-PSEABATHY(JPT))
!
     IHMOLEVEL(JPT)=NOCKMAX
!
     DO J=IK1,NOCKMAX
       IF (-XZHOC(J)>O%XSEAHMO(JPT)) THEN
         IHMOLEVEL(JPT)=J-1
         EXIT
       ENDIF
     ENDDO
!
!       4.     Thermal and haline contents verification
!              ----------------------------------------
     IF (IHMOLEVEL(JPT)<=1) THEN
       DGO%XTOCMOY(JPT)=O%XSEAT(JPT,IK1)
       DGO%XSOCMOY(JPT)=O%XSEAS(JPT,IK1)
       DGO%XUOCMOY(JPT)=O%XSEAU(JPT,IK1)
       DGO%XVOCMOY(JPT)=O%XSEAV(JPT,IK1)
       DGO%XDOCMOY(JPT)=ZSEADENS(JPT,IK1)
     ELSE !IHMOLEVEL>=2
       ZTMOY(JPT)=O%XSEAT(JPT,IK1)
       ZSMOY(JPT)=O%XSEAS(JPT,IK1)
       ZUMOY(JPT)=O%XSEAU(JPT,IK1)
       ZVMOY(JPT)=O%XSEAV(JPT,IK1)
       ZDMOY(JPT)=ZSEADENS(JPT,IK1)
       DO J=IK2,IHMOLEVEL(JPT)
         ZTMOY(JPT)=ZTMOY(JPT)+O%XSEAT(JPT,J)*(-XZHOC(J)+XZHOC(J-1))
         ZSMOY(JPT)=ZSMOY(JPT)+O%XSEAS(JPT,J)*(-XZHOC(J)+XZHOC(J-1))
         ZUMOY(JPT)=ZUMOY(JPT)+O%XSEAU(JPT,J)*(-XZHOC(J)+XZHOC(J-1))
         ZVMOY(JPT)=ZVMOY(JPT)+O%XSEAV(JPT,J)*(-XZHOC(J)+XZHOC(J-1))
         ZDMOY(JPT)=ZDMOY(JPT)+ZSEADENS(JPT,J)*(-XZHOC(J)+XZHOC(J-1))
       ENDDO
       DGO%XTOCMOY(JPT)=ZTMOY(JPT)/(-XZHOC(IHMOLEVEL(JPT)))
       DGO%XSOCMOY(JPT)=ZSMOY(JPT)/(-XZHOC(IHMOLEVEL(JPT)))
       DGO%XUOCMOY(JPT)=ZUMOY(JPT)/(-XZHOC(IHMOLEVEL(JPT)))
       DGO%XVOCMOY(JPT)=ZVMOY(JPT)/(-XZHOC(IHMOLEVEL(JPT)))
       DGO%XDOCMOY(JPT)=ZDMOY(JPT)/(-XZHOC(IHMOLEVEL(JPT)))
     ENDIF
!
   ENDDO
!
ENDIF
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_OCEAN_N',1,ZHOOK_HANDLE)
CONTAINS
!
!
!!           #########################################
             FUNCTION DRHOCOMPUTE(T,S) RESULT(R)
!            ##########################################
!
!!****  *DRHOCOMPUTE* 
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!     
!!    REFERENCE
!!    ---------
!!    
!!     
!!    AUTHOR
!!    ------
!!     C. Lebeaupin  *Meteo-France* 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     27/02/2005
!!
!-------------------------------------------------------------------------------
!*       0.     DECLARATIONS
!               ------------
!*       0.1 declaration of arguments and results
!
REAL ::  T,S ! oceanic temperature and salinity
REAL ::  R   ! density
!
!*       0.2 local variables
!
REAL :: DET,DES
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!*       1.     COMPUTE R
!               ---------
!
IF (LHOOK) CALL DR_HOOK('DRHOCOMPUTE',0,ZHOOK_HANDLE)
DET = T-13.5
DES = S-32.6
R = DET*(-0.19494-0.49038E-2*DET)+0.77475*DES
IF (LHOOK) CALL DR_HOOK('DRHOCOMPUTE',1,ZHOOK_HANDLE)
!
END FUNCTION DRHOCOMPUTE
!
END SUBROUTINE DIAG_INLINE_OCEAN_n
