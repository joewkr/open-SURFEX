!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
!     ##########################
      SUBROUTINE TOPD_TO_DF (IO, NK, NP, NPE, PWG)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!     This routines updates the soil water content of ISBA DIF afeter TOPODYN
!     lateral distribution  
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
!!    REFERENCE
!!    ---------
!!     
!!    AUTHOR
!!    ------
!!
!!       ELYAZIDI/HEYMES/RISTOR * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original  02/2011 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_NP_t, ISBA_NPE_t, ISBA_NK_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_SURF_PAR,      ONLY : XUNDEF, NUNDEF
USE MODD_COUPLING_TOPD, ONLY : XTOTBV_IN_MESH, XFRAC_D3
USE MODD_ISBA_PAR,      ONLY : XWGMIN
!
USE YOMHOOK   ,         ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,         ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
 REAL, DIMENSION(:,:), INTENT(IN) :: PWG
!      
!*      0.2    declarations of local variables
!
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
REAL                              :: ZWORK          ! numbers of layers in root and deep zones
INTEGER                           :: IDEPTH, IMASK
INTEGER                           :: JI, JL, JP ! loop indexes
REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TOPD_TO_DF',0,ZHOOK_HANDLE)
!
DO JP=1,IO%NPATCH

  KK => NK%AL(JP)
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)

  IF (PK%NSIZE_P == 0 ) CYCLE

  DO JL = 1,IO%NGROUND_LAYER

    DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)

      IDEPTH=PK%NWG_LAYER(JI)

      IF(JL<=IDEPTH.AND.IDEPTH/=NUNDEF.AND.(XTOTBV_IN_MESH(IMASK)/=0.0).AND.(XTOTBV_IN_MESH(IMASK)/=XUNDEF)) THEN

      ! root layers
      IF (PK%XDZG(JI,JL)/=XUNDEF.AND.PK%XDG2(JI)/=XUNDEF.AND.PK%XDG(JI,JL)/=XUNDEF) THEN 
        ZWORK=MIN(PK%XDZG(JI,JL),MAX(0.0,PK%XDG2(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))
      ENDIF

      IF ((PWG(IMASK,2)/=XUNDEF).AND.(ZWORK>0.).AND.(ZWORK/=XUNDEF)) THEN
        PEK%XWG(JI,JL)=MIN(MAX(PWG(IMASK,2),XWGMIN),KK%XWSAT(JI,JL)) 
      ENDIF

      ! deep layers
      IF ((XFRAC_D3(IMASK)/=0.0).AND.(XFRAC_D3(IMASK)/=XUNDEF)) THEN     

        IF (PK%XDZG(JI,JL)/=XUNDEF.AND.PK%XDG2(JI)/=XUNDEF.AND.PK%XDG(JI,JL)/=XUNDEF) THEN
          ZWORK=MIN(PK%XDZG(JI,JL),MAX(0.0,PK%XDG(JI,JL)-PK%XDG2(JI)))
        ENDIF

        IF ((PWG(IMASK,3)/=XUNDEF).AND.(ZWORK>0.).AND.(ZWORK/=XUNDEF)) THEN
          PEK%XWG(JI,JL)=MIN(MAX(PWG(IMASK,3),XWGMIN),KK%XWSAT(JI,JL))
        ENDIF

      ENDIF

    ENDIF

  ENDDO
 ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('TOPD_TO_DF',1,ZHOOK_HANDLE)

END SUBROUTINE TOPD_TO_DF


