!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ALLOCATE_GR_SNOW(TPSNOW,KLU)
!     ##############################################
!
!!****  *ALLOCATE_GR_SNOW* - 
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!       TPSNOW%SCHEME must yet be initialized
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
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!      
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/01/99
!
!!     F.Solmon     06/00 Adapt for patch cases 
!!     V. Masson    01/2004 Externalization
!!     A. Bogatchev 09/2005 EBA snow option
!!     P. Samuelsson 07/2014 Added snow albedos
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR,    ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
TYPE(SURF_SNOW), INTENT(INOUT)             :: TPSNOW
INTEGER, INTENT(IN)                        :: KLU
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_GR_SNOW',0,ZHOOK_HANDLE)
!
IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO' .OR. TPSNOW%SCHEME=='1-L' .OR.  &
    TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA') THEN
  !
  ALLOCATE(TPSNOW%WSNOW  (KLU,TPSNOW%NLAYER))
  ALLOCATE(TPSNOW%RHO    (KLU,TPSNOW%NLAYER))
  ALLOCATE(TPSNOW%ALB    (KLU))  
  ALLOCATE(TPSNOW%ALBVIS (KLU))  
  ALLOCATE(TPSNOW%ALBNIR (KLU))  
  ALLOCATE(TPSNOW%ALBFIR (KLU))  
  TPSNOW%WSNOW   = 0.
  TPSNOW%RHO     = XUNDEF  
  TPSNOW%ALB     = XUNDEF
  TPSNOW%ALBVIS  = XUNDEF
  TPSNOW%ALBNIR  = XUNDEF
  TPSNOW%ALBFIR  = XUNDEF
  !
  IF (TPSNOW%SCHEME/='D95' .AND. TPSNOW%SCHEME/='EBA') THEN
    !
    ALLOCATE(TPSNOW%EMIS(KLU))
    ALLOCATE(TPSNOW%TS  (KLU))
    TPSNOW%EMIS = XUNDEF
    TPSNOW%TS   = XUNDEF
    !
    IF (TPSNOW%SCHEME/='1-L') THEN
      !
      ALLOCATE(TPSNOW%TEMP(KLU,TPSNOW%NLAYER))
      ALLOCATE(TPSNOW%HEAT(KLU,TPSNOW%NLAYER))
      ALLOCATE(TPSNOW%AGE (KLU,TPSNOW%NLAYER)) 
      TPSNOW%TEMP = XUNDEF
      TPSNOW%HEAT = XUNDEF
      TPSNOW%AGE  = XUNDEF
      !
      IF(TPSNOW%SCHEME=='CRO') THEN
        !
        ALLOCATE(TPSNOW%GRAN1(KLU,TPSNOW%NLAYER))
        ALLOCATE(TPSNOW%GRAN2(KLU,TPSNOW%NLAYER))  
        ALLOCATE(TPSNOW%HIST (KLU,TPSNOW%NLAYER))  
        TPSNOW%GRAN1 = XUNDEF
        TPSNOW%GRAN2 = XUNDEF  
        TPSNOW%HIST  = XUNDEF  
        !
      END IF
      !
    ELSE
      !
      ALLOCATE(TPSNOW%T(KLU,TPSNOW%NLAYER))
      TPSNOW%T = XUNDEF
      !
    END IF
  ENDIF
ENDIF
!
!
IF (TPSNOW%SCHEME/='CRO') THEN
  !
  ALLOCATE(TPSNOW%GRAN1(0,0))
  ALLOCATE(TPSNOW%GRAN2(0,0))  
  ALLOCATE(TPSNOW%HIST (0,0))  
  !
  IF (TPSNOW%SCHEME/='3-L') THEN
    !
    ALLOCATE(TPSNOW%TEMP(0,0))    
    ALLOCATE(TPSNOW%HEAT(0,0))
    ALLOCATE(TPSNOW%AGE (0,0))
    !
    IF (TPSNOW%SCHEME/='1-L') THEN
      !
      ALLOCATE(TPSNOW%EMIS (0))
      ALLOCATE(TPSNOW%TS   (0))
      !
      IF (TPSNOW%SCHEME/='D95' .AND. TPSNOW%SCHEME/='EBA') THEN
        !
        ALLOCATE(TPSNOW%WSNOW  (0,0))
        ALLOCATE(TPSNOW%RHO    (0,0))
        ALLOCATE(TPSNOW%ALB    (0))
        ALLOCATE(TPSNOW%ALBVIS (0))
        ALLOCATE(TPSNOW%ALBNIR (0))
        ALLOCATE(TPSNOW%ALBFIR (0))
        !
      ENDIF
      !
    ENDIF
    !
  ENDIF
  ! 
END IF
!
IF (TPSNOW%SCHEME/='1-L') THEN
  !
  ALLOCATE(TPSNOW%T(0,0))
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_GR_SNOW',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE ALLOCATE_GR_SNOW
