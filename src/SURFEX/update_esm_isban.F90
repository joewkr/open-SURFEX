!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE UPDATE_ESM_ISBA_n (IO, S, K, NK, NP, NPE, KI,KSW,PZENITH,PSW_BANDS,PDIR_ALB,& 
                                   PSCA_ALB,PEMIS,PTSRAD,PTSURF      )
!     ################################################################
!
!!****  *UPDATE_ESM_ISBA_n* - update ISBA radiative and physical properties in Earth System Model 
!!                            after the call to OASIS coupler in order 
!!                            to close the energy budget between radiative scheme and surfex
!!
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!      B. Decharme 06/2013 new coupling variables
!!      P. Samuelsson 10/2014 MEB
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t, ISBA_NK_t, &
                        ISBA_NP_t, ISBA_NPE_t
!
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODI_AVERAGE_RAD
USE MODI_AVERAGE_TSURF
USE MODI_UPDATE_RAD_ISBA_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSW       ! number of short-wave spectral bands
!
REAL,             DIMENSION(KI),    INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS ! short-wave spectral bands
!
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),    INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSRAD    ! radiative temperature
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
REAL, DIMENSION(KI,KSW,IO%NPATCH) :: ZDIR_ALB_PATCH
REAL, DIMENSION(KI,KSW,IO%NPATCH) :: ZSCA_ALB_PATCH
REAL, DIMENSION(KI,IO%NPATCH)     :: ZEMIS_PATCH
REAL, DIMENSION(KI,IO%NPATCH)     :: ZTSRAD_PATCH
REAL, DIMENSION(KI,IO%NPATCH)     :: ZTSURF_PATCH
REAL, DIMENSION(KI,IO%NPATCH)     :: ZEMIS          ! emissivity with flood
!
LOGICAL :: LEXPLICIT_SNOW ! snow scheme key
!
INTEGER :: IMASK, JI, JP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     Defaults
!               --------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_ISBA_N',0,ZHOOK_HANDLE)
!
ZDIR_ALB_PATCH(:,:,:) = 0.0
ZSCA_ALB_PATCH(:,:,:) = 0.0
ZEMIS_PATCH   (:,:  ) = 0.0
!
LEXPLICIT_SNOW = (NPE%AL(1)%TSNOW%SCHEME=='3-L'.OR.NPE%AL(1)%TSNOW%SCHEME=='CRO')
!
!
!
!*       2.     Update nature albedo and emissivity
!               -----------------------------------
!
ZEMIS(:,:) = 0.0
ZTSRAD_PATCH(:,:) = 0.0
ZTSURF_PATCH(:,:) = 0.0
!
DO JP = 1,IO%NPATCH
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)
  KK => NK%AL(JP)

  CALL UPDATE_RAD_ISBA_n(IO, S, KK, PK, PEK, JP, PZENITH, PSW_BANDS, &
                        ZDIR_ALB_PATCH(:,:,JP),ZSCA_ALB_PATCH(:,:,JP),ZEMIS_PATCH(:,JP)  )
  !
  !*       3.     radiative surface temperature
  !               -----------------------------
  !
  DO JI = 1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)

    ZEMIS (IMASK,JP) = PEK%XEMIS(JI)

    IF(LEXPLICIT_SNOW.AND.IO%LFLOOD)THEN
      IF (PEK%XPSN(JI)<1.0.AND.PEK%XEMIS(JI)/=XUNDEF) THEN
        ZEMIS(IMASK,JP) = ((1.-KK%XFF(JI)-PEK%XPSN(JI))*PEK%XEMIS(JI) + &
                           KK%XFF(JI)*KK%XEMISF(JI)) / (1.-PEK%XPSN(JI))
      ENDIF
    ENDIF
    !
    ZTSRAD_PATCH (IMASK,JP) = PEK%XTG(JI,1)
    ZTSURF_PATCH (IMASK,JP) = PEK%XTG(JI,1)
    !
    IF(LEXPLICIT_SNOW)THEN
      IF(PEK%XEMIS(JI)/=XUNDEF.AND.ZEMIS_PATCH(IMASK,JP)/=0.) THEN
        ZTSRAD_PATCH(IMASK,JP) = ( ( (1.-PEK%XPSN(JI))*ZEMIS(IMASK,JP)*PEK%XTG(JI,1)**4     &
                               +  PEK%XPSN(JI) *PEK%TSNOW%EMIS(JI)*PEK%TSNOW%TS(JI)**4 )   &
                             / ZEMIS_PATCH(IMASK,JP) )**0.25     
      ENDIF
      ZTSURF_PATCH(IMASK,JP) = PEK%XTG(JI,1)*(1.-PEK%XPSN(JI)) + PEK%TSNOW%TS(JI)*PEK%XPSN(JI)

    ENDIF
    !
  ENDDO    
  !
ENDDO
!
!*       4.     averaged fields
!               ---------------
!
 CALL AVERAGE_RAD(S%XPATCH,                                                   &
                   ZDIR_ALB_PATCH, ZSCA_ALB_PATCH, ZEMIS_PATCH, ZTSRAD_PATCH, &
                   PDIR_ALB,       PSCA_ALB,       S%XEMIS_NAT,   S%XTSRAD_NAT    )  
!
PEMIS = S%XEMIS_NAT
PTSRAD = S%XTSRAD_NAT
!
!* averaged effective temperature
!
!
 CALL AVERAGE_TSURF(S%XPATCH, ZTSURF_PATCH, PTSURF)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_ESM_ISBA_n
