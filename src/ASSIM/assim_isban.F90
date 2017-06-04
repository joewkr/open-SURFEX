!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE ASSIM_ISBA_n (IM, U, HPROGRAM, KI, &
                        PCON_RAIN, PSTRAT_RAIN, PCON_SNOW, PSTRAT_SNOW,&
                        PCLOUDS,   PLSM,        PEVAPTR,   PEVAP,      &
                        PSWEC,     PTSC,        PUCLS,     PVCLS,      &
                        PTS,       PT2M,        PHU2M,     PSWE,       &
                        HTEST,     OD_MASKEXT,  PLON_IN,   PLAT_IN )

!     ###############################################################################
!
!!****  *ASSIM_ISBA_n * - Chooses the surface assimilation schemes for ISBA
!!
!!    PURPOSE
!!    -------
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
!!     T. Aspelien
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012
!!      Trygve Aspelien, Separating IO  06/2013
!!--------------------------------------------------------------------
!
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_PE_t
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ASSIM,          ONLY : CASSIM_ISBA,LAESNM,LEXTRAP_NATURE,NPRINTLEV
!
!
USE YOMHOOK,             ONLY : LHOOK,   DR_HOOK
USE PARKIND1,            ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_OI_HOR_EXTRAPOL_SURF
USE MODI_ASSIM_ISBA_UPDATE_SNOW
USE MODI_ASSIM_NATURE_ISBA_EKF
USE MODI_ASSIM_NATURE_ISBA_ENKF
USE MODI_ASSIM_NATURE_ISBA_OI
USE MODI_AVERAGE_DIAG_MISC_ISBA_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),    INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN) :: KI
REAL, DIMENSION(KI), INTENT(IN) :: PCON_RAIN
REAL, DIMENSION(KI), INTENT(IN) :: PSTRAT_RAIN
REAL, DIMENSION(KI), INTENT(IN) :: PCON_SNOW
REAL, DIMENSION(KI), INTENT(IN) :: PSTRAT_SNOW
REAL, DIMENSION(KI), INTENT(IN) :: PCLOUDS
REAL, DIMENSION(KI), INTENT(IN) :: PLSM
REAL, DIMENSION(KI), INTENT(IN) :: PEVAPTR
REAL, DIMENSION(KI), INTENT(IN) :: PEVAP
REAL, DIMENSION(KI), INTENT(IN) :: PSWEC
REAL, DIMENSION(KI), INTENT(IN) :: PTSC
REAL, DIMENSION(KI), INTENT(IN) :: PUCLS
REAL, DIMENSION(KI), INTENT(IN) :: PVCLS
REAL, DIMENSION(KI), INTENT(IN) :: PTS
REAL, DIMENSION(KI), INTENT(IN) :: PT2M
REAL, DIMENSION(KI), INTENT(IN) :: PHU2M
REAL, DIMENSION(KI), INTENT(IN) :: PSWE
CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
LOGICAL,  DIMENSION (KI), INTENT(IN) ::  OD_MASKEXT
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLON_IN
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLAT_IN
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
LOGICAL, DIMENSION(:), ALLOCATABLE :: GINTERP_NATURE
LOGICAL, DIMENSION(:), ALLOCATABLE :: GINTERP_SN
REAL,    DIMENSION(:), ALLOCATABLE :: ZTS_EP,ZTS_EP0
REAL,    DIMENSION(:), ALLOCATABLE :: ZTP_EP,ZTP_EP0
REAL,    DIMENSION(:), ALLOCATABLE :: ZWS_EP,ZWS_EP0
REAL,    DIMENSION(:), ALLOCATABLE :: ZWP_EP,ZWP_EP0
REAL,    DIMENSION(:), ALLOCATABLE :: ZTL_EP,ZTL_EP0
REAL,    DIMENSION(:), ALLOCATABLE :: ZSWE_EP,ZSWE_EP0
REAL,    DIMENSION(:), ALLOCATABLE :: ZSNR_EP,ZSNR_EP0
REAL,    DIMENSION(:), ALLOCATABLE :: ZSNA_EP,ZSNA_EP0
REAL,    DIMENSION(KI) :: ZSWE
REAL,    DIMENSION(KI) :: ZSWE_ORIG
INTEGER :: JI,JL,JP, IMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('ASSIM_ISBA_N',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_ISBA_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
ZSWE = PSWE
!
! General snow update
IF ( CASSIM_ISBA /= 'OI   ' ) THEN
  !
  ! Snow analysis/update
  IF (LAESNM) THEN
    IF (NRANK==NPIO) WRITE(*,*) 'UPDATE SNOW FROM ANALYSED VALUES'
    CALL ASSIM_ISBA_UPDATE_SNOW(IM%O, IM%NP, IM%NPE, HPROGRAM, KI, &
                                ZSWE, ZSWE_ORIG, .TRUE., .TRUE., HTEST)
  ELSE
    IF (NRANK==NPIO) WRITE(*,*) 'SNOW IS NOT UPDATED FROM ANALYSED VALUES'
  ENDIF
ENDIF
!
! Soil assimilation
IF ( CASSIM_ISBA == 'EKF  ' ) THEN
  !
  ! Run EKF for soil
  CALL ASSIM_NATURE_ISBA_EKF(IM%O, IM%S, IM%K, IM%NP, IM%NPE, HPROGRAM, KI, PT2M, PHU2M, HTEST)
  !
ELSEIF ( CASSIM_ISBA == 'ENKF ') THEN
  !
  CALL ASSIM_NATURE_ISBA_ENKF(IM%O, IM%S, IM%K, IM%NP, IM%NPE, HPROGRAM, KI, PT2M, PHU2M, HTEST)
  !  
ELSEIF ( CASSIM_ISBA == 'OI   ' ) THEN
  !
  ! Snow analysis/update. Store the original field in the surfex file
  IF (LAESNM) THEN
    IF (NRANK==NPIO) WRITE(*,*) 'UPDATE SNOW FROM ANALYSED VALUES'
    CALL ASSIM_ISBA_UPDATE_SNOW(IM%O, IM%NP, IM%NPE, HPROGRAM, KI, &
                                ZSWE, ZSWE_ORIG, .TRUE., .FALSE., HTEST)
  ELSE
    IF (NRANK==NPIO) WRITE(*,*) 'SNOW IS NOT UPDATED FROM ANALYSED VALUES'
  ENDIF
  !
  ! Run OI for soil
  CALL ASSIM_NATURE_ISBA_OI(IM%O, IM%S, IM%K, IM%NP, IM%NPE, HPROGRAM, KI, &
                            PCON_RAIN, PSTRAT_RAIN, PCON_SNOW, PSTRAT_SNOW,&
                            PCLOUDS,   PLSM,        PEVAPTR,   PEVAP,      &
                            PSWEC,     PTSC,        PUCLS,     PVCLS,      &
                            PTS,       PT2M,        PHU2M,     ZSWE,       &
                            HTEST,     OD_MASKEXT,  PLON_IN,   PLAT_IN )
  !
  ! Snow analysis/update (changed in oi_cacsts). Get the full increment
  IF (LAESNM) THEN
    IF (NRANK==NPIO) WRITE(*,*) 'UPDATE SNOW FROM ANALYSED OI_CACSTS VALUES'
    CALL ASSIM_ISBA_UPDATE_SNOW(IM%O, IM%NP, IM%NPE, HPROGRAM, KI, &
                                ZSWE, ZSWE_ORIG, .FALSE., .TRUE., HTEST)
  ELSE
    IF (NRANK==NPIO) WRITE(*,*) 'SNOW IS NOT UPDATED FROM ANALYSED OI_CACSTS VALUES'
  ENDIF
  !
ELSE
  CALL ABOR1_SFX(CASSIM_ISBA//' is not a defined scheme for ASSIM_ISBA_N')
ENDIF

! Set snow layers and patches
JP = 1
PK => IM%NP%AL(JP)
PEK => IM%NPE%AL(JP)
JL = 1
!
! Extrapolation if requested
IF ( LEXTRAP_NATURE ) THEN
  !
  ALLOCATE(ZWS_EP(KI),ZWP_EP(KI),ZTS_EP(KI),ZTP_EP(KI),&
           ZTL_EP(KI),ZSWE_EP(KI),ZSNR_EP(KI),ZSNA_EP(KI))
  !
  ZWS_EP(:) = XUNDEF
  ZWP_EP(:) = XUNDEF
  ZTS_EP(:) = XUNDEF
  ZTP_EP(:) = XUNDEF
  ZTL_EP(:) = XUNDEF
  ZSWE_EP(:) = XUNDEF
  ZSNR_EP(:) = XUNDEF
  ZSNA_EP(:) = XUNDEF
  DO JI = 1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)
    ZWS_EP(IMASK)  = PEK%XWG(JI,1)
    ZWP_EP(IMASK)  = PEK%XWG(JI,2)
    ZTS_EP(IMASK)  = PEK%XTG(JI,1)
    ZTP_EP(IMASK)  = PEK%XTG(JI,2)
    ZTL_EP(IMASK)  = PEK%XWGI(JI,2)
    ZSWE_EP(IMASK) = PEK%TSNOW%WSNOW(JI,JL)
    ZSNR_EP(IMASK) = PEK%TSNOW%RHO  (JI,JL)
    ZSNA_EP(IMASK) = PEK%TSNOW%ALB  (JI)
  ENDDO
  !
  ALLOCATE(GINTERP_NATURE(KI),GINTERP_SN(KI))
  !
  ! Search for the nearest grid point values for land surface fields
  ! at locations where the CANARI land fraction is less than 50%
  ! and therefore useless values MIGTH be given
  GINTERP_NATURE = .FALSE.
  GINTERP_SN     = .FALSE.
  !
  ! Snow albedo and density are also extrapolated in points 
  ! which get initial snow in the snow analysis
  WHERE ( ZSWE_EP(:) < 1.0E-10 .AND. PSWE(:)>= 1.0E-10 )
    GINTERP_SN(:) = .TRUE.
    ZSNA_EP(:)    = XUNDEF
    ZSNR_EP(:)    = XUNDEF
  END WHERE
  ZSWE_EP(:) = PSWE(:)
  !
  WHERE ( PLSM(:) < 0.5 )
    GINTERP_NATURE(:) = .TRUE.
    GINTERP_SN(:) = .TRUE.
    ZTS_EP(:)     = XUNDEF
    ZTP_EP(:)     = XUNDEF
    ZWS_EP(:)     = XUNDEF
    ZWP_EP(:)     = XUNDEF
    ZTL_EP(:)     = XUNDEF
    ZSWE_EP(:)    = XUNDEF
    ZSNA_EP(:)    = XUNDEF
    ZSNR_EP(:)    = XUNDEF
  END WHERE
  !
  ALLOCATE(ZWS_EP0(KI),ZWP_EP0(KI),ZTS_EP0(KI),ZTP_EP0(KI),&
           ZTL_EP0(KI),ZSWE_EP0(KI),ZSNR_EP0(KI),ZSNA_EP0(KI))
  !
  ZWS_EP0(:) = ZWS_EP(:)
  CALL OI_HOR_EXTRAPOL_SURF(KI,IM%G%XLAT,IM%G%XLON,ZWS_EP0,IM%G%XLAT,IM%G%XLON,ZWS_EP,GINTERP_NATURE)
  ZWP_EP0(:) = ZWP_EP(:)
  CALL OI_HOR_EXTRAPOL_SURF(KI,IM%G%XLAT,IM%G%XLON,ZWP_EP0,IM%G%XLAT,IM%G%XLON,ZWP_EP,GINTERP_NATURE)
  ZTS_EP0(:) = ZTS_EP(:)
  CALL OI_HOR_EXTRAPOL_SURF(KI,IM%G%XLAT,IM%G%XLON,ZTS_EP0,IM%G%XLAT,IM%G%XLON,ZTS_EP,GINTERP_NATURE,IM%S%XZS)
  ZTP_EP0(:) = ZTP_EP(:)
  CALL OI_HOR_EXTRAPOL_SURF(KI,IM%G%XLAT,IM%G%XLON,ZTP_EP0,IM%G%XLAT,IM%G%XLON,ZTP_EP,GINTERP_NATURE,IM%S%XZS)
  ZTL_EP0(:) = ZTL_EP(:)
  CALL OI_HOR_EXTRAPOL_SURF(KI,IM%G%XLAT,IM%G%XLON,ZTL_EP0,IM%G%XLAT,IM%G%XLON,ZTL_EP,GINTERP_NATURE)
  ZSWE_EP0(:) = ZSWE_EP(:)
  CALL OI_HOR_EXTRAPOL_SURF(KI,IM%G%XLAT,IM%G%XLON,ZSWE_EP0,IM%G%XLAT,IM%G%XLON,ZSWE_EP,GINTERP_SN)
  ZSNR_EP0(:) = ZSNR_EP(:)
  CALL OI_HOR_EXTRAPOL_SURF(KI,IM%G%XLAT,IM%G%XLON,ZSNR_EP0,IM%G%XLAT,IM%G%XLON,ZSNR_EP,GINTERP_SN)
  ZSNA_EP0(:) = ZSNA_EP(:)
  CALL OI_HOR_EXTRAPOL_SURF(KI,IM%G%XLAT,IM%G%XLON,ZSNA_EP0,IM%G%XLAT,IM%G%XLON,ZSNA_EP,GINTERP_SN)  
  !
  DEALLOCATE(ZWS_EP0,ZWP_EP0,ZTS_EP0,ZTP_EP0,ZTL_EP0,ZSWE_EP0,ZSNR_EP0,ZSNA_EP0)
  !
  ! PRINT values produced by OI_HO_EXTRAPOL_SURF for TS
  IF ( NPRINTLEV > 2 ) THEN
    DO JI=1,KI
     IF (GINTERP_NATURE(JI)) THEN
       PRINT *,'Surface temperature set to ',ZTS_EP(JI),'from nearest neighbour at I=',U%NR_NATURE(JI)
     ENDIF
    ENDDO
  ENDIF
  !
  DEALLOCATE(GINTERP_NATURE,GINTERP_SN)
  !
  DO JI = 1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)  
    ! Set extrpolated fields to global
    PEK%XWG (JI,1) = ZWS_EP(IMASK)
    PEK%XWG (JI,2) = ZWP_EP(IMASK)
    PEK%XTG (JI,1) = ZTS_EP(IMASK)
    PEK%XTG (JI,2) = ZTP_EP(IMASK)
    PEK%XWGI(JI,2) = ZTL_EP(IMASK)
    PEK%TSNOW%WSNOW(JI,JL) = ZSWE_EP(IMASK)
    PEK%TSNOW%RHO  (JI,JL) = ZSNR_EP(IMASK)
    PEK%TSNOW%ALB  (JI) = ZSNA_EP(IMASK)
  ENDDO
  !
  DEALLOCATE(ZWS_EP,ZWP_EP,ZTS_EP,ZTP_EP,ZTL_EP,ZSWE_EP,ZSNR_EP,ZSNA_EP)
  !
ENDIF

! Snow analysis/update security
IF (LAESNM) THEN

  ! removes very small values due to computation precision
  WHERE( PEK%TSNOW%WSNOW(:,JL) < 1.0E-10 ) PEK%TSNOW%WSNOW(:,JL) = 0.0

  ! No SNOW
  WHERE ( PEK%TSNOW%WSNOW(:,JL) == 0.0 )
    PEK%TSNOW%RHO(:,JL) = XUNDEF
    PEK%TSNOW%ALB(:)    = XUNDEF
  END WHERE
  !
ENDIF
!
!to be improved later - needed for surfex course
 CALL AVERAGE_DIAG_MISC_ISBA_n(IM%ID%DM, IM%ID%NDM, IM%O, IM%NP, IM%NPE)
 !
IF (LHOOK) CALL DR_HOOK('ASSIM_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_ISBA_n
