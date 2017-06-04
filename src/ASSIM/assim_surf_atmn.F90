!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE ASSIM_SURF_ATM_n (U, IM, SM, TM, WM, HPROGRAM, KI,                          &
                             PCON_RAIN, PSTRAT_RAIN, PCON_SNOW, PSTRAT_SNOW, PCLOUDS,  &
                             PLSM, PEVAPTR, PEVAP, PSWEC, PTSC, PTS, PT2M, PHU2M, PSWE,&
                             PSST, PSIC, PUCLS, PVCLS, HTEST, OD_MASKEXT, PLON, PLAT,  &
                             OLKEEPEXTZONE )
!     #################################################################################
!
!
!!****  *ASSIM_SURF_ATM_n * - Driver to call the schemes for the 
!!       four surface types (SEA, WATER, NATURE, TOWN)
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
!!-------------------------------------------------------------
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t, SEAFLUX_MODEL_t, WATFLUX_MODEL_t, TEB_MODEL_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_MPI,  ONLY : NRANK, NPIO
!
USE MODD_SURF_CONF,      ONLY : CPROGNAME
!
USE MODD_ASSIM,          ONLY : XAT2M_ISBA, XAHU2M_ISBA, XAZON10M_ISBA, XAMER10M_ISBA, XAT2M_TEB, LAROME
!
!RJ: unneeded?
!
USE MODI_ABOR1_SFX
USE MODI_ASSIM_SEA_n
USE MODI_ASSIM_INLAND_WATER_n
USE MODI_ASSIM_NATURE_n
USE MODI_ASSIM_TOWN_n
!
USE YOMHOOK,             ONLY : LHOOK,   DR_HOOK
USE PARKIND1,            ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(SEAFLUX_MODEL_t), INTENT(INOUT) :: SM
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(WATFLUX_MODEL_t), INTENT(INOUT) :: WM
!
CHARACTER(LEN=6),    INTENT(IN) :: HPROGRAM     ! program calling surf. schemes
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
REAL, DIMENSION(KI), INTENT(IN) :: PTS
REAL, DIMENSION(KI), INTENT(IN) :: PT2M
REAL, DIMENSION(KI), INTENT(IN) :: PHU2M
REAL, DIMENSION(KI), INTENT(IN) :: PSWE
REAL, DIMENSION(KI), INTENT(IN) :: PSST
REAL, DIMENSION(KI), INTENT(IN) :: PSIC
REAL, DIMENSION(KI), INTENT(IN) :: PUCLS
REAL, DIMENSION(KI), INTENT(IN) :: PVCLS
CHARACTER(LEN=2),   INTENT(IN)  :: HTEST        ! must be equal to 'OK'
LOGICAL,  DIMENSION (KI), INTENT(IN) ::  OD_MASKEXT
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLON
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLAT
LOGICAL, INTENT(IN) :: OLKEEPEXTZONE
!
!*      0.2    declarations of local variables
!
INTEGER :: JTILE                        ! loop on type of surface
LOGICAL :: GNATURE, GTOWN, GWATER, GSEA ! .T. if the corresponding surface is represented
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ASSIM_SURF_ATM_N',0,ZHOOK_HANDLE)
!
CPROGNAME = HPROGRAM
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_SURF_ATMN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!-------------------------------------------------------------------------------------
! Preliminaries: Tile related operations
!-------------------------------------------------------------------------------------

! FLAGS for the various surfaces:
!
GSEA      = U%NDIM_SEA    >0
GWATER    = U%NDIM_WATER  >0
GTOWN     = U%NDIM_TOWN   >0
GNATURE   = U%NDIM_NATURE >0
!
! Tile counter:
!
JTILE     = 0 
!
!--------------------------------------------------------------------------------------
! Call interfaces for sea, water, nature and town here...
!--------------------------------------------------------------------------------------
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! SEA Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GSEA)THEN
!
  CALL ASSIM_TREAT_SURF(JTILE,U%NSIZE_SEA,U%NR_SEA)
!
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! INLAND WATER Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GWATER)THEN
!
  CALL ASSIM_TREAT_SURF(JTILE,U%NSIZE_WATER,U%NR_WATER)
!
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! NATURAL SURFACE Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GNATURE)THEN
!
  CALL ASSIM_TREAT_SURF(JTILE,U%NSIZE_NATURE,U%NR_NATURE)

  IF ( ALLOCATED(XAT2M_ISBA))    DEALLOCATE(XAT2M_ISBA)
  IF ( ALLOCATED(XAHU2M_ISBA))   DEALLOCATE(XAHU2M_ISBA)
  IF ( ALLOCATED(XAZON10M_ISBA)) DEALLOCATE(XAZON10M_ISBA)
  IF ( ALLOCATED(XAMER10M_ISBA)) DEALLOCATE(XAMER10M_ISBA)
!
ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! URBAN Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GTOWN)THEN
!
  CALL ASSIM_TREAT_SURF(JTILE,U%NSIZE_TOWN,U%NR_TOWN)

  IF ( ALLOCATED(XAT2M_TEB))    DEALLOCATE(XAT2M_TEB)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ASSIM_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!=======================================================================================
CONTAINS
!
!=======================================================================================
SUBROUTINE ASSIM_TREAT_SURF(KTILE,KSIZE,KMASK)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)                   :: KTILE
INTEGER, INTENT(IN)                   :: KSIZE
INTEGER, INTENT(IN), DIMENSION(KSIZE) :: KMASK
!
REAL,DIMENSION(KSIZE)                 :: ZP_PCON_RAIN
REAL,DIMENSION(KSIZE)                 :: ZP_PSTRAT_RAIN
REAL,DIMENSION(KSIZE)                 :: ZP_PCON_SNOW
REAL,DIMENSION(KSIZE)                 :: ZP_PSTRAT_SNOW
REAL,DIMENSION(KSIZE)                 :: ZP_PCLOUDS
REAL,DIMENSION(KSIZE)                 :: ZP_PLSM
REAL,DIMENSION(KSIZE)                 :: ZP_PEVAPTR
REAL,DIMENSION(KSIZE)                 :: ZP_PEVAP
REAL,DIMENSION(KSIZE)                 :: ZP_PSWEC
REAL,DIMENSION(KSIZE)                 :: ZP_PTSC
REAL,DIMENSION(KSIZE)                 :: ZP_PTS
REAL,DIMENSION(KSIZE)                 :: ZP_PT2M
REAL,DIMENSION(KSIZE)                 :: ZP_PHU2M
REAL,DIMENSION(KSIZE)                 :: ZP_PSWE
REAL,DIMENSION(KSIZE)                 :: ZP_PSST
REAL,DIMENSION(KSIZE)                 :: ZP_PSIC
REAL,DIMENSION(KSIZE)                 :: ZP_UCLS
REAL,DIMENSION(KSIZE)                 :: ZP_VCLS
REAL,DIMENSION(KSIZE)                 :: ZP_LON
REAL,DIMENSION(KSIZE)                 :: ZP_LAT
LOGICAL,DIMENSION(KSIZE)              :: GD_MASKEXT
INTEGER                               :: JJ,JI
!
DO JJ=1,KSIZE
  JI=KMASK(JJ)
  ZP_PLSM(JJ)        = PLSM(JI)  
  ZP_PCON_RAIN(JJ)   = PCON_RAIN(JI)
  ZP_PSTRAT_RAIN(JJ) = PSTRAT_RAIN(JI)
  ZP_PCON_SNOW(JJ)   = PCON_SNOW(JI)
  ZP_PSTRAT_SNOW(JJ) = PSTRAT_SNOW(JI)
  ZP_PCLOUDS(JJ)     = PCLOUDS(JI)
  ZP_PEVAPTR(JJ)     = PEVAPTR(JI)
  ZP_PEVAP(JJ)       = PEVAP(JI)
  ZP_PSWE(JJ)        = PSWE(JI)  
  ZP_PSWEC(JJ)       = PSWEC(JI)
  ZP_PTSC(JJ)        = PTSC(JI)
  ZP_PTS(JJ)         = PTS(JI) 
  ZP_PT2M(JJ)        = PT2M(JI)
  ZP_PHU2M(JJ)       = PHU2M(JI)
  ZP_PSST(JJ)        = PSST(JI)
  ZP_PSIC(JJ)        = PSIC(JI)
  ZP_UCLS(JJ)        = PUCLS(JI)
  ZP_VCLS(JJ)        = PVCLS(JI)
  ZP_LON(JJ)         = PLON(JI)
  ZP_LAT(JJ)         = PLAT(JI)
  GD_MASKEXT(JJ)     = OD_MASKEXT(JI)
ENDDO

IF (KTILE==1) THEN
  
  IF (NRANK==NPIO) THEN 
    WRITE(*,*) '*********************************************'
    WRITE(*,*) '*      ASSIMILATIONS FOR SEA POINTS         *'
    WRITE(*,*) '*********************************************'
  ENDIF
 
  CALL ASSIM_SEA_n(SM%S, U, HPROGRAM, KSIZE,                &
                   ZP_PTS, ZP_PSST, ZP_PSIC, ZP_PLSM, HTEST,&
                   OLKEEPEXTZONE, GD_MASKEXT, ZP_LON, ZP_LAT)

ELSEIF (KTILE==2) THEN
  
  IF (NRANK==NPIO) THEN
    WRITE(*,*) '*********************************************'  
    WRITE(*,*) '*      ASSIMILATIONS FOR WATER POINTS       *'
    WRITE(*,*) '*********************************************'
  ENDIF

  CALL ASSIM_INLAND_WATER_n(IM%NPE, WM%W, U, HPROGRAM, KSIZE, &
                            ZP_PTS, ZP_PLSM, HTEST,           &
                            OLKEEPEXTZONE, GD_MASKEXT, ZP_LON, ZP_LAT)

ELSEIF (KTILE==3) THEN
  
  IF (NRANK==NPIO) THEN
    WRITE(*,*) '*********************************************'  
    WRITE(*,*) '*      ASSIMILATIONS FOR NATURE POINTS      *'
    WRITE(*,*) '*********************************************'
  ENDIF

  CALL ASSIM_NATURE_n(IM, U, HPROGRAM, KSIZE,                                     &
                      ZP_PCON_RAIN, ZP_PSTRAT_RAIN, ZP_PCON_SNOW, ZP_PSTRAT_SNOW, &
                      ZP_PCLOUDS,   ZP_PLSM,        ZP_PEVAPTR,   ZP_PEVAP,       & 
                      ZP_PSWEC,     ZP_PTSC,        ZP_UCLS,      ZP_VCLS,        &
                      ZP_PTS,       ZP_PT2M,        ZP_PHU2M,     ZP_PSWE,        & 
                      HTEST, GD_MASKEXT, ZP_LON, ZP_LAT )
  
ELSEIF (KTILE==4) THEN
  
  IF (NRANK==NPIO) THEN
    WRITE(*,*) '*********************************************'  
    WRITE(*,*) '*      ASSIMILATIONS FOR URBAN POINTS       *'
    WRITE(*,*) '*********************************************'
  ENDIF

  CALL ASSIM_TOWN_n(U, TM%NT, TM%TOP, HPROGRAM, KSIZE, ZP_PT2M, HTEST)
  
ENDIF

END SUBROUTINE ASSIM_TREAT_SURF
!=======================================================================================
END SUBROUTINE ASSIM_SURF_ATM_n
!=======================================================================================

