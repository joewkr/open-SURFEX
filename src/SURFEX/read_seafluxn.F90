!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SEAFLUX_n (DTCO, G, S, U, HPROGRAM,KLUOUT)
!     #########################################
!
!!****  *READ_SEAFLUX_n* - read SEAFLUX varaibles
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      Modified    02/2008 Add oceanic variables initialisation
!!      S. Belamari 04/2014 Suppress LMERCATOR
!!      R. Séférian 01/2015 introduce new ocean surface albedo 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_READ_SURF
USE MODI_INTERPOL_SST_MTH
!
USE MODI_GET_TYPE_DIM_n
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KLUOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JMTH, INMTH
CHARACTER(LEN=2 ) :: YMTH
!
INTEGER           :: ILU          ! 1D physical dimension
!
INTEGER           :: IRESP          ! Error code after redding
!
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
INTEGER           :: IVERSION       ! surface version
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_N',0,ZHOOK_HANDLE)
!
YRECFM='SIZE_SEA'
CALL GET_TYPE_DIM_n(DTCO, U, 'SEA   ',ILU)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* water temperature
!
ALLOCATE(S%XSST(ILU))
!
IF(S%LINTERPOL_SST)THEN
!
! Precedent, Current, Next, and Second-next Monthly SST
  INMTH=4
!
  ALLOCATE(S%XSST_MTH(SIZE(S%XSST),INMTH))
  DO JMTH=1,INMTH
     WRITE(YMTH,'(I2)') (JMTH-1)
     YRECFM='SST_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
     CALL READ_SURF(HPROGRAM,YRECFM,S%XSST_MTH(:,JMTH),IRESP)
  ENDDO
!
  CALL INTERPOL_SST_MTH(S,'T')
!
ELSE
! 
  ALLOCATE(S%XSST_MTH(0,0))
!
  YRECFM='SST'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XSST(:),IRESP)
!
ENDIF
!
!* stochastic flux perturbation pattern
!
ALLOCATE(S%XPERTFLUX(ILU))
IF( S%LPERTFLUX ) THEN
   CALL READ_SURF(HPROGRAM,'PERTSEAFLUX',S%XPERTFLUX(:),IRESP)
ELSE
  S%XPERTFLUX(:) = 0.
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     Semi-prognostic fields:
!               ----------------------
!
!* roughness length
!
ALLOCATE(S%XZ0(ILU))
YRECFM='Z0SEA'
S%XZ0(:) = 0.001
CALL READ_SURF(HPROGRAM,YRECFM,S%XZ0(:),IRESP)
!
!* flag to use or not the SeaIce model 
!
CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
IF (IVERSION <8) THEN
   S%LHANDLE_SIC=.FALSE.
ELSE
   CALL READ_SURF(HPROGRAM,'HANDLE_SIC',S%LHANDLE_SIC,IRESP)
ENDIF
!
!
! * sea surface salinity
!
ALLOCATE(S%XSSS(ILU))
S%XSSS(:)=0.0
!
!* Sea surface salinity nudging data
!
IF(S%LINTERPOL_SSS)THEN
   !
   ! Precedent, Current, Next, and Second-next Monthly SSS
   INMTH=4
   !
   ALLOCATE(S%XSSS_MTH(ILU,INMTH))
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SSS_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL READ_SURF(HPROGRAM,YRECFM,S%XSSS_MTH(:,JMTH),IRESP)
      CALL CHECK_SEA(YRECFM,S%XSSS_MTH(:,JMTH))
   ENDDO
   !
   CALL INTERPOL_SST_MTH(S,'S')
   !
ELSEIF (IVERSION>=8) THEN
   ! 
   ALLOCATE(S%XSSS_MTH(0,0))
   !
   YRECFM='SSS'
   CALL READ_SURF(HPROGRAM,YRECFM,S%XSSS,IRESP)
   IF(S%LHANDLE_SIC)THEN
     CALL CHECK_SEA(YRECFM,S%XSSS(:))
   ENDIF
   !
ENDIF
!
!* ocean surface albedo (direct and diffuse fraction)
!
ALLOCATE(S%XDIR_ALB (ILU))
ALLOCATE(S%XSCA_ALB (ILU))
!
IF(S%CSEA_ALB=='RS14')THEN
!
  YRECFM='OSA_DIR'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XDIR_ALB(:),IRESP)
!
  YRECFM='OSA_SCA'
  CALL READ_SURF(HPROGRAM,YRECFM,S%XSCA_ALB(:),IRESP)
!
ELSE
!
  S%XDIR_ALB(:)=0.065
  S%XSCA_ALB(:)=0.065
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE CHECK_SEA(HFIELD,PFIELD)
!
!
IMPLICIT NONE
!
CHARACTER(LEN=12),  INTENT(IN) :: HFIELD
REAL, DIMENSION(:), INTENT(IN) :: PFIELD
!
REAL            :: ZMAX,ZMIN
INTEGER         :: JI, IERRC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_N:CHECK_SEA',0,ZHOOK_HANDLE)
!
ZMIN=-1.0E10
ZMAX=1.0E10
!
IERRC=0
!
DO JI=1,ILU
   IF(PFIELD(JI)>ZMAX.OR.PFIELD(JI)<ZMIN)THEN
      IERRC=IERRC+1
      WRITE(KLUOUT,*)'PROBLEM FIELD '//TRIM(HFIELD)//' =',PFIELD(JI),&
                     'NOT REALISTIC AT LOCATION (LAT/LON)',G%XLAT(JI),G%XLON(JI)
   ENDIF
ENDDO
!         
IF(IERRC>0) CALL ABOR1_SFX('READ_SEAFLUX_N: FIELD '//TRIM(HFIELD)//' NOT REALISTIC')
!
IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_N:CHECK_SEA',1,ZHOOK_HANDLE)

END SUBROUTINE CHECK_SEA
!
!------------------------------------------------------------------------------
END SUBROUTINE READ_SEAFLUX_n
