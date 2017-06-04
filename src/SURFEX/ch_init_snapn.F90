!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_INIT_SNAP_n (CHN, HSV, HPROGRAM,KLU,HINIT,PRHOA,HCHEM_SURF_FILE)
!     #######################################
!
!!****  *CH_INIT_EMIISION_TEMP_n* - routine to initialize chemical emissions data structure
!!
!!    PURPOSE
!!    -------
!       Allocates and initialize emission surface fields
!       by reading their value in initial file.
!
!!**  METHOD
!!    ------
!!    
!!    
!!    AUTHOR
!!    ------
!!      S.QUEGUINER 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        11/2011
!!      J.Escobar       11/2013 : ajout use MODI_CH_OPEN_INPUTB
!!        M.Moge    01/2016  using READ_SURF_FIELD2D for 2D surfex fields reads
!!      M.Leriche & V. Masson 05/16 move open namelist for reading ascii chemi.file
!!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_CH_SNAP_n, ONLY : CH_EMIS_SNAP_t
!
USE MODD_CSTS,       ONLY : XAVOGADRO, XMD
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_ABOR1_SFX
USE MODI_CH_CONVERSION_FACTOR
USE MODI_CH_OPEN_INPUTB
USE MODI_BUILD_PRONOSLIST_n
USE MODI_READ_SURF_FIELD2D
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
!
!
TYPE(CH_EMIS_SNAP_t), INTENT(INOUT) :: CHN
 CHARACTER(LEN=*), DIMENSION(:), POINTER :: HSV
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! Program name
INTEGER,           INTENT(IN)  :: KLU      ! number of points
 CHARACTER(LEN=3),  INTENT(IN)  :: HINIT    ! Flag to know if one initializes:
!                                          ! 'ALL' : all variables for a run
!                                          ! 'PRE' : only variables to build 
!                                          !         an initial file
REAL, DIMENSION(:),INTENT(IN)  :: PRHOA    ! air density
CHARACTER(LEN=28), INTENT(IN)  :: HCHEM_SURF_FILE ! ascii file for chemistry aggregation
!
!*       0.2   declarations of local variables
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTEMP
INTEGER :: ISNAP
INTEGER             :: IRESP                 !   File 
INTEGER             :: ILUOUT                ! output listing logical unit
 CHARACTER (LEN=3)   :: YCONVERSION
 CHARACTER (LEN=16)  :: YRECFM                ! management
 CHARACTER (LEN=100) :: YCOMMENT              ! variables
INTEGER             :: JSPEC                 ! Loop index for chemical species
INTEGER             :: JSNAP                 ! Loop index for SNAP categories
!
 CHARACTER(LEN=40)  :: YSPEC_NAME            ! species name
!
INTEGER           :: IVERSION       ! version of surfex file being read
INTEGER           :: IBUG           ! version of SURFEX bugfix
INTEGER           :: ICH      ! unit of input chemical file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CH_INIT_SNAP_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!* ascendant compatibility
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUG,IRESP)
!
!*      1.     Chemical Emission snap configuration
!              ------------------------------------
!
! Read the number of emission species and snaps
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUG>=3) ) THEN
  CALL READ_SURF(HPROGRAM,'EMISPEC_NBR',CHN%NEMIS_NBR,IRESP)
  CALL READ_SURF(HPROGRAM,'SNAP_NBR',CHN%NEMIS_SNAP,IRESP)
  CALL READ_SURF(HPROGRAM,'SNAP_TIME',CHN%CSNAP_TIME_REF,IRESP)
ELSE
  CALL ABOR1_SFX('CH_INIT_SNAPN: NO SNAP EMISSIONS IN SURFEX FILE: FILE TOO OLD')
END IF
!
! Number of instants for each temporal profile.
! For the time being, they are constant (even for the diurnal cycle)
!
CHN%NSNAP_M=12 ! 12 months
CHN%NSNAP_D=7  !  7 day a week
CHN%NSNAP_H=24 ! 24 hours a day (=> temporal resolution = 1 hour)
!
!
!*      2.     Chemical Emission fields
!              ------------------------
!
ALLOCATE(CHN%CEMIS_NAME       (               CHN%NEMIS_NBR))
ALLOCATE(CHN%CEMIS_COMMENT    (               CHN%NEMIS_NBR))
ALLOCATE(CHN%XEMIS_FIELDS_SNAP(KLU,CHN%NEMIS_SNAP,CHN%NEMIS_NBR))
ALLOCATE(CHN%XEMIS_FIELDS     (KLU,           CHN%NEMIS_NBR))
!
ALLOCATE(CHN%XSNAP_MONTHLY(CHN%NSNAP_M,CHN%NEMIS_SNAP,CHN%NEMIS_NBR))
ALLOCATE(CHN%XSNAP_DAILY  (CHN%NSNAP_D,CHN%NEMIS_SNAP,CHN%NEMIS_NBR))
ALLOCATE(CHN%XSNAP_HOURLY (CHN%NSNAP_H,CHN%NEMIS_SNAP,CHN%NEMIS_NBR))
!
IF (CHN%CSNAP_TIME_REF=='LEGAL') THEN
  ALLOCATE(CHN%XDELTA_LEGAL_TIME(KLU))
  YRECFM='LEGALTIME'
  CALL READ_SURF(HPROGRAM,YRECFM,CHN%XDELTA_LEGAL_TIME(:),IRESP,YCOMMENT)
END IF
!
IF (HPROGRAM=="NC    ") THEN
  ISNAP = MAX(CHN%NSNAP_M,CHN%NSNAP_D,CHN%NSNAP_H)
  ALLOCATE(ZTEMP(ISNAP,CHN%NEMIS_SNAP))
ENDIF
!
DO JSPEC = 1,CHN%NEMIS_NBR ! Loop on the number of species
!
! Read the species name
  WRITE(YRECFM,'("EMISNAME",I3.3)') JSPEC
  CALL READ_SURF(HPROGRAM,YRECFM,YSPEC_NAME,IRESP,YCOMMENT)
  CHN%CEMIS_COMMENT(JSPEC)=YCOMMENT
  IF (IRESP/=0) THEN
    CALL ABOR1_SFX('CH_INIT_SNAPN: PROBLEM WHEN READING NAME OF EMITTED CHEMICAL SPECIES')
  END IF
  WRITE(ILUOUT,*) ' Emission ',JSPEC,' : ',TRIM(YSPEC_NAME)
  CHN%CEMIS_NAME(JSPEC) = YSPEC_NAME(1:12)
! 
! Read  the potential emission of species for each snap
  DO JSNAP=1,CHN%NEMIS_SNAP
    WRITE(YRECFM,'("SN",I2.2,"_",A7)') JSNAP,CHN%CEMIS_NAME(JSPEC)
    CALL READ_SURF(HPROGRAM,YRECFM,CHN%XEMIS_FIELDS_SNAP(:,JSNAP,JSPEC),IRESP,YCOMMENT)
  END DO
!
! Read the temporal profiles of all snaps
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_M"
  IF (HPROGRAM=="NC    ") THEN
    CALL READ_SURF_FIELD2D(HPROGRAM,ZTEMP,YRECFM,YCOMMENT,HDIR='-')          
    CHN%XSNAP_MONTHLY(:,:,JSPEC) = ZTEMP(1:CHN%NSNAP_M,:)
  ELSE
    CALL READ_SURF_FIELD2D(HPROGRAM,CHN%XSNAP_MONTHLY(:,:,JSPEC),YRECFM,YCOMMENT,HDIR='-')          
  ENDIF
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_D"
  IF (HPROGRAM=="NC    ") THEN
    CALL READ_SURF_FIELD2D(HPROGRAM,ZTEMP,YRECFM,YCOMMENT,HDIR='-')
    CHN%XSNAP_DAILY(:,:,JSPEC) = ZTEMP(1:CHN%NSNAP_D,:)
  ELSE
    CALL READ_SURF_FIELD2D(HPROGRAM,CHN%XSNAP_DAILY(:,:,JSPEC),YRECFM,YCOMMENT,HDIR='-')
  ENDIF
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_H"
  IF (HPROGRAM=="NC    ") THEN
    CALL READ_SURF_FIELD2D(HPROGRAM,ZTEMP,YRECFM,YCOMMENT,HDIR='-')
    CHN%XSNAP_HOURLY(:,:,JSPEC) = ZTEMP(1:CHN%NSNAP_H,:)
  ELSE
    CALL READ_SURF_FIELD2D(HPROGRAM,CHN%XSNAP_HOURLY(:,:,JSPEC),YRECFM,YCOMMENT,HDIR='-')
  ENDIF
END DO
!
IF (HPROGRAM=="NC    ") DEALLOCATE(ZTEMP)
!
!*      3.     Conversion factor
!              -----------------
!
IF (HINIT=='ALL') THEN
  CALL OPEN_NAMELIST(HPROGRAM,ICH,HFILE=HCHEM_SURF_FILE)
  CALL CH_OPEN_INPUTB("EMISUNIT", ICH, ILUOUT)
!
! read unit identifier
  READ(ICH,'(A3)') YCONVERSION
!
  CHN%CCONVERSION = YCONVERSION
!
  ALLOCATE (CHN%XCONVERSION(KLU))
! determine the conversion factor
  CALL CH_CONVERSION_FACTOR(CHN%XCONVERSION, CHN%CCONVERSION,PRHOA)
!
!*      4.     List of emissions to be aggregated into atm. chemical species
!              -------------------------------------------------------------
!
  CALL BUILD_PRONOSLIST_n(HSV, CHN%NEMIS_NBR,CHN%CEMIS_NAME,CHN%TSPRONOSLIST,ICH,ILUOUT,6)
!
  CALL CLOSE_NAMELIST(HPROGRAM,ICH)
!-------------------------------------------------------------------------------
END IF
!
IF (LHOOK) CALL DR_HOOK('CH_INIT_SNAP_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE CH_INIT_SNAP_n
