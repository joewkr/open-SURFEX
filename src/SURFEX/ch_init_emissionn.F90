!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_INIT_EMISSION_n (CHE, PCONVERSION, HSV, HPROGRAM,KLU,HINIT,PRHOA,HCHEM_SURF_FILE)
!     #######################################
!
!!****  *CH_INIT_EMIISION_n* - routine to initialize chemical emissions data structure
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
!!      D. Gazen       * L.A. *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        08/03/2001
!!      D.Gazen  01/12/03  change emissions handling for surf. externalization
!!      P.Tulet  01/01/04  introduction of rhodref for externalization
!!      M.Leriche 04/2014  change length of CHARACTER for emission 6->12
!!      M.Leriche & V. Masson 05/16 bug in write emis fields for nest
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
USE MODD_CH_EMIS_FIELD_n, ONLY : CH_EMIS_FIELD_t
!
USE MODI_GET_LUOUT
USE MODI_BUILD_EMISSTAB_n
USE MODI_BUILD_PRONOSLIST_n
USE MODI_READ_SURF
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_READ_SURF_FIELD2D
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
!
TYPE(CH_EMIS_FIELD_t), INTENT(INOUT) :: CHE
REAL, DIMENSION(:), POINTER :: PCONVERSION
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
INTEGER             :: IRESP                 !   File 
INTEGER             :: ILUOUT                ! output listing logical unit
 CHARACTER (LEN=16) :: YRECFM                ! management
 CHARACTER (LEN=40) :: YCOMMENT              ! variables
INTEGER             :: JSPEC                 ! Loop index for cover data
INTEGER             :: IIND1,IIND2           ! Indices counter
!
 CHARACTER(LEN=40)                 :: YSPEC_NAME ! species name
 CHARACTER(LEN=12), DIMENSION(:),ALLOCATABLE :: YEMIS_NAME ! species name
INTEGER,DIMENSION(:),ALLOCATABLE  :: INBTIMES! number of emission times array
INTEGER,DIMENSION(:),ALLOCATABLE  :: ITIMES  ! emission times for a species
INTEGER,DIMENSION(:),ALLOCATABLE  :: IOFFNDX ! index array of offline emission species
INTEGER                           :: INBTS   ! number of emission times for a species
INTEGER                           :: INBOFF  ! Number of offline emissions
INTEGER                           :: IVERB   ! verbose level
INTEGER                           :: ICH      ! logical unit of input chemistry file
CHARACTER(LEN=3)                  :: YSURF   ! surface type
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK2D ! work array to read emission fields
!
INTEGER           :: IVERSION       ! version of surfex file being read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CH_INIT_EMISSION_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
WRITE(ILUOUT,*) '------ Beginning of CH_INIT_EMISSION ------'
!
!* ascendant compatibility
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
!*      1.     Chemical Emission fields
!              ------------------------
!
! Read the total number of emission files 
IF (IVERSION>=4) THEN
  CALL READ_SURF(HPROGRAM,'EMISFILE_NBR',CHE%NEMIS_NBR,IRESP)
ELSE
  CALL READ_SURF(HPROGRAM,'EMISFILE_GR_NBR',CHE%NEMIS_NBR,IRESP)
END IF
IF (IRESP/=0) THEN
  CALL ABOR1_SFX('CH_INIT_EMISSIONN: PROBLEM WHEN READING NB OF 2D CHEMICAL EMISSION FIELDS')
END IF
!
! Read the number of emission species
IF (IVERSION>=4) THEN
  CALL READ_SURF(HPROGRAM,'EMISPEC_NBR',CHE%NEMISPEC_NBR,IRESP)
ELSE
  CALL READ_SURF(HPROGRAM,'EMISPEC_GR_NBR',CHE%NEMISPEC_NBR,IRESP)
END IF
IF (IRESP/=0) THEN
  CALL ABOR1_SFX('CH_INIT_EMISSIONN: PROBLEM WHEN READING NB OF EMITTED CHEMICAL SPECIES')
END IF
!
!
IF (.NOT. ASSOCIATED(CHE%CEMIS_NAME))  THEN 
  ALLOCATE(CHE%CEMIS_NAME(CHE%NEMISPEC_NBR))
ELSE
  WRITE(ILUOUT,*) 'CEMIS_NAME already allocated with SIZE :',SIZE(CHE%CEMIS_NAME)
END IF

IF (.NOT. ASSOCIATED(CHE%CEMIS_AREA))   ALLOCATE(CHE%CEMIS_AREA(CHE%NEMISPEC_NBR))
IF (.NOT. ASSOCIATED(CHE%NEMIS_TIME))   ALLOCATE(CHE%NEMIS_TIME(CHE%NEMIS_NBR))
CHE%NEMIS_TIME(:) = -1
!
IF (HINIT/='ALL') THEN
  ALLOCATE(CHE%XEMIS_FIELDS(KLU,CHE%NEMIS_NBR))
  ALLOCATE(CHE%CEMIS_COMMENT(CHE%NEMIS_NBR))
END IF
!
ALLOCATE(ITIMES(CHE%NEMIS_NBR))
ALLOCATE(INBTIMES(CHE%NEMISPEC_NBR))
ALLOCATE(IOFFNDX(CHE%NEMISPEC_NBR))
!
INBTIMES(:) = -1
IOFFNDX(:)  = 0 ! Index array of offline species 
!
IIND1      = 0 ! Index to fill NEMIS_GR_TIMES array
IIND2      = 0 ! with emission times of offline species
!
INBOFF     = 0 ! number of offline emission species (with emis time > 0)
DO JSPEC = 1,CHE%NEMISPEC_NBR ! Loop on the number of species
!
! Read article EMISNAMExxx for the name of species
! and extract from comment : surface type + number of emission times
  WRITE(YRECFM,'("EMISNAME",I3.3)') JSPEC
  CALL READ_SURF(HPROGRAM,YRECFM,YSPEC_NAME,IRESP,YCOMMENT)
  IF (IRESP/=0) THEN
    CALL ABOR1_SFX('CH_INIT_EMISSIONN: PROBLEM WHEN READING NAME OF EMITTED CHEMICAL SPECIES')
  END IF

  WRITE(YRECFM,'("EMISAREA",I3.3)') JSPEC
  CALL READ_SURF(HPROGRAM,YRECFM,YSURF,IRESP,YCOMMENT)
  WRITE(YRECFM,'("EMISNBT",I3.3)') JSPEC
  CALL READ_SURF(HPROGRAM,YRECFM,INBTS,IRESP,YCOMMENT)
  WRITE(ILUOUT,*) ' Emission ',JSPEC,' : ',TRIM(YSPEC_NAME),'(',INBTS,' instants )'
!
! Read emission times for species number JSPEC
  WRITE(YRECFM,'("EMISTIMES",I3.3)') JSPEC
  CALL READ_SURF(HPROGRAM,YRECFM,ITIMES(1:INBTS),IRESP,YCOMMENT,'-')
  IF (IRESP/=0) THEN
    CALL ABOR1_SFX('CH_INIT_EMISSIONN: PROBLEM WHEN READING EMISSION TIMES')
  END IF
  IF (INBTS == 1) WRITE(ILUOUT,*) ' -> ',ITIMES(1)
!
! Is it an offline emission ?
  IF (INBTS >= 1) THEN
    IF (ITIMES(1) >= 0) THEN 
! Yes it is. (Note that negative time refers to inline emission like biogenics
! fluxes)
!
      INBOFF = INBOFF+1
      IOFFNDX(INBOFF)  = JSPEC
!
! INBTIMES and NEMIS_TIME only updated for offline emission
      IIND1 = IIND2+1
      IIND2 = IIND2+INBTS
      CHE%NEMIS_TIME(IIND1:IIND2) = ITIMES(1:INBTS)
      INBTIMES(INBOFF) = INBTS
    END IF
  END IF
!
CHE%NTIME_MAX = MAXVAL(CHE%NEMIS_TIME)
!
! INBTIMES, CEMIS_AREA and CEMIS_NAME 
! are updated for ALL species
  CHE%CEMIS_NAME(JSPEC) = YSPEC_NAME
  CHE%CEMIS_AREA(JSPEC) = YSURF
! 
!*      2.     Simple reading of emission fields

  IF (HINIT /= "ALL") THEN
    YRECFM='E_'//TRIM(ADJUSTL(YSPEC_NAME))
    ALLOCATE(ZWORK2D(KLU,INBTS))
    CALL READ_SURF_FIELD2D(HPROGRAM,ZWORK2D(:,:),YRECFM,YCOMMENT)
    CHE%XEMIS_FIELDS(:,IIND1:IIND2) = ZWORK2D(:,:)
    CHE%CEMIS_COMMENT(IIND1:IIND2) = YCOMMENT
    DEALLOCATE(ZWORK2D)
  END IF
!
END DO
!
WRITE(ILUOUT,*) '---- Nunmer of OFFLINE species = ',INBOFF
WRITE(ILUOUT,*) 'INBTIMES=',INBTIMES
WRITE(ILUOUT,*) 'IOFFNDX=',IOFFNDX

IVERB=6

!*      3.     Conversion and aggregation

IF (HINIT == "ALL") THEN
  IF (INBOFF > 0) THEN
    CALL OPEN_NAMELIST(HPROGRAM,ICH,HFILE=HCHEM_SURF_FILE)
    ALLOCATE(CHE%TSEMISS(INBOFF))
    ALLOCATE(YEMIS_NAME(INBOFF))

    CALL BUILD_EMISSTAB_n(PCONVERSION, HPROGRAM,ICH,CHE%CEMIS_NAME,INBTIMES,CHE%NEMIS_TIME,&
                          IOFFNDX,CHE%TSEMISS,KLU,ILUOUT,IVERB,PRHOA)  
    DO JSPEC = 1,INBOFF ! Loop on the number of species
      YEMIS_NAME(JSPEC) = CHE%TSEMISS(JSPEC)%CNAME(1:12)
    END DO
    CALL BUILD_PRONOSLIST_n(HSV, SIZE(CHE%TSEMISS),YEMIS_NAME,CHE%TSPRONOSLIST,ICH,ILUOUT,IVERB)
    DEALLOCATE(YEMIS_NAME)
    CALL CLOSE_NAMELIST(HPROGRAM,ICH)
  ELSE
    ALLOCATE(CHE%TSEMISS(0))
    NULLIFY(CHE%TSPRONOSLIST)
  END IF
ENDIF

DEALLOCATE(ITIMES,INBTIMES,IOFFNDX)
WRITE(ILUOUT,*) '------ Leaving CH_INIT_EMISSION ------'
IF (LHOOK) CALL DR_HOOK('CH_INIT_EMISSION_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE CH_INIT_EMISSION_n
