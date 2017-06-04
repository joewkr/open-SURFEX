!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
MODULE MODE_READ_GRIB
!     #####################
!-------------------------------------------------------------------
!
USE MODI_ABOR1_SFX
USE GRIB_API
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE MAKE_GRIB_INDEX(HGRIB)
!     ####################
!
USE MODD_GRID_GRIB, ONLY : CGRIB_FILE, NIDX
!
IMPLICIT NONE
!
 CHARACTER(LEN=*), INTENT(IN) :: HGRIB
!
INTEGER(KIND=kindOfInt) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:MAKE_GRIB_INDEX',0,ZHOOK_HANDLE)
!
IF (CGRIB_FILE==HGRIB) CALL DR_HOOK('MODE_READ_GRIB:MAKE_GRIB_INDEX',1,ZHOOK_HANDLE)
IF (CGRIB_FILE==HGRIB) RETURN
!
CGRIB_FILE=HGRIB
!
 CALL GRIB_INDEX_CREATE(NIDX,HGRIB,'indicatorOfParameter',IRET)
IF (IRET/=0) CALL ABOR1_SFX("MODE_READ_GRIB:MAKE_GRIB_INDEX: error while creating the grib index")
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:MAKE_GRIB_INDEX',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_GRIB_INDEX
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE CLEAR_GRIB_INDEX
!     ####################
!
USE MODD_GRID_GRIB, ONLY : CGRIB_FILE, NIDX
!
IMPLICIT NONE
!
INTEGER(KIND=kindOfInt) :: IRET
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:CLEAR_GRIB_INDEX',0,ZHOOK_HANDLE)
!
IF (CGRIB_FILE.NE."") THEN
  CGRIB_FILE=""
  CALL GRIB_INDEX_RELEASE(NIDX,IRET)
  IF (IRET/=0) CALL ABOR1_SFX("MODE_READ_GRIB:MAKE_GRIB_INDEX: error while deleting the grib index")
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:CLEAR_GRIB_INDEX',1,ZHOOK_HANDLE)
!
END SUBROUTINE CLEAR_GRIB_INDEX
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE GET_GRIB_MESSAGE(KLUOUT,KLTYPE,KLEV1,KLEV2,KGRIB,KFOUND)
!     ####################
!
USE MODD_GRID_GRIB, ONLY : NIDX
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KLUOUT
INTEGER, INTENT(INOUT)  :: KLTYPE
INTEGER, INTENT(INOUT)  :: KLEV1
INTEGER, INTENT(INOUT)  :: KLEV2
INTEGER(KIND=kindOfInt), INTENT(INOUT) :: KGRIB
INTEGER, INTENT(OUT) :: KFOUND
!
INTEGER :: ILTYPE
INTEGER :: ILEV1
INTEGER :: ILEV2
INTEGER(KIND=kindOfInt) :: IRET
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:GET_GRIB_MESSAGE',0,ZHOOK_HANDLE)
!
IRET = 0
KFOUND=0
!
DO WHILE (IRET /= GRIB_END_OF_INDEX .AND. KFOUND/=3)
  !
  IRET = 0
  KFOUND=0
  !
  IF (KLTYPE/=-2) THEN
    CALL GRIB_GET(KGRIB,'indicatorOfTypeOfLevel',ILTYPE,IRET)
    CALL TEST_IRET(KLUOUT,ILTYPE,KLTYPE,IRET)
  ENDIF
  !
  IF (IRET.EQ.0) THEN
    !
    KFOUND = KFOUND + 1
    !
    IF (KLEV1/=-2) THEN
      CALL GRIB_GET(KGRIB,'topLevel',ILEV1,IRET)
      CALL TEST_IRET(KLUOUT,ILEV1,KLEV1,IRET)
    ENDIF
    !
    IF (IRET.EQ.0) THEN
      !
      KFOUND = KFOUND + 1
      !
      IF (KLEV2/=-2) THEN
        CALL GRIB_GET(KGRIB,'bottomLevel',ILEV2,IRET)
        CALL TEST_IRET(KLUOUT,ILEV2,KLEV2,IRET)
      ENDIF
      !
      IF (IRET.EQ.0) KFOUND = KFOUND + 1
      !
    ENDIF
    !
  ENDIF
  !
  IF (KFOUND.NE.3) THEN
    CALL GRIB_RELEASE(KGRIB)
    CALL GRIB_NEW_FROM_INDEX(NIDX,KGRIB,IRET)
  ENDIF
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:GET_GRIB_MESSAGE',1,ZHOOK_HANDLE)
!
CONTAINS
!
!       ##############
        SUBROUTINE TEST_IRET(KLUOUT,VAL1,VAL0,KRET)
!       ##############
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KLUOUT ! logical unit of output listing
INTEGER, INTENT(IN) :: VAL1
INTEGER, INTENT(INOUT) :: VAL0
INTEGER(KIND=kindOfInt), INTENT(INOUT) :: KRET   ! number of the message researched
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:TEST_IRET',0,ZHOOK_HANDLE)
!
IF (KRET > 0) THEN
  WRITE (KLUOUT,'(A)')' | Error encountered in the Grib file, skipping field'
ELSE IF (KRET == -6) THEN
  WRITE (KLUOUT,'(A)')' | ECMWF pseudo-Grib data encountered, skipping field'
ELSEIF (VAL1 /= VAL0) THEN
  IF (VAL0 == -1) THEN
    VAL0 = VAL1
  ELSE
    KRET=1
  ENDIF
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:TEST_IRET',1,ZHOOK_HANDLE)
END SUBROUTINE TEST_IRET
!
END SUBROUTINE GET_GRIB_MESSAGE
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE READ_GRIB(HGRIB,KLUOUT,KPARAM,KRET,PFIELD,KLTYPE,KLEV1,KLEV2,KPARAM2)
!     ####################
!
USE MODD_GRID_GRIB, ONLY : NIDX
!
IMPLICIT NONE
!
 CHARACTER(LEN=*), INTENT(IN) :: HGRIB      ! name of the GRIB file
INTEGER, INTENT(IN) :: KLUOUT
INTEGER,INTENT(IN)    :: KPARAM ! Parameter to read
INTEGER(KIND=kindOfInt), INTENT(OUT)  :: KRET
REAL, DIMENSION(:), POINTER :: PFIELD
INTEGER,INTENT(INOUT), OPTIONAL :: KLTYPE ! Level type
INTEGER,INTENT(INOUT), OPTIONAL :: KLEV1  ! Level parameter 1
INTEGER,INTENT(INOUT), OPTIONAL :: KLEV2  ! Level parameter 2
INTEGER, INTENT(INOUT), OPTIONAL :: KPARAM2
!
INTEGER :: ILTYPE, ILEV1, ILEV2
INTEGER(KIND=kindOfInt) :: IGRIB
INTEGER :: ISIZE, IFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB',0,ZHOOK_HANDLE)
!
ILTYPE=-2
IF (PRESENT(KLTYPE)) ILTYPE=KLTYPE
ILEV1=-2
IF (PRESENT(KLEV1)) ILEV1=KLEV1
ILEV2=-2
IF (PRESENT(KLEV2)) ILEV2=KLEV2
!
 CALL MAKE_GRIB_INDEX(HGRIB)
!
IFOUND=0
KRET=0
!
 CALL GRIB_INDEX_SELECT(NIDX,'indicatorOfParameter',KPARAM,KRET)
 CALL GRIB_NEW_FROM_INDEX(NIDX,IGRIB,KRET)
IF (KRET.EQ.0) CALL GET_GRIB_MESSAGE(KLUOUT,ILTYPE,ILEV1,ILEV2,IGRIB,IFOUND)
!
IF (PRESENT(KPARAM2)) THEN
  IF (IFOUND/=3) THEN
    CALL GRIB_INDEX_SELECT(NIDX,'indicatorOfParameter',KPARAM2,KRET)
    CALL GRIB_NEW_FROM_INDEX(NIDX,IGRIB,KRET)
    IF (KRET.EQ.0) THEN
      ILTYPE=-2
      IF (PRESENT(KLTYPE)) ILTYPE=KLTYPE
      CALL GET_GRIB_MESSAGE(KLUOUT,ILTYPE,ILEV1,ILEV2,IGRIB,IFOUND)
    ENDIF
  ELSE
    KPARAM2 = KPARAM
  ENDIF
ENDIF
!
IF (IFOUND==3) THEN
  !
  IF (PRESENT(KLTYPE)) KLTYPE = ILTYPE
  IF (PRESENT(KLEV1))  KLEV1  = ILEV1
  IF (PRESENT(KLEV2))  KLEV2  = ILEV2
  !
  IF (.NOT.ASSOCIATED(PFIELD)) THEN
    CALL GRIB_GET_SIZE(IGRIB,'values',ISIZE,KRET)
    IF (KRET.NE.0) CALL ABOR1_SFX("MODE_READ_GRIB:READ_GRIB: Problem getting size of values")
    ALLOCATE(PFIELD(ISIZE))
  ENDIF
  !
  CALL GRIB_GET(IGRIB,'values',PFIELD,KRET)
  IF (KRET.NE.0) CALL ABOR1_SFX("MODE_READ_GRIB:READ_GRIB: Problem getting values")
  CALL GRIB_RELEASE(IGRIB,KRET)
  IF (KRET.NE.0) CALL ABOR1_SFX("MODE_READ_GRIB:READ_GRIB: Problem releasing memory")
  !
ELSE
  !
  KRET = 1
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB
!
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ####################
      SUBROUTINE READ_GRIB_LAND_MASK(HGRIB,KLUOUT,HINMODEL,PMASK)
!     ####################
!
IMPLICIT NONE
!
CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER       :: PMASK     ! Land mask
!
INTEGER(KIND=kindOfInt)                 :: IRET      ! return code
INTEGER                           :: ILTYPE    ! leveltype
INTEGER                           :: ILEV      ! level
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_LAND_MASK',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_LAND_MASK: | Reading land mask from ',HINMODEL
!
SELECT CASE (HINMODEL)
  CASE ('ECMWF ')
    CALL READ_GRIB(HGRIB,KLUOUT,172,IRET,PMASK) 
  CASE ('ARPEGE','ALADIN','MOCAGE')
    CALL READ_GRIB(HGRIB,KLUOUT,81,IRET,PMASK)          
  CASE ('HIRLAM')        
    ILTYPE=105
    ILEV  =0    
    CALL READ_GRIB(HGRIB,KLUOUT,81,IRET,PMASK,KLTYPE=ILTYPE,KLEV1=ILEV)            
  CASE DEFAULT
    CALL ABOR1_SFX('MODE_READ_GRIB:READ_GRIB_LAND_MASK: OPTION NOT SUPPORTED '//HINMODEL)
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: LAND SEA MASK MISSING (READ_GRIB_LAND_MASK)')
END IF
!
WHERE (PMASK>0.5)
  PMASK = 1.
ELSEWHERE
  PMASK = 0.
END WHERE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_LAND_MASK',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_LAND_MASK
!-------------------------------------------------------------------
!     ############################
      SUBROUTINE READ_GRIB_ZS(HGRIB,KLUOUT,HINMODEL,PZS)
!     ############################
!
USE MODD_CSTS,       ONLY : XG
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER       :: PZS       ! 
!
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!* Read orography
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_ZS',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_ZS: | Reading orography from ',HINMODEL
!
SELECT CASE (HINMODEL)
  CASE ('ECMWF ')
    CALL READ_GRIB(HGRIB,KLUOUT,129,IRET,PZS) 
  CASE ('ARPEGE','MOCAGE')
    CALL READ_GRIB(HGRIB,KLUOUT,8,IRET,PZS)               
  CASE ('HIRLAM','ALADIN')
    CALL READ_GRIB(HGRIB,KLUOUT,6,IRET,PZS)                  
  CASE DEFAULT
    CALL ABOR1_SFX('MODE_READ_GRIB:READ_GRIB_ZS:OPTION NOT SUPPORTED '//HINMODEL)
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: OROGRAPHY MISSING (READ_GRIB_ZS_LAND)')
END IF
!
! Datas given in archives are multiplied by the gravity acceleration
PZS = PZS / XG
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_ZS',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_ZS
!-------------------------------------------------------------------
!     ############################
      SUBROUTINE READ_GRIB_ZS_LAND(HGRIB,KLUOUT,HINMODEL,PMASK,PZSL)
!     ############################
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:), POINTER       :: PZSL      ! 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_ZS_LAND',0,ZHOOK_HANDLE)
!
 CALL READ_GRIB_ZS(HGRIB,KLUOUT,HINMODEL,PZSL)
!
IF (SIZE(PMASK)==SIZE(PZSL)) &
  WHERE (PMASK(:)/=1.) PZSL = 0.
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_ZS_LAND',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_ZS_LAND
!-------------------------------------------------------------------
!     ############################
      SUBROUTINE READ_GRIB_ZS_SEA(HGRIB,KLUOUT,HINMODEL,PMASK,PZSS)
!     ############################
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:), POINTER       :: PZSS      ! 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_ZS_SEA',0,ZHOOK_HANDLE)
!
  CALL READ_GRIB_ZS(HGRIB,KLUOUT,HINMODEL,PZSS)
!
IF (SIZE(PMASK)==SIZE(PZSS)) &
  WHERE (PMASK(:)/=0.) PZSS = 0.
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_ZS_SEA',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_ZS_SEA
!-------------------------------------------------------------------
!     ###########################
      SUBROUTINE READ_GRIB_T(HGRIB,KLUOUT,HINMODEL,PT)
!     ###########################
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER       :: PT        ! 
!
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
INTEGER                           :: ILTYPE    ! type of level (Grib code table 3)
INTEGER                           :: ILEV      ! level definition
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!* Read surface temperature
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_T',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_T: | Reading surface temperature'
!
SELECT CASE (HINMODEL)
  CASE ('ECMWF ')
    CALL READ_GRIB(HGRIB,KLUOUT,139,IRET,PT)    

  CASE ('ARPEGE','ALADIN','MOCAGE')
    ILEV=0
    ILTYPE=111
    CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,PT,KLTYPE=ILTYPE,KLEV1=ILEV) 
    IF (IRET /= 0) THEN
      ILTYPE=1
      CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,PT,KLTYPE=ILTYPE) 
      IF (IRET /= 0) THEN
         ILTYPE=105
        CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,PT,KLTYPE=ILTYPE,KLEV1=ILEV)   
      ENDIF        
    END IF

  CASE ('HIRLAM ')
    WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_T: | Reading surface temperature tile 4'
     ILTYPE=105
     ILEV=904
    CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,PT,KLTYPE=ILTYPE,KLEV1=ILEV)

  CASE DEFAULT
    CALL ABOR1_SFX('MODE_READ_GRIB:READ_GRIB_T:OPTION NOT SUPPORTED '//HINMODEL)
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: SURFACE TEMPERATURE MISSING (READ_GRIB_T)')
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_T',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_T
!-------------------------------------------------------------------
!     ###########################
      SUBROUTINE READ_GRIB_TS(HGRIB,KLUOUT,HINMODEL,PMASK,PTS)
!     ###########################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:), POINTER       :: PTS       ! 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TS',0,ZHOOK_HANDLE)
!
 CALL READ_GRIB_T(HGRIB,KLUOUT,HINMODEL,PTS)
!
IF (SIZE(PMASK)==SIZE(PTS)) WHERE (PMASK(:)/=1.) PTS = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TS',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_TS
!-------------------------------------------------------------------
!     ###########################
      SUBROUTINE READ_GRIB_SST(HGRIB,KLUOUT,HINMODEL,PMASK,PSST)
!     ###########################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:), POINTER       :: PSST      ! 
!
INTEGER :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SST',0,ZHOOK_HANDLE)
!
SELECT CASE (HINMODEL)
  CASE ('ECMWF ')
     CALL READ_GRIB(HGRIB,KLUOUT,34,IRET,PSST)
     IF (IRET /= 0) CALL READ_GRIB_T(HGRIB,KLUOUT,HINMODEL,PSST)
  CASE ('ARPEGE','ALADIN','MOCAGE','HIRLAM')
    CALL READ_GRIB_T(HGRIB,KLUOUT,HINMODEL,PSST)
   CASE DEFAULT
     CALL ABOR1_SFX('MODE_READ_GRIB:READ_GRIB_SST:OPTION NOT SUPPORTED '//HINMODEL)    
END SELECT
!
IF (SIZE(PMASK)==SIZE(PSST)) WHERE (PMASK(:)/=0.) PSST = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SST',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_SST
!-------------------------------------------------------------------
!     ###########################
      SUBROUTINE READ_GRIB_TSWATER(HGRIB,KLUOUT,HINMODEL,PMASK,PTS)
!     ###########################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:), POINTER       :: PTS     ! 
!
INTEGER :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TSWATER',0,ZHOOK_HANDLE)
!
SELECT CASE (HINMODEL)
  CASE ('ECMWF ')
     CALL READ_GRIB(HGRIB,KLUOUT,3080,IRET,PTS)
     IF (IRET /= 0) CALL READ_GRIB_T2(HGRIB,KLUOUT,HINMODEL,PMASK,PTS)
  CASE ('ARPEGE','ALADIN','MOCAGE','HIRLAM')
    CALL READ_GRIB_T2(HGRIB,KLUOUT,HINMODEL,PMASK,PTS)
   CASE DEFAULT
     CALL ABOR1_SFX('MODE_READ_GRIB:READ_GRIB_TSWATER:OPTION NOT SUPPORTED '//HINMODEL)    
END SELECT
!
IF (SIZE(PMASK)==SIZE(PTS)) WHERE (PMASK(:)/=0.) PTS = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TSWATER',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_TSWATER
!-------------------------------------------------------------------
!     ###########################
      SUBROUTINE READ_GRIB_T2(HGRIB,KLUOUT,HINMODEL,PMASK,PT2)
!     ###########################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:), POINTER       :: PT2       ! 
!
INTEGER(KIND=kindOfInt)                           :: IRET
INTEGER                           :: ILTYPE, ILEV    ! type of level (Grib code table 3)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!* Read deep soil temperature
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_T2',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_T2: | Reading deep soil temperature'
!
SELECT CASE (HINMODEL)
  CASE ('ECMWF ')
     CALL READ_GRIB(HGRIB,KLUOUT,170,IRET,PT2)   
  CASE ('ARPEGE','ALADIN','MOCAGE')
     ILTYPE=111
     CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,PT2,KLTYPE=ILTYPE) 
    IF (IRET /= 0) THEN
       ILTYPE=105
      CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,PT2,KLTYPE=ILTYPE)   
    ENDIF  
  CASE ('HIRLAM ')
     ILTYPE=105
     ILEV=954
    CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,PT2,KLTYPE=ILTYPE,KLEV1=ILEV)      
   CASE DEFAULT
     CALL ABOR1_SFX('MODE_READ_GRIB:READ_GRIB_T2:OPTION NOT SUPPORTED '//HINMODEL)
END SELECT
!
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: DEEP SOIL TEMPERATURE MISSING (READ_GRIB_T2)')
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_T2',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_T2
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     ###########################
      SUBROUTINE READ_GRIB_T2_LAND(HGRIB,KLUOUT,HINMODEL,PMASK,ZFIELD)
!     ###########################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),   INTENT(IN)   :: HGRIB     ! Grib file name
INTEGER,            INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),   INTENT(IN)   :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:), POINTER       :: ZFIELD    ! 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------
!* Read deep soil temperature
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_T2_LAND',0,ZHOOK_HANDLE)
!
 CALL READ_GRIB_T2(HGRIB,KLUOUT,HINMODEL,PMASK,ZFIELD)
!
IF (SIZE(PMASK)==SIZE(ZFIELD)) WHERE (PMASK(:)/=1.) ZFIELD = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_T2_LAND',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_T2_LAND
!-------------------------------------------------------------------
!-------------------------------------------------------------------
SUBROUTINE PUT_LAYER_DEPTH(KLUOUT,KLEV,HROUT,KLTYPE,KLEV1,KLEV2,KNLAYERDEEP,PV4,PV,PD)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KLUOUT
INTEGER, INTENT(IN) :: KLEV
 CHARACTER(LEN=*), INTENT(IN) :: HROUT
INTEGER, INTENT(INOUT) :: KLTYPE
INTEGER, INTENT(IN) :: KLEV1
INTEGER, INTENT(IN) :: KLEV2
INTEGER, INTENT(IN) :: KNLAYERDEEP
REAL, INTENT(IN) :: PV4
REAL, INTENT(IN) :: PV
REAL, INTENT(OUT) :: PD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:PUT_LAYER_DEPTH',0,ZHOOK_HANDLE)
!
IF (KLEV2 == -1) KLTYPE = 0
IF (KLTYPE==112) THEN
  PD = (KLEV2 - KLEV1) / 100.
ELSE
  IF (KNLAYERDEEP == 4) THEN
    PD = PV4
  ELSE
    PD = PV
  END IF
  WRITE (KLUOUT,'(A,i1,A,f5.2,A)') 'MODE_READ_GRIB:'//TRIM(HROUT)//': | Level ',&
                                    KLEV,' height not available, assuming ',PD,' m'
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:PUT_LAYER_DEPTH',1,ZHOOK_HANDLE)
END SUBROUTINE PUT_LAYER_DEPTH
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE FILL_PFIELD(KLUOUT,HROUT,KNLAYERDEEP,PDIN,PFIELDIN,PMASK,PFIELDOUT,PDOUT)
!     #######################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KLUOUT
 CHARACTER(LEN=*), INTENT(IN) :: HROUT
INTEGER, INTENT(IN) :: KNLAYERDEEP
REAL, DIMENSION(:), INTENT(IN) :: PDIN
REAL, DIMENSION(:,:), INTENT(IN) :: PFIELDIN
REAL, DIMENSION(:), INTENT(IN) :: PMASK
REAL, DIMENSION(:,:), POINTER :: PFIELDOUT
REAL, DIMENSION(:,:), POINTER :: PDOUT
!
 CHARACTER(LEN=20) :: FMT0
INTEGER :: JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:FILL_PFIELD',0,ZHOOK_HANDLE)
!--------------------------------------------------------------------------------
! 1.  Display the number of layer found
!     -----------------------
WRITE(FMT0,FMT='(A8,I1,A11)') '(A,I1,A,',KNLAYERDEEP,'(F5.2,","))'
WRITE (KLUOUT,FMT=FMT0) 'MODE_READ_GRIB:'//TRIM(HROUT)//': | ',KNLAYERDEEP,&
                        ' deep layers, heights are : ',PDIN(1:KNLAYERDEEP)
!--------------------------------------------------------------------------------
! 2.  Set temperature profile and layer thicknesses
!     -----------------------------------------------
ALLOCATE(PFIELDOUT(SIZE(PFIELDIN,1),KNLAYERDEEP))
ALLOCATE(PDOUT(SIZE(PFIELDIN,1),KNLAYERDEEP))
!
DO JL=1,KNLAYERDEEP
  PDOUT(:,JL)=SUM(PDIN(1:JL))
  PFIELDOUT(:,JL)=PFIELDIN(:,JL)
  IF (SIZE(PMASK)==SIZE(PFIELDOUT,1)) &
    WHERE (PMASK(:)/=1.) PFIELDOUT(:,JL) = XUNDEF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:FILL_PFIELD',1,ZHOOK_HANDLE)
END SUBROUTINE FILL_PFIELD
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_TG_ECMWF(HGRIB,KLUOUT,HINMODEL,PMASK,PTG,PD)
!     #######################
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PTG       ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
INTEGER                           :: ILTYPE    ! type of level (Grib code table 3)
INTEGER                           :: ILEV1     ! level definition
INTEGER                           :: ILEV2     ! level definition
INTEGER                           :: JL         ! layer loop counter
INTEGER                           :: INLAYERDEEP! number of deep moisture layers
REAL,    DIMENSION(:), POINTER    :: ZFIELD => NULL()  ! first layer temperature
REAL,  DIMENSION(:,:), ALLOCATABLE:: ZTG      ! first layer temperature
REAL, DIMENSION(:)   , ALLOCATABLE:: ZD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TG_ECMWF',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_TG_ECMWF: | Reading soil temperature'
!
ALLOCATE(ZD(5))
!
! 1.  Search and read level 1 (and its depth)
!     --------------------------------------
ILTYPE= -1
ILEV1 = -1
ILEV2 = -1
 CALL READ_GRIB(HGRIB,KLUOUT,139,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
!
IF (IRET== 0) THEN
  CALL PUT_LAYER_DEPTH(KLUOUT,1,'READ_GRIB_TG_ECMWF',ILTYPE,ILEV1,ILEV2,4,0.07,0.07,ZD(1))
  ALLOCATE(ZTG(SIZE(ZFIELD),5))
  ZTG(:,1)=ZFIELD
ELSE
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL TEMPERATURE LEVEL 1 MISSING (READ_GRIB_TG_ECMWF)')
ENDIF
!
! 2.  Search and read level 4 (and its depth) This level is optionnal
!     ---------------------------------------------------------------
ILTYPE= -1
ILEV1 = -1
ILEV2 = -1
 CALL READ_GRIB(HGRIB,KLUOUT,236,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
!
IF (IRET == 0) THEN
  INLAYERDEEP = 4
  CALL PUT_LAYER_DEPTH(KLUOUT,4,'READ_GRIB_TG_ECMWF',ILTYPE,ILEV1,ILEV2,INLAYERDEEP,1.89,1.89,ZD(4))
  ZTG(:,4)=ZFIELD
ELSE
  INLAYERDEEP = 3
  ZD(4) = 0.
ENDIF
!
! 3.  Search and read level 3 (and its depth) This level is optionnal
!     ---------------------------------------------------------------
ILTYPE= -1
ILEV1 = -1
ILEV2 = -1
 CALL READ_GRIB(HGRIB,KLUOUT,183,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
!
IF (IRET == 0) THEN      
  CALL PUT_LAYER_DEPTH(KLUOUT,3,'READ_GRIB_TG_ECMWF',ILTYPE,ILEV1,ILEV2,INLAYERDEEP,0.72,0.42,ZD(3))
  ZTG(:,3)=ZFIELD 
ELSE
  INLAYERDEEP = 2
  ZD(3) = 0.        
ENDIF
!
! 4.  Search and read level 2 (and its depth)
!     ---------------------------------------
ILTYPE= -1
ILEV1 = -1
ILEV2 = -1
 CALL READ_GRIB(HGRIB,KLUOUT,170,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
!
IF (IRET== 0) THEN         
  CALL PUT_LAYER_DEPTH(KLUOUT,2,'READ_GRIB_TG_ECMWF',ILTYPE,ILEV1,ILEV2,INLAYERDEEP,0.21,0.42,ZD(2))
  ZTG(:,2)=ZFIELD
  DEALLOCATE(ZFIELD)    
ELSE
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL TEMPERATURE LEVEL 2 MISSING (READ_GRIB_TG_ECMWF)')
ENDIF
!--------------------------------------------------------------------------------
! 5.  Assumes uniform temperature profile up to 3m depth
!     -------------------------------------------------
!
IF(SUM(ZD(1:INLAYERDEEP)) < 3.) THEN
  !We add a temperature layer
  INLAYERDEEP=INLAYERDEEP+1
  ZD(INLAYERDEEP)=3.-SUM(ZD(1:INLAYERDEEP-1))
  ZTG(:,INLAYERDEEP)=ZTG(:,INLAYERDEEP-1)
ENDIF
!
!--------------------------------------------------------------------------------
! 6.  Set temperature profile and layer thicknesses
!     ----------------------------------------------
 CALL FILL_PFIELD(KLUOUT,'READ_GRIB_TG_ECMWF',INLAYERDEEP,ZD,ZTG,PMASK,PTG,PD)
DEALLOCATE(ZD)
DEALLOCATE(ZTG)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TG_ECMWF',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_TG_ECMWF
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_WG_ECMWF_1(HGRIB,KLUOUT,HINMODEL,PMASK,PWG,PD)
!     #######################
!
! This tasks is divided in the following steps :
!  - computing the MesoNH constants
!  - reading the grib datas according to the type of file (ECMWF/Arpege/Aladin)
!  - converting from specific humidity to relative humidity
!  - interpolation with land mask
!  - converting back from relative humidity to specific humidity with MesoNH constants
! Five different models are supported :
!  - ECMWF with 2 layers (untested)
!  - ECMWF with 3 layers (archive before 1991 - Blondin model)
!  - ECMWF with 4 layers (archive after 1991 - Viterbo model)
!  - Arpege/Aladin before ISBA (I don't know the name of this model)
!  - Arpege/Aladin with ISBA model
! The available model is detect according to the fields which are presents :
!  - ECMWF archive : loads as many layers as possible
!  - Arpege/Aladin archive : ISBA model needs Clay and Sans fraction fields, if they
!    are present, they are used and the model is declared to be ISBA.
! To detect the height of the layers, two methods are used :
!  - if level type is not 112, a default value is assumed and a warning message is
!    displayed
!  - if level type is ID 112, then the position of the top and bottom surface may be
!    given. If they are present, they are used, if not the default value is taken and
!    a warning message is issued.
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PWG       ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
INTEGER                           :: IPAR      ! parameter number for field reading
INTEGER                           :: ILTYPE    ! type of level (Grib code table 3)
INTEGER                           :: ILEV1     ! level definition
INTEGER                           :: ILEV2     ! level definition
INTEGER                           :: INLAYERDEEP! number of deep moisture layers
REAL,    DIMENSION(:), POINTER    :: ZFIELD => NULL()  ! first layer temperature
REAL,  DIMENSION(:,:), ALLOCATABLE:: ZWG      ! first layer temperature
REAL, DIMENSION(:)   , ALLOCATABLE:: ZD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WG_ECMWF_1',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_WG_ECMWF_1: | Reading soil moisture'
!
ALLOCATE(ZD(4))
!
! 1.  Search and read level 1 (and its depth)
!     --------------------------------------
ILTYPE= -1
ILEV1 = -1
ILEV2 = -1
IPAR=39
 CALL READ_GRIB(HGRIB,KLUOUT,140,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2,KPARAM2=IPAR)
!
IF (IRET == 0) THEN 
  CALL PUT_LAYER_DEPTH(KLUOUT,1,'READ_GRIB_WG_ECMWF_1',ILTYPE,ILEV1,ILEV2,4,0.07,0.07,ZD(1))
  ALLOCATE(ZWG(SIZE(ZFIELD,1),4))
  ZWG(:,1)=ZFIELD
  !
  IF (IPAR==140) ZWG(:,1)=ZWG(:,1) / ZD(1)
ELSE
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL MOISTURE LEVEL 1 MISSING (READ_GRIB_WG_ECMWF_1)')
ENDIF
!
! 2.  Search and read level 4 (and its depth) This level is optionnal
!     ---------------------------------------------------------------
ILTYPE= -1
ILEV1 = -1
ILEV2 = -1
IPAR=42
 CALL READ_GRIB(HGRIB,KLUOUT,237,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2,KPARAM2=IPAR)
!
IF (IRET == 0) THEN   
  INLAYERDEEP = 4
  CALL PUT_LAYER_DEPTH(KLUOUT,4,'READ_GRIB_WG_ECMWF_1',ILTYPE,ILEV1,ILEV2,INLAYERDEEP,1.89,1.89,ZD(4))
  ZWG(:,4)=ZFIELD   
  !
  IF (IPAR==237) ZWG(:,4)=ZWG(:,4) / ZD(1) 
ELSE
  INLAYERDEEP = 3  
  ZD(4) = 0.
ENDIF
!
! 3.  Search and read level 3 (and its depth) This level is optionnal
!     ---------------------------------------------------------------
ILTYPE= -1
ILEV1 = -1
ILEV2 = -1
IPAR=41
 CALL READ_GRIB(HGRIB,KLUOUT,184,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2,KPARAM2=IPAR)
!
IF (IRET == 0) THEN
  CALL PUT_LAYER_DEPTH(KLUOUT,3,'READ_GRIB_WG_ECMWF_1',ILTYPE,ILEV1,ILEV2,INLAYERDEEP,0.72,0.42,ZD(3))
  ZWG(:,3)=ZFIELD 
  !
  IF (IPAR==184) ZWG(:,3)=ZWG(:,3) / ZD(1)
ELSE
  INLAYERDEEP = 2  
  ZD(3) = 0.  
ENDIF
!
! 4.  Search and read level 2 (and its depth)
!     ---------------------------------------
ILTYPE= -1
ILEV1 = -1
ILEV2 = -1
IPAR=40
 CALL READ_GRIB(HGRIB,KLUOUT,171,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2,KPARAM2=IPAR)
!
IF (IRET == 0) THEN
  CALL PUT_LAYER_DEPTH(KLUOUT,2,'READ_GRIB_WG_ECMWF_1',ILTYPE,ILEV1,ILEV2,INLAYERDEEP,0.21,0.42,ZD(2))
  ZWG(:,2)=ZFIELD
  DEALLOCATE(ZFIELD)  
  !
  IF (IPAR==171) ZWG(:,2)=ZWG(:,2) / ZD(1)
ELSE
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL MOISTURE LEVEL 2 MISSING (READ_GRIB_WG_ECMWF_1)')
ENDIF
!
!--------------------------------------------------------------------------------
!
 CALL FILL_PFIELD(KLUOUT,'READ_GRIB_WG_ECMWF_1',INLAYERDEEP,ZD,ZWG,PMASK,PWG,PD)
DEALLOCATE(ZD)
DEALLOCATE(ZWG)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WG_ECMWF_1',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_WG_ECMWF_1
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE ECMWF_WGI(PTG,PWG,PWGI)
!     #######################
!
! ECMWF grib only contain (ice+water) content.
! This routine computes iced part and water part according to the formula
! given in ECMWF documentation. But we use real water content instead of
! (CL+CH) times saturation capacity.
!
USE MODD_CSTS,        ONLY : XTT, XPI
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
REAL, DIMENSION(:,:), INTENT(IN)    :: PTG       ! Temperature profil
REAL, DIMENSION(:,:), INTENT(INOUT) :: PWG       ! INPUT contains (water+ice) profil
                                                 ! OUTPUT contains only water profil
REAL, DIMENSION(:,:), INTENT(OUT)   :: PWGI      ! ice profil
!
!* local variables
!  ---------------
REAL  :: ZT1, ZT2  ! Temperature threshold
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:ECMWF_WGI',0,ZHOOK_HANDLE)
!
ZT1=XTT + 1.
ZT2=XTT - 3.
!
WHERE(PTG(:,:) > ZT1)
  PWGI(:,:) = 0.
ELSEWHERE(PTG(:,:) < ZT2)
  PWGI(:,:) = PWG(:,:)
  PWG(:,:) = 0.
ELSEWHERE
  PWGI(:,:)=PWG(:,:) * 0.5* (1 - sin(XPI * (PTG(:,:) - 0.5*ZT1 - 0.5*ZT2) / &
                                           (ZT1 - ZT2                   )   ))
  PWG(:,:) = PWG(:,:) - PWGI(:,:)
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:ECMWF_WGI',1,ZHOOK_HANDLE)
END SUBROUTINE ECMWF_WGI
!--------------------------------------------------------------------------------
!     #######################
      SUBROUTINE HARMONIZE_GRIB_WG_WGI_ECMWF(HGRIB,KLUOUT,HINMODEL,PMASK,PWG,PD,PWGI)
!     #######################
!
! ECMWF grib only contain (ice+water) content.
! This routine computes iced part and water part according to the formula
! given in ECMWF documentation. But we use real water content instead of
! (CL+CH) times saturation capacity.
!
USE MODD_CSTS,        ONLY : XTT, XPI
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), OPTIONAL, POINTER :: PWG   ! INPUT contains (water+ice) profil
                                                 ! OUTPUT contains only water profil
REAL, DIMENSION(:,:), OPTIONAL, POINTER :: PD    ! thickness of each layer                          
REAL, DIMENSION(:,:), OPTIONAL, POINTER :: PWGI  ! ice profil
!* local variables
!  ---------------
REAL,  DIMENSION(:,:), POINTER :: ZWG => NULL()          ! profile of soil water contents
REAL,  DIMENSION(:,:), POINTER :: ZD => NULL()           ! thickness of each layer
REAL,  DIMENSION(:,:), POINTER :: ZTG => NULL()          ! profile of temperature
REAL,  DIMENSION(:,:), POINTER :: ZDT => NULL()          ! thickness of each temperature layer
REAL,  DIMENSION(:,:), ALLOCATABLE:: ZWGI      ! profile of soil ice contents
REAL  :: ZT1, ZT2  ! Temperature threshold
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:HARMONIZE_GRIB_WG_WGI_ECMWF',0,ZHOOK_HANDLE)
!
 CALL READ_GRIB_TG_ECMWF(HGRIB,KLUOUT,HINMODEL,PMASK,ZTG,ZDT)
 CALL READ_GRIB_WG_ECMWF_1(HGRIB,KLUOUT,HINMODEL,PMASK,ZWG,ZD)
!
IF (SIZE(ZTG,2) .LT. SIZE(ZWG,2)) THEN
  WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:HARMONIZE_GRIB_WG_WGI_ECMWF: '
  WRITE  (KLUOUT,'(A)') 'ERROR, YOU HAVE NOT THE SAME NUMBER OF LEVELS '
  WRITE  (KLUOUT,'(A)') 'IN SOIL FOR TEMPERATURE AND HUMIDITY '
  WRITE  (KLUOUT,'(A)') 'VERIFY GRIB FILE '
  CALL ABOR1_SFX("MODE_READ_GRIB:HARMONIZE_GRIB_WG_WGI_ECMWF: VERIFY NUMBER OF LEVELS IN GRIB FILE")
ENDIF
!
IF (PRESENT(PD)) THEN
  ALLOCATE(PD(SIZE(ZD,1),SIZE(ZD,2)))
  PD(:,:)=ZD(:,:)
ENDIF
IF (PRESENT(PWGI)) THEN
  ALLOCATE(PWGI(SIZE(ZWG,1),SIZE(ZWG,2)))
  PWGI(:,:)=0.
ENDIF
!
!If same vertical grids are taken into account for WG and TG we can
!compute ice content and new water content
IF(ALL(ZDT(:,1:SIZE(ZWG,2))==ZD(:,1:SIZE(ZWG,2)))) THEN     
  ALLOCATE(ZWGI(SIZE(ZWG,1),SIZE(ZWG,2)))
  CALL ECMWF_WGI(ZTG(:,1:SIZE(ZWG,2)),ZWG,ZWGI)
  IF (PRESENT(PWGI)) PWGI(:,:)=ZWGI(:,:)
  DEALLOCATE(ZWGI)
ENDIF
!
IF (PRESENT(PWG)) THEN
  ALLOCATE(PWG(SIZE(ZWG,1),SIZE(ZWG,2)))
  PWG(:,:)=ZWG(:,:)
ENDIF
!
DEALLOCATE(ZWG)
DEALLOCATE(ZD)
DEALLOCATE(ZTG)
DEALLOCATE(ZDT)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:HARMONIZE_GRIB_WG_WGI_ECMWF',1,ZHOOK_HANDLE)
END SUBROUTINE HARMONIZE_GRIB_WG_WGI_ECMWF
!--------------------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_WG_ECMWF(HGRIB,KLUOUT,HINMODEL,PMASK,PFIELD,PD)
!     #######################
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer     
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)                           :: IRET       ! return code
REAL,    DIMENSION(:), POINTER    :: ZSLT => NULL()       ! soil type
REAL,    DIMENSION(:), ALLOCATABLE:: ZWWILT     ! ECMWF wilting point
REAL,    DIMENSION(:), ALLOCATABLE:: ZWFC       ! ECMWF field capacity
INTEGER                           :: JL         ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WG_ECMWF',0,ZHOOK_HANDLE)
!
 CALL HARMONIZE_GRIB_WG_WGI_ECMWF(HGRIB,KLUOUT,HINMODEL,PMASK,PWG=PFIELD,PD=PD)
!
! 1.  Get soil type to compute SWI
!     ----------------------------
 CALL READ_GRIB(HGRIB,KLUOUT,43,IRET,ZSLT)
!--------------------------------------------------------------------------------
ALLOCATE (ZWFC(SIZE(PFIELD,1)))
ALLOCATE (ZWWILT(SIZE(PFIELD,1)))
ZWFC  (:) = 0.
ZWWILT(:) = 0.
!
IF (IRET == 0) THEN
!        
! 2.1 Convert from specific humidity to relative humidity using soil types
!     --------------------------------------------------------------------
  WHERE (ZSLT(:)==1.) 
    ZWFC(:)         = 0.242
    ZWWILT(:)       = 0.059
  ELSEWHERE (ZSLT(:)==2.) 
    ZWFC(:)         = 0.346
    ZWWILT(:)       = 0.151
  ELSEWHERE (ZSLT(:)==3.) 
    ZWFC(:)         = 0.382
    ZWWILT(:)       = 0.133
  ELSEWHERE (ZSLT(:)==4.) 
    ZWFC(:)         = 0.448
    ZWWILT(:)       = 0.279
  ELSEWHERE (ZSLT(:)==5.) 
    ZWFC(:)         = 0.541
    ZWWILT(:)       = 0.335
  ELSEWHERE (ZSLT(:)==6.) 
    ZWFC(:)         = 0.662
    ZWWILT(:)       = 0.267
  ENDWHERE
  !
ELSE
!
! 2.2 Convert from specific humidity to relative humidity single soil type
!     --------------------------------------------------------------------
  ! Compute model's constants
  IF (SIZE(PFIELD,2)==4) THEN
    ZWFC(:)   = 0.323
    ZWWILT(:) = 0.171
  ELSE
    ZWFC(:)   = 0.171
    ZWWILT(:) = 0.086
  END IF
  !
ENDIF
!
DO JL=1,SIZE(PFIELD,2)
  WHERE ( PFIELD(:,JL).NE.XUNDEF .AND. ZWFC(:).NE.0. ) 
    PFIELD(:,JL) = (PFIELD(:,JL) - ZWWILT(:)) / (ZWFC(:) - ZWWILT(:))
  ELSEWHERE
    PFIELD(:,JL) = 0.
  ENDWHERE
ENDDO
!
IF (ASSOCIATED(ZSLT)) DEALLOCATE(ZSLT)
DEALLOCATE(ZWFC)
DEALLOCATE(ZWWILT)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WG_ECMWF',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_WG_ECMWF
!----------------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_WGI_ECMWF(HGRIB,KLUOUT,HINMODEL,PMASK,PFIELD,PD)
!     #######################
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
REAL,    DIMENSION(:), POINTER    :: ZSLT => NULL()      ! soil type
REAL,  DIMENSION(:)  , ALLOCATABLE:: ZWSAT     ! ECMWF saturation
INTEGER                           :: JL        ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WGI_ECMWF',0,ZHOOK_HANDLE)
!
 CALL HARMONIZE_GRIB_WG_WGI_ECMWF(HGRIB,KLUOUT,HINMODEL,PMASK,PD=PD,PWGI=PFIELD)
!
! 1.  Get soil type to compute WSAT
!     ----------------------------
 CALL READ_GRIB(HGRIB,KLUOUT,43,IRET,ZSLT)
!--------------------------------------------------------------------------------
ALLOCATE (ZWSAT(SIZE(PFIELD,1)))
ZWSAT(:)=0.
!
IF (IRET == 0) THEN
!        
! 2.1 Convert from specific humidity to relative humidity using soil types
!     --------------------------------------------------------------------
  WHERE (ZSLT(:)==1.) 
    ZWSAT(:) = 0.403
  ELSEWHERE (ZSLT(:)==2.) 
    ZWSAT(:) = 0.439
  ELSEWHERE (ZSLT(:)==3.) 
    ZWSAT(:) = 0.430
  ELSEWHERE (ZSLT(:)==4.) 
    ZWSAT(:) = 0.520
  ELSEWHERE (ZSLT(:)==5.) 
    ZWSAT(:) = 0.614
  ELSEWHERE (ZSLT(:)==6.) 
    ZWSAT(:) = 0.766
  ENDWHERE
!
ELSE
!
! 2.2 Convert from specific humidity to relative humidity single soil type
!     --------------------------------------------------------------------
  ! Compute model's constants
  IF (SIZE(PFIELD,2)==4) THEN
    ZWSAT(:)  = 0.472
  ELSE
    ZWSAT(:)  = 0.286
  END IF
  !
ENDIF
!
! Then perform conversion
DO JL=1,SIZE(PFIELD,2)
  WHERE ( PFIELD(:,JL).NE.XUNDEF .AND. ZWSAT(:).NE.0. ) 
    PFIELD(:,JL) = PFIELD(:,JL) / ZWSAT(:)
  ELSEWHERE
    PFIELD(:,JL) = 0.
  ENDWHERE
ENDDO
!
IF (ASSOCIATED(ZSLT)) DEALLOCATE(ZSLT)
DEALLOCATE(ZWSAT)
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WGI_ECMWF',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_WGI_ECMWF
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_TG_METEO_FRANCE(HGRIB,KLUOUT,HINMODEL,PMASK,PTG,PDT)
!     #######################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PTG       ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PDT       ! thickness of each layer
!* local variables
!  ---------------
REAL,    DIMENSION(:), POINTER    :: ZFIELD => NULL()    ! field to read
INTEGER                           :: JL         ! layer loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TG_METEO_FRANCE',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_TG_METEO_FRANCE: | Reading soil temperature'
!--------------------------------------------------------------------------------
! 1.  Allocate soil temperature profile
!     ---------------------------------
!--------------------------------------------------------------------------------
! 2.  Search and read level 1 (and its depth)
!     ---------------------------------------
 CALL READ_GRIB_TS(HGRIB,KLUOUT,HINMODEL,PMASK,ZFIELD)
!
ALLOCATE(PTG(SIZE(ZFIELD),3))
ALLOCATE(PDT(SIZE(ZFIELD),3))
!
PTG(:,1) = ZFIELD(:)
PDT(:,1) = 0.01
!--------------------------------------------------------------------------------
! 3.  Deep soil temperature
!     ---------------------
 CALL READ_GRIB_T2_LAND(HGRIB,KLUOUT,HINMODEL,PMASK,ZFIELD)
!
PTG(:,2) = ZFIELD(:)
PDT(:,2) = 0.4         ! deep temperature layer depth assumed equal to 0.4m
DEALLOCATE(ZFIELD)
!--------------------------------------------------------------------------------
! 4.  Assumes uniform temperature profile below
!     -----------------------------------------
PTG(:,3) = PTG(:,2)
PDT(:,3) = 5.          ! temperature profile down to 5m
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TG_METEO_FRANCE',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_TG_METEO_FRANCE
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_SAND_CLAY_METEO_FRANCE(HGRIB,KLUOUT,HINMODEL,PSAND,PCLAY,GISBA)
!     ######################
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), POINTER         :: PSAND     ! field to initialize
REAL, DIMENSION(:), POINTER         :: PCLAY     ! thickness of each layer
LOGICAL, INTENT(OUT)                :: GISBA     ! T: surface scheme in file is ISBA
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
INTEGER                           :: IPAR      ! parameter number for field reading
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! 1.  Search and read clay fraction if available
!     ------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SAND_CLAY_METEO_FRANCE',0,ZHOOK_HANDLE)
!
IF (HINMODEL == 'ARPEGE' .OR. HINMODEL == 'MOCAGE') IPAR=171
IF (HINMODEL == 'ALADIN') IPAR=128
 CALL READ_GRIB(HGRIB,KLUOUT,IPAR,IRET,PCLAY)
!
! if not available, the model is not ISBA (IWMODE=1)
IF (IRET /= 0) THEN
  GISBA = .FALSE.
ELSE
  GISBA = .TRUE.
  PCLAY(:) = PCLAY(:) / 100. ! this field is given in percent
  WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_SAND_CLAY_METEO_FRANCE: | The soil model is ISBA'
END IF
!-------------------------------------------------------------------------------
! 2.  Search and read sand fraction if available
!     ------------------------------------------
IF (HINMODEL == 'ARPEGE' .OR. HINMODEL == 'MOCAGE') IPAR=172
IF (HINMODEL == 'ALADIN') IPAR=129
 CALL READ_GRIB(HGRIB,KLUOUT,IPAR,IRET,PSAND)
!
! if not available, the model is not ISBA (IWMODE=1)
IF (GISBA) THEN
  IF (IRET /= 0) THEN
    CALL ABOR1_SFX('MODE_READ_GRIB: SAND FRACTION MISSING (READ_GRIB_SAND_CLAY_METEO_FRANCE)')
  ELSE
    PSAND(:) = PSAND(:) / 100. ! this field is given in percent
  END IF
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SAND_CLAY_METEO_FRANCE',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_SAND_CLAY_METEO_FRANCE
!-----------------------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_WG_METEO_FRANCE(HGRIB,KLUOUT,HINMODEL,PMASK,PFIELD,PD)
!     #######################
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!* local variables
!  ---------------
LOGICAL                           :: GISBA     ! T: surface scheme in file is ISBA
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
INTEGER                           :: ILTYPE    ! type of level (Grib code table 3)
INTEGER                           :: ILEV1     ! level definition
INTEGER                           :: ILEV2     ! level definition
REAL,  DIMENSION(:),   POINTER    :: ZCLAY => NULL()     ! clay fraction
REAL,  DIMENSION(:),   POINTER    :: ZSAND => NULL()     ! sand fraction
REAL,  DIMENSION(:),   POINTER    :: ZFIELD => NULL()
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWWILT     ! wilting point
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWFC       ! field capacity
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWSAT      ! saturation
INTEGER                           :: JL         ! layer loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WG_METEO_FRANCE',0,ZHOOK_HANDLE)
!
! 1.  Search and read clay and sand fractions if available
!     ----------------------------------------------------
 CALL READ_GRIB_SAND_CLAY_METEO_FRANCE(HGRIB,KLUOUT,HINMODEL,ZSAND,ZCLAY,GISBA)
!-------------------------------------------------------------------------------
IF (GISBA) THEN
  ALLOCATE(PFIELD(SIZE(ZSAND),3))
  ALLOCATE(PD(SIZE(ZSAND),3))
ELSE
  ALLOCATE(PFIELD(NNI,3))
  ALLOCATE(PD(NNI,3))
ENDIF
!
PD(:,1) = 0.01
PD(:,2) = 0.20
!-------------------------------------------------------------------------------
! 2.  Read layer 1 moisture
!     ---------------------
ILEV1   = 0
IF (HINMODEL == 'ARPEGE' .OR. HINMODEL=='MOCAGE') THEN
  ILTYPE  = 112     
  ILEV2   = 1        
  CALL READ_GRIB(HGRIB,KLUOUT,153,IRET,ZFIELD,KLEV1=ILEV1,KLEV2=ILEV2)
ELSE
  ILTYPE  = 105
  ILEV2   = 0
  CALL READ_GRIB(HGRIB,KLUOUT,86,IRET,ZFIELD,KLEV1=ILEV1,KLEV2=ILEV2)
ENDIF
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL MOISTURE LEVEL 1 MISSING (READ_GRIB_WG_METEO_FRANCE)')
END IF
!
PFIELD(:,1) = ZFIELD(:)
!-------------------------------------------------------------------------------
! 3.  Read layer 2 moisture
!     ---------------------
IF (HINMODEL == 'ARPEGE' .OR. HINMODEL=='MOCAGE') THEN
  ILTYPE  = 112  
  ILEV1   = 0  
  ILEV2   = 250             
  CALL READ_GRIB(HGRIB,KLUOUT,153,IRET,ZFIELD,KLEV1=ILEV1,KLEV2=ILEV2)
ELSE
  ILTYPE  = 111
  ILEV1   = -1
  ILEV2   = -1        
  CALL READ_GRIB(HGRIB,KLUOUT,86,IRET,ZFIELD,KLEV1=ILEV1,KLEV2=ILEV2)
ENDIF
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL MOISTURE LEVEL 2 MISSING (READ_GRIB_WG_METEO_FRANCE)')
END IF
!        
PFIELD(:,2) = ZFIELD(:)
!-------------------------------------------------------------------------------
! 4.  Read layer 2 depth (ISBA only)
!     -----------------------------
!* note that soil water reservoir is considered uniform between 0.2m and GRIB soil depth
IF (GISBA) THEN
  IF (HINMODEL == 'ARPEGE' .OR. HINMODEL == 'MOCAGE') THEN
    CALL READ_GRIB(HGRIB,KLUOUT,173,IRET,ZFIELD)
  ELSE
    CALL READ_GRIB(HGRIB,KLUOUT,130,IRET,ZFIELD)
  ENDIF
  IF (IRET /= 0) THEN
    CALL ABOR1_SFX('MODE_READ_GRIB: SOIL MOISTURE LEVEL 2 DEPTH MISSING (READ_GRIB_WG_METEO_FRANCE)')          
  END IF
  PD(:,3) = ZFIELD(:)
  DEALLOCATE(ZFIELD)
ELSE
  PD(:,3) = 2.
END IF
!-------------------------------------------------------------------------------
! 5.  Compute relative humidity from units kg/m^2
!     -------------------------------------------
! Compute ISBA model constants (if needed)
IF (GISBA) THEN
  !
  !* updates Wg in m3/m3
  PFIELD(:,1) = PFIELD(:,1) / 10.
  PFIELD(:,2) = PFIELD(:,2) /(1000. * PD(:,3))
  !
  ALLOCATE (ZWSAT (SIZE(ZSAND)))
  ZWSAT (:) = (-1.08*100. * ZSAND(:) + 494.305) * 0.001
  PFIELD(:,1) = MAX(MIN(PFIELD(:,1),ZWSAT(:)),0.)
  PFIELD(:,2) = MAX(MIN(PFIELD(:,2),ZWSAT(:)),0.)
  DEALLOCATE(ZWSAT)
  DEALLOCATE (ZSAND)
  !
  ALLOCATE (ZWWILT(SIZE(ZCLAY)))
  ALLOCATE (ZWFC  (SIZE(ZCLAY)))
  ZWWILT(:) = 37.1342E-3 * SQRT( 100. * ZCLAY(:) )
  ZWFC  (:) = 89.0467E-3 * (100. * ZCLAY(:) )**0.3496
  PFIELD(:,1) = (PFIELD(:,1) - ZWWILT(:)) / (ZWFC(:) - ZWWILT(:))
  PFIELD(:,2) = (PFIELD(:,2) - ZWWILT(:)) / (ZWFC(:) - ZWWILT(:))
  DEALLOCATE (ZWWILT)
  DEALLOCATE (ZWFC)
  DEALLOCATE (ZCLAY)
  !
ELSE ! Non ISBA
 !
  PFIELD(:,2) = (PFIELD(:,1)+PFIELD(:,2)) / (20. + 100.) 
  PFIELD(:,1) =  PFIELD(:,1)           /  20.
  !
END IF
!
PFIELD(:,3) = PFIELD(:,2)
!--------------------------------------------------------------------------------
! 6.  Apply land mask
!     ---------------
IF (SIZE(PMASK)==SIZE(PFIELD,1)) THEN
  DO JL=1,SIZE(PFIELD,2)
    WHERE (PMASK(:)/=1.) PFIELD(:,JL) = XUNDEF
  END DO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WG_METEO_FRANCE',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_WG_METEO_FRANCE
!--------------------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_WGI_METEO_FRANCE(HGRIB,KLUOUT,HINMODEL,PMASK,PFIELD,PD)
!     #######################
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!* local variables
!  ---------------
LOGICAL                           :: GISBA     ! T: surface scheme in file is ISBA
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
INTEGER                           :: ILTYPE    ! type of level (Grib code table 3)
INTEGER                           :: ILEV1     ! level definition
INTEGER                           :: ILEV2     ! level definition
REAL,  DIMENSION(:),   POINTER    :: ZCLAY => NULL()     ! clay fraction
REAL,  DIMENSION(:),   POINTER    :: ZSAND => NULL()     ! sand fraction
REAL,  DIMENSION(:),   POINTER    :: ZFIELD => NULL()
REAL,  DIMENSION(:),   ALLOCATABLE:: ZWSAT      ! saturation
INTEGER                           :: JL         ! layer loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WGI_METEO_FRANCE',0,ZHOOK_HANDLE)
!
! 1.  Search and read clay fraction if available
!     ------------------------------------------
 CALL READ_GRIB_SAND_CLAY_METEO_FRANCE(HGRIB,KLUOUT,HINMODEL,ZSAND,ZCLAY,GISBA)
!-------------------------------------------------------------------------------
IF (GISBA) THEN
  ALLOCATE(PFIELD(SIZE(ZSAND),2))
  ALLOCATE(PD(SIZE(ZSAND),2))
ELSE
  ALLOCATE(PFIELD(NNI,2))
  ALLOCATE(PD(NNI,2))
ENDIF
!
PD(:,1) = 0.01
!-------------------------------------------------------------------------------
! 2.  Read layer 1 soil ice
!     ---------------------
ILEV1   = 0  
IF (HINMODEL == 'ARPEGE' .OR. HINMODEL=='MOCAGE') THEN
  ILTYPE  = 112     
  ILEV2   = 1
  CALL READ_GRIB(HGRIB,KLUOUT,152,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
ELSE
  ILTYPE  = 105      
  ILEV2   = 0        
  CALL READ_GRIB(HGRIB,KLUOUT,139,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
END IF
!
IF (IRET == 0) THEN
  PFIELD(:,1) = ZFIELD(:)
  WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_WGI_METEO_FRANCE: -> Soil ice level 1 is present'
ELSE
  PFIELD(:,1) = 0.
END IF
!-------------------------------------------------------------------------------
! 3.  Read layer 2 soil ice
!     ---------------------
IF (HINMODEL == 'ARPEGE' .OR. HINMODEL=='MOCAGE') THEN
  ILTYPE  = 112
  ILEV1   = 0        
  ILEV2   = 250
  CALL READ_GRIB(HGRIB,KLUOUT,152,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
ELSE
  ILTYPE  = 111
  ILEV1   = -1        
  ILEV2   = -1        
  CALL READ_GRIB(HGRIB,KLUOUT,139,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
END IF
!
IF (IRET == 0) THEN
  PFIELD(:,2) = ZFIELD(:)     
  WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_WGI_METEO_FRANCE: -> Soil ice level 2 is present'
ELSE
  PFIELD(:,2) = 0.
END IF
!-------------------------------------------------------------------------------
! 4.  Read layer 2 depth (ISBA only)
!     ------------------------------
IF (GISBA) THEN
  IF (HINMODEL == 'ARPEGE' .OR. HINMODEL=='MOCAGE') THEN 
    CALL READ_GRIB(HGRIB,KLUOUT,173,IRET,ZFIELD)
  ELSE
    CALL READ_GRIB(HGRIB,KLUOUT,130,IRET,ZFIELD)
  ENDIF
  IF (IRET /= 0) THEN
    CALL ABOR1_SFX('MODE_READ_GRIB: SOIL ICE LEVEL 2 MISSING (READ_GRIB_WGI_METEO_FRANCE)')
  END IF
  PD(:,2) = ZFIELD(:)
  DEALLOCATE(ZFIELD)
ELSE
  PD(:,2) = 2.
END IF
!-------------------------------------------------------------------------------
! 5.  Compute relative humidity from units kg/m^2
!     -------------------------------------------
IF (GISBA) THEN
  !
  !* updates Wgi in m3/m3
  PFIELD(:,1) = PFIELD(:,1) / 10.
  PFIELD(:,2) = PFIELD(:,2) /(1000. * PD(:,2))
  !        
  ALLOCATE (ZWSAT (NNI))
  ZWSAT (:) = (-1.08*100. * ZSAND(:) + 494.305) * 0.001
  PFIELD(:,1) = PFIELD(:,1) / ZWSAT(:)
  PFIELD(:,2) = PFIELD(:,2) / ZWSAT(:)
  DEALLOCATE (ZWSAT) 
  DEALLOCATE (ZSAND)
  DEALLOCATE (ZCLAY)
  !
ELSE ! Non ISBA
  !
  PFIELD(:,1) = 0.
  PFIELD(:,2) = 0.
  !
END IF
!--------------------------------------------------------------------------------
! 6.  Apply land mask
!     ---------------
IF (SIZE(PMASK)==SIZE(PFIELD,1)) THEN
  DO JL=1,SIZE(PFIELD,2)
    WHERE (PMASK(:)/=1.) PFIELD(:,JL) = XUNDEF
  END DO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WGI_METEO_FRANCE',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_WGI_METEO_FRANCE
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_TG_HIRLAM(HGRIB,KLUOUT,HINMODEL,PMASK,PTG,PDT)
!     #######################
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PTG       ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PDT       ! thickness of each layer
!
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
INTEGER                           :: ILEV1     ! level definition
INTEGER                           :: ILEV2     ! level definition
REAL,  DIMENSION(:),   POINTER    :: ZFIELD => NULL()
INTEGER                           :: JL         ! layer loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TG_HIRLAM',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_TG_HIRLAM: | Reading soil temperature'
!--------------------------------------------------------------------------------
! 1.  Search and read level 1 (and its depth)
!     -----------------------
ILEV1 = 904
ILEV2 = -1
 CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,ZFIELD,KLEV1=ILEV1,KLEV2=ILEV2)
IF (IRET /= 0 ) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL TEMPERATURE LEVEL 1 MISSING (READ_GRIB_TG_HIRLAM)')
END IF
!
ALLOCATE(PTG(SIZE(ZFIELD),3))
ALLOCATE(PDT(SIZE(ZFIELD),3))
PTG(:,1)= ZFIELD(:)
PDT(:,1) = 0.01
!--------------------------------------------------------------------------------
! 2.  Deep soil temperature
!     ---------------------
WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_TG_HIRLAM: | Reading deep soil temperature'
!
ILEV1 = 954
ILEV2 = -1
 CALL READ_GRIB(HGRIB,KLUOUT,11,IRET,ZFIELD,KLEV1=ILEV1,KLEV2=ILEV2)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: DEEP SOIL TEMPERATURE MISSING (READ_GRIB_TG_HIRLAM)')
END IF
!
PTG(:,2)= ZFIELD(:)
DEALLOCATE(ZFIELD)
PDT(:,2) = 0.4         ! deep temperature layer depth assumed equal to 0.40m
!--------------------------------------------------------------------------------
! 4.  Assumes uniform temperature profile below
!     -----------------------------------------
PTG(:,3) = PTG(:,2)
PDT(:,3) = 5.          ! temperature profile down to 5m
!--------------------------------------------------------------------------------
! 5.  Apply land mask
!     ---------------
IF (SIZE(PMASK)==SIZE(PTG,1)) THEN
  DO JL=1,SIZE(PTG,2)
    WHERE (PMASK(:)/=1.) PTG(:,JL) = XUNDEF
  END DO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TG_HIRLAM',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_TG_HIRLAM
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_WG_HIRLAM(HGRIB,KLUOUT,HINMODEL,PMASK,PFIELD,PD)
!     #######################
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:), INTENT(IN)      :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
INTEGER                           :: ILTYPE    ! type of level (Grib code table 3)
INTEGER                           :: ILEV1     ! level definition
INTEGER                           :: ILEV2     ! level definition
REAL, DIMENSION(:), POINTER       :: ZFIELD => NULL()
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWG        ! first water reservoir
REAL, DIMENSION(:), ALLOCATABLE   :: ZD         ! Height of each layer
INTEGER                           :: INLAYERDEEP! number of deep moisture layers
REAL                              :: ZWWILT     ! ECMWF wilting point
REAL                              :: ZWFC       ! ECMWF field capacity
REAL                              :: ZWSAT      ! ECMWF saturation
INTEGER                           :: JL         ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WG_HIRLAM',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_WG_HIRLAM: | Reading soil moisture'
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_WG_HIRLAM: | WARNING READING LOW VEGETATION TILE (NR 4) ONLY'
!
ALLOCATE(ZD(2))
!
! 1.  Search and read level 1 (and its depth)
!     -----------------------
ILTYPE=105
ILEV1=904
ILEV2=-1
 CALL READ_GRIB(HGRIB,KLUOUT,86,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
IF (IRET /= 0 ) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL MOISTURE LEVEL 1 MISSING (READ_GRIB_WG_HIRLAM)')
END IF
!
ALLOCATE(ZWG(SIZE(ZFIELD),2))
ZWG(:,1)=ZFIELD
!
ZD(1) = 0.01
WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_WG_HIRLAM: | Level 1 height set to 0.01 m '
!
ZWG(:,1) = ZWG(:,1) / ZD(1)      ! convert units to m3/m3  
!--------------------------------------------------------------------------------
! 2.  Search and read level 2 (and its depth) This level is optionnal
!     -----------------------
ILTYPE=105
ILEV1=954
ILEV2=-1
 CALL READ_GRIB(HGRIB,KLUOUT,86,IRET,ZFIELD,KLTYPE=ILTYPE,KLEV1=ILEV1,KLEV2=ILEV2)
IF (IRET /= 0 ) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: SOIL MOISTURE LEVEL 2 MISSING (READ_GRIB_WG_HIRLAM)')
END IF
!
ZWG(:,2)=ZFIELD ! already units m3/m3 
DEALLOCATE(ZFIELD)
!
INLAYERDEEP = 2
ZD(2) = 0.42
WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_WG_HIRLAM: | Level 2 height set to 0.42 m '
!
WRITE  (KLUOUT,'(A)') 'WARNING MODE_READ_GRIB: ZWG3 AND ZWG4 SET TO 0. (READ_GRIB_WG_HIRLAM)'
!--------------------------------------------------------------------------------
! 3.  Set water content profile and layer thicknesses
!     -----------------------------------------------
 CALL FILL_PFIELD(KLUOUT,'READ_GRIB_WG_HIRLAM',INLAYERDEEP,ZD,ZWG,PMASK,PFIELD,PD)
DEALLOCATE(ZD)
DEALLOCATE(ZWG)
!--------------------------------------------------------------------------------
! 4.  Convert from specific humidity to relative humidity
!     ---------------------------------------------------
! Compute model's constants
ZWFC   = 0.171
ZWWILT = 0.086
!
! Then perform conversion
DO JL=1,INLAYERDEEP
  WHERE (PFIELD(:,JL).NE.XUNDEF) PFIELD(:,JL) = (PFIELD(:,JL) - ZWWILT) / (ZWFC - ZWWILT)
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WG_HIRLAM',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_WG_HIRLAM
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_WGI_HIRLAM(HGRIB,KLUOUT,PFIELD,PD)
!     #######################
!
USE MODD_GRID_GRIB,  ONLY : NNI
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!* local variables
!  ---------------
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WGI_HIRLAM',0,ZHOOK_HANDLE)
!
ALLOCATE (PFIELD(NNI,2))
ALLOCATE (PD    (NNI,2))
PFIELD(:,:) = 0.
!
PD    (:,1) = 0.01
PD    (:,2) = 1.
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_WGI_HIRLAM',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_WGI_HIRLAM
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_SNOW_VEG_AND_DEPTH(HGRIB,KLUOUT,HINMODEL,PMASK,PSNV,PSNVD)
!     #######################
!!
!!    MODIFICATIONS
!!    -------------
!!    C Ardilouze 07/2013 : possibility to read snow density (ERAI-land)
!!
!-------------------------------------------------------------------------------
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XRHOSMAX
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:), OPTIONAL, POINTER :: PSNV    ! field to initialize
REAL, DIMENSION(:), OPTIONAL, POINTER :: PSNVD   ! field to initialize
!
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)                           :: IRET      ! return code
REAL, DIMENSION(:), POINTER       :: ZFIELD => NULL()    ! field to initialize
REAL, DIMENSION(:), POINTER       :: ZFIELD2 => NULL()    ! field to initialize
REAL, DIMENSION(:), POINTER       :: ZRHO => NULL()
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SNOW_VEG_AND_DEPTH',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_SNOW_VEG_AND_DEPTH: | Reading snow depth and density (if present)'
!
SELECT CASE(HINMODEL)
  CASE('ECMWF ')
    CALL READ_GRIB(HGRIB,KLUOUT,141,IRET,ZFIELD)
  CASE('ARPEGE','ALADIN','MOCAGE','HIRLAM')
    CALL READ_GRIB(HGRIB,KLUOUT,66,IRET,ZFIELD)          
  CASE DEFAULT
    CALL ABOR1_SFX('MODE_READ_GRIB:READ_GRIB_SNOW_VEG_AND_DEPTH: OPTION NOT SUPPORTED '//HINMODEL)
END SELECT
!
IF (IRET /= 0 ) THEN
  CALL ABOR1_SFX('MODE_READ_GRIB: SNOW AND VEG DEPTH MISSING (READ_GRIB_SNOW_VEG_AND_DEPTH)')
END IF
!
CALL READ_GRIB_SNOW_DEN(HGRIB,KLUOUT,HINMODEL,PMASK,ZRHO)
!
IF (PRESENT(PSNV)) THEN
  ALLOCATE(PSNV(SIZE(ZFIELD)))
  PSNV(:)=ZFIELD(:)
  IF (HINMODEL=='ECMWF ') PSNV(:) = PSNV(:) * ZRHO(:)
  IF (SIZE(PMASK)==SIZE(PSNV)) &
    WHERE (PMASK(:)/=1.) PSNV(:) = XUNDEF
ENDIF
!
IF (PRESENT(PSNVD)) THEN
  ALLOCATE(PSNVD(SIZE(ZFIELD)))
  PSNVD(:)=ZFIELD(:)
  IF (HINMODEL/='ECMWF ') PSNVD = PSNVD / ZRHO(:)
  IF (SIZE(PMASK)==SIZE(PSNVD)) &
    WHERE (PMASK(:)/=1.) PSNVD(:) = XUNDEF
ENDIF
!
DEALLOCATE(ZFIELD)
DEALLOCATE(ZRHO)
!
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SNOW_VEG_AND_DEPTH',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_SNOW_VEG_AND_DEPTH
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_SNOW_ALB(HGRIB,KLUOUT,HINMODEL,PMASK,PSNVA)
!     #######################
!!
!!    AUTHOR
!!    -------------
!!    C Ardilouze 07/2013 : possibility to read snow albedo (ERAI-land)
!!
!-------------------------------------------------------------------------------
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XANSMIN, XANSMAX
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB    ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT   ! logical unit of output listing
CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK    ! grib land mask
REAL, DIMENSION(:), POINTER         :: PSNVA    ! field to initialize
!
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)           :: IRET      ! return code
REAL, DIMENSION(:), POINTER       :: ZFIELD => NULL()    ! field to initialize 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SNOW_ALB',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_SNOW_ALB: | Reading snow albedo'
!
ALLOCATE(PSNVA(NNI))
PSNVA(:) = 0.5 * ( XANSMIN + XANSMAX )
IF (HINMODEL == 'ECMWF') THEN
  CALL READ_GRIB(HGRIB,KLUOUT,32,IRET,ZFIELD)
  IF (IRET == 0 ) THEN
    DEALLOCATE(PSNVA)      
    ALLOCATE(PSNVA(SIZE(ZFIELD)))
    PSNVA(:)=ZFIELD(:)
    DEALLOCATE(ZFIELD)
  END IF
END IF 
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SNOW_ALB',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_SNOW_ALB
!!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_SNOW_DEN(HGRIB,KLUOUT,HINMODEL,PMASK,PSNV)
!     #######################
!!
!!    AUTHOR
!!    -------------
!!    C Ardilouze 08/2013 : possibility to read snow density (ERAI-land)
!!
!-------------------------------------------------------------------------------
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XRHOSMAX
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB    ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT   ! logical unit of output listing
CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL ! Grib originating model
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK    ! grib land mask
REAL, DIMENSION(:), POINTER         :: PSNV    ! field to initialize
!
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)           :: IRET      ! return code
REAL, DIMENSION(:), POINTER       :: ZFIELD => NULL()    ! field to initialize 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SNOW_DEN',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_SNOW_DEN: | Reading snow density'
!
ALLOCATE(PSNV(NNI))
PSNV(:) = XRHOSMAX
IF (HINMODEL == 'ECMWF') THEN
  CALL READ_GRIB(HGRIB,KLUOUT,33,IRET,ZFIELD)
  IF (IRET == 0 ) THEN
    DEALLOCATE(PSNV)      
    ALLOCATE(PSNV(SIZE(ZFIELD)))
    PSNV(:)=ZFIELD(:)
    DEALLOCATE(ZFIELD)
  END IF
END IF 
!
IF (SIZE(PMASK)==SIZE(PSNV)) &
  WHERE (PMASK(:)/=1.) PSNV = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_SNOW_DEN',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_SNOW_DEN
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_T_TEB(HGRIB,KLUOUT,HINMODEL,PTI,PMASK,PT,PD)
!     #######################
!
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL,                 INTENT(IN)    :: PTI       ! internal temperature
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PT        ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! normalized grid
!
!* local variables
!  ---------------
REAL, DIMENSION(:), POINTER       :: ZFIELD => NULL()    ! field to initialize
INTEGER                           :: JL         ! layer loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_T_TEB',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_T_TEB: | Reading temperature for buildings'
!
 CALL READ_GRIB_TS(HGRIB,KLUOUT,HINMODEL,PMASK,ZFIELD)
!
ALLOCATE(PT(SIZE(ZFIELD),3))
ALLOCATE(PD(SIZE(ZFIELD),3))
!
PT(:,1) = ZFIELD
DEALLOCATE(ZFIELD)
PD(:,1) = 0.
!
PT(:,2) = PTI
PD(:,2) = 0.5         ! deep temperature depth assumed at half of wall or roof
!
PT(:,3) = PTI
PD(:,3) = 1.          ! temperature at building interior
!
IF (SIZE(PMASK)==SIZE(PT,1)) THEN
  DO JL=1,SIZE(PT,2)
    WHERE (PMASK(:)/=1.) PT(:,JL) = XUNDEF
  END DO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_T_TEB',1,ZHOOK_HANDLE)
END SUBROUTINE READ_GRIB_T_TEB
!-------------------------------------------------------------------
!     #######################
      SUBROUTINE READ_GRIB_TF_TEB(HGRIB,KLUOUT,HINMODEL,PTI,PMASK,PTF,PD)
!     #######################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
 CHARACTER(LEN=*),     INTENT(IN)    :: HGRIB     ! Grib file name
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6),     INTENT(IN)    :: HINMODEL  ! Grib originating model
REAL,                 INTENT(IN)    :: PTI       ! internal temperature
REAL, DIMENSION(:),   INTENT(IN)    :: PMASK     ! grib land mask
REAL, DIMENSION(:,:), POINTER       :: PTF       ! field to initialize
REAL, DIMENSION(:,:), POINTER       :: PD        ! thickness of each layer
!
!* local variables
!  ---------------
INTEGER(KIND=kindOfInt)           :: IRET      ! return code
REAL,    DIMENSION(:), POINTER    :: ZFIELD => NULL()    ! field to read
INTEGER                           :: JL         ! layer loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TF_TEB',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_TF_TEB: | Reading temperature for building floor'
!
! 1.  Deep soil temperature
!     ---------------------
!
WRITE (KLUOUT,'(A)') 'MODE_READ_GRIB:READ_GRIB_TF_TEB: | Reading deep soil temperature'
!
 CALL READ_GRIB_T2_LAND(HGRIB,KLUOUT,HINMODEL,PMASK,ZFIELD)
!
ALLOCATE(PTF(SIZE(ZFIELD),3))
ALLOCATE(PD (SIZE(ZFIELD),3))
!
PTF(:,2) = ZFIELD(:)
PD (:,2) = 0.5           ! deep temperature depth assumed at half of the floor
!
DEALLOCATE(ZFIELD)
!
! 2.  level 1 is the internal building temperature
!     -----------------------
!
PTF(:,1) = PTI
PD (:,1) = 0.
!
! 3.  Assumes uniform temperature profile below
!     -----------------------------------------
!
PTF(:,3) = PTF(:,2)
PD (:,3) = 1.          ! deep temperature value
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_GRIB:READ_GRIB_TF_TEB',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
END SUBROUTINE READ_GRIB_TF_TEB
!-------------------------------------------------------------------
END MODULE MODE_READ_GRIB
