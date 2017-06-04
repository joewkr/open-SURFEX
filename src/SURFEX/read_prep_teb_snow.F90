!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_TEB_SNOW(HPROGRAM,HSNOW_ROOF,KSNOW_ROOF,HSNOW_ROAD,KSNOW_ROAD,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE)
!     #######################################################
!
!!****  *READ_PREP_TEB_SNOW* - routine to read the configuration for snow
!!                              in TEB fields preparation
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
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2007 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_PREP_TEB_SNOW
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODE_POS_SURF
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODD_CSTS,     ONLY : XTT
USE MODD_SNOW_PAR, ONLY : XANSMIN, XRHOSMAX
USE MODD_PREP_TEB, ONLY : XWSNOW_ROOF_p=>XWSNOW_ROOF, XTSNOW_ROOF_p=>XTSNOW_ROOF, XLWCSNOW_ROOF_p=>XLWCSNOW_ROOF, &
                          XRSNOW_ROOF_p=>XRSNOW_ROOF, XASNOW_ROOF, &
                          XWSNOW_ROAD_p=>XWSNOW_ROAD, XTSNOW_ROAD_p=>XTSNOW_ROAD, XLWCSNOW_ROAD_p=>XLWCSNOW_ROAD,&
                          XRSNOW_ROAD_p=>XRSNOW_ROAD, XASNOW_ROAD, &
                          CFILE_SNOW_TEB, CTYPE_SNOW, CFILEPGD_SNOW_TEB, &
                           CTYPEPGD_SNOW, LSNOW_IDEAL_TEB
!
USE MODD_PREP_SNOW, ONLY : NSNOW_LAYER_MAX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling TEB
 CHARACTER(LEN=3),  INTENT(OUT) :: HSNOW_ROOF ! snow scheme for roofs
 CHARACTER(LEN=3),  INTENT(OUT) :: HSNOW_ROAD ! snow scheme for roads
INTEGER,           INTENT(OUT) :: KSNOW_ROOF ! snow scheme layers for roofs
INTEGER,           INTENT(OUT) :: KSNOW_ROAD ! snow scheme layers for roads
 CHARACTER(LEN=28), OPTIONAL, INTENT(OUT) :: HFILE        ! file name
 CHARACTER(LEN=6),  OPTIONAL, INTENT(OUT) :: HFILETYPE    ! file type
 CHARACTER(LEN=28), OPTIONAL, INTENT(OUT) :: HFILEPGD       ! file name
 CHARACTER(LEN=6),  OPTIONAL, INTENT(OUT) :: HFILEPGDTYPE    ! file type
 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(NSNOW_LAYER_MAX) :: XWSNOW_ROAD, XRSNOW_ROAD, XTSNOW_ROAD, XLWCSNOW_ROAD, &
                                    XWSNOW_ROOF, XRSNOW_ROOF, XTSNOW_ROOF, XLWCSNOW_ROOF
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output file logical unit
INTEGER           :: ILUNAM         ! namelist file logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
NAMELIST/NAM_PREP_TEB_SNOW/CSNOW_ROOF, CSNOW_ROAD, CFILE_SNOW_TEB, CTYPE_SNOW, &
                           LSNOW_IDEAL_TEB, CFILEPGD_SNOW_TEB, CTYPEPGD_SNOW,      & 
                           XWSNOW_ROOF, XTSNOW_ROOF, XLWCSNOW_ROOF, XRSNOW_ROOF, XASNOW_ROOF, &
                           XWSNOW_ROAD, XTSNOW_ROAD, XLWCSNOW_ROAD, XRSNOW_ROAD, XASNOW_ROAD
!-------------------------------------------------------------------------------
!
!* default
!  -------
!

IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_SNOW',0,ZHOOK_HANDLE)
IF (LNAM_READ) THEN
  !
  CSNOW_ROOF = '1-L'
  CSNOW_ROAD = '1-L'
  !
  CFILE_SNOW_TEB = '                         '
  CTYPE_SNOW     = '      ' 
  CFILEPGD_SNOW_TEB = '                         '
  CTYPEPGD_SNOW     = '      '    
  !
  XWSNOW_ROOF(:) = 0.
  XTSNOW_ROOF(:) = XTT
  XLWCSNOW_ROOF(:) = 0.
  XRSNOW_ROOF(:) = XRHOSMAX
  XASNOW_ROOF = XANSMIN
  !
  XWSNOW_ROAD(:) = 0.
  XTSNOW_ROAD(:) = XTT
  XLWCSNOW_ROAD(:) = 0.
  XRSNOW_ROAD(:) = XRHOSMAX
  XASNOW_ROAD = XANSMIN
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
  !
  !* reading of namelist
  !  -------------------
  !
  !
  CALL POSNAM(ILUNAM,'NAM_PREP_TEB_SNOW',GFOUND,ILUOUT)
  IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PREP_TEB_SNOW)
  !
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW_ROOF',CSNOW_ROOF,'1-L')
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW_ROAD',CSNOW_ROAD,'1-L')
  !
  ALLOCATE(XWSNOW_ROOF_p(1))
  ALLOCATE(XRSNOW_ROOF_p(1))
  ALLOCATE(XTSNOW_ROOF_p(1))
  ALLOCATE(XLWCSNOW_ROOF_p(1))
  !
  XWSNOW_ROOF_p=XWSNOW_ROOF(1)
  XRSNOW_ROOF_p=XRSNOW_ROOF(1)
  XTSNOW_ROOF_p=XTSNOW_ROOF(1)
  XLWCSNOW_ROOF_p=XLWCSNOW_ROOF(1)
  !
  ALLOCATE(XWSNOW_ROAD_p(1))
  ALLOCATE(XRSNOW_ROAD_p(1))
  ALLOCATE(XTSNOW_ROAD_p(1))
  ALLOCATE(XLWCSNOW_ROAD_p(1))
  !
  XWSNOW_ROAD_p=XWSNOW_ROAD(1)
  XRSNOW_ROAD_p=XRSNOW_ROAD(1)
  XTSNOW_ROAD_p=XTSNOW_ROAD(1)
  XLWCSNOW_ROAD_p=XLWCSNOW_ROAD(1)
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
ENDIF
!
HSNOW_ROOF = CSNOW_ROOF
HSNOW_ROAD = CSNOW_ROAD
KSNOW_ROOF = 1
KSNOW_ROAD = 1
!
IF (LEN_TRIM(CFILE_SNOW_TEB)>0 .AND. LEN_TRIM(CTYPE_SNOW)>0 & 
        .AND.LEN_TRIM(CFILEPGD_SNOW_TEB)>0.AND.LEN_TRIM(CTYPEPGD_SNOW)>0) THEN
  IF (PRESENT(HFILE)) HFILE = CFILE_SNOW_TEB
  IF (PRESENT(HFILETYPE)) HFILETYPE = CTYPE_SNOW
  IF (PRESENT(HFILEPGD)) HFILEPGD = CFILEPGD_SNOW_TEB
  IF (PRESENT(HFILEPGDTYPE)) HFILEPGDTYPE = CTYPEPGD_SNOW  
END IF
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_SNOW',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_TEB_SNOW
