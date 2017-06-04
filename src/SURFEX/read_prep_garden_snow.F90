!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_GARDEN_SNOW(HPROGRAM,HSNOW,KSNOW_LAYER,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,OUNIF)
!     #######################################################
!
!!****  *READ_PREP_GARDEN_SNOW* - routine to read the configuration for snow
!!                              in ISBA fields preparation
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!     A. Bogatchev 09/2005 EBA snow option
!!     V. Vionnet   06/2008 - Flag for snow metamorphism
!                           - Preparation of uniform snow fields : density, temperture,albedo,grain types
!!                          - Flag to avtivate new maximal liquid water holding capacity : formulation used by Crocus
!!     M. Lafaysse  08/2013 init XZSNOW or XLWCSNOW
!      B. Decharme  07/2013 Add ES snow grid case : 6-L or 12-L
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_PREP_GARDEN_SNOW
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XANSMIN, XRHOSMAX
USE MODD_CSTS,       ONLY : XTT
!
USE MODE_POS_SURF
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_ABOR1_SFX
!
USE MODD_PREP_TEB_GARDEN, ONLY : CFILE_SNOW_GD, CTYPE_SNOW, CFILEPGD_SNOW_GD, &
                                 CTYPEPGD_SNOW, LSNOW_IDEAL_GD, &
                                 XWSNOW_p=>XWSNOW_GD, XTSNOW_p=>XTSNOW_GD, XLWCSNOW_p=>XLWCSNOW_GD, &
                                 XRSNOW_p=>XRSNOW_GD, XAGESNOW_p=>XAGESNOW_GD, XASNOW_GD
!
USE MODD_PREP_SNOW, ONLY : NSNOW_LAYER_MAX, LSNOW_PREP_PERM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA
CHARACTER(LEN=3),  INTENT(OUT) :: HSNOW    ! snow scheme
INTEGER, INTENT(OUT)           :: KSNOW_LAYER  ! number of snow layers
CHARACTER(LEN=28), OPTIONAL, INTENT(OUT) :: HFILE        ! file name
CHARACTER(LEN=6),  OPTIONAL, INTENT(OUT) :: HFILETYPE    ! file type
 CHARACTER(LEN=28),OPTIONAL, INTENT(OUT) :: HFILEPGD     ! file name
 CHARACTER(LEN=6), OPTIONAL, INTENT(OUT) :: HFILEPGDTYPE ! file type 
 LOGICAL,          OPTIONAL, INTENT(OUT) :: OUNIF        ! uniform snow
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
CHARACTER(LEN=3) :: CSNOW
INTEGER :: NSNOW_LAYER
CHARACTER(LEN=28) :: CFILE_SNOW, CFILEPGD_SNOW
LOGICAL :: LSNOW_IDEAL, LSNOW_FRAC_TOT, LSWEMAX
REAL :: XASNOW, XSWEMAX
REAL, DIMENSION(NSNOW_LAYER_MAX) :: XWSNOW, XZSNOW, XRSNOW, XTSNOW, XLWCSNOW, XSG1SNOW, XSG2SNOW,&
                                    XHISTSNOW, XAGESNOW
INTEGER           :: JLAYER
!
REAL, DIMENSION(NSNOW_LAYER_MAX) :: XWSNOW_GD, XZSNOW_GD, XRSNOW_GD, XTSNOW_GD, XLWCSNOW_GD, &
                                    XSG1SNOW_GD, XSG2SNOW_GD, XHISTSNOW_GD, XAGESNOW_GD
!
LOGICAL           :: LFILE
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output file logical unit
INTEGER           :: ILUNAM         ! namelist file logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
NAMELIST/NAM_PREP_ISBA_SNOW/CSNOW, NSNOW_LAYER, CFILE_SNOW, CTYPE_SNOW, &
                            CFILEPGD_SNOW, CTYPEPGD_SNOW,               & 
                            LSNOW_IDEAL, LSNOW_FRAC_TOT, LSNOW_PREP_PERM,       &
                            XWSNOW, XZSNOW, XTSNOW, XLWCSNOW, XRSNOW, XASNOW,  &
                            XSG1SNOW, XSG2SNOW, XHISTSNOW, XAGESNOW,    &
                            LSWEMAX,XSWEMAX
NAMELIST/NAM_PREP_GARDEN_SNOW/CSNOW_GD, NSNOW_LAYER_GD, CFILE_SNOW_GD, CTYPE_SNOW, &
                              CFILEPGD_SNOW_GD, CTYPEPGD_SNOW,               & 
                              LSNOW_IDEAL_GD, XWSNOW_GD, XZSNOW_GD, XTSNOW_GD, XLWCSNOW_GD, XRSNOW_GD, XASNOW_GD
!-------------------------------------------------------------------------------
!* default
!  -------
!

IF (LHOOK) CALL DR_HOOK('READ_PREP_GARDEN_SNOW',0,ZHOOK_HANDLE)
IF (LNAM_READ) THEN
  !  
  CSNOW_GD = 'D95'
  NSNOW_LAYER_GD = 1
  !
  CFILE_SNOW_GD    = '                         '
  CTYPE_SNOW    = '      '  
  CFILEPGD_SNOW_GD    = '                         '
  CTYPEPGD_SNOW    = '      '    
  !
  LSNOW_IDEAL_GD = .FALSE.
  LSNOW_PREP_PERM = .TRUE.
  !
  XWSNOW_GD(:) = 0.
  XZSNOW_GD(:) = XUNDEF
  XRSNOW_GD(:) = XRHOSMAX
  XTSNOW_GD(:) = XTT
  XLWCSNOW_GD(:) = 0.
  XASNOW_GD = XANSMIN  
  XSG1SNOW_GD(:) = XUNDEF
  XSG2SNOW(:) = XUNDEF
  XHISTSNOW_GD(:) = XUNDEF
  XAGESNOW_GD(:) = XUNDEF
  !
  LSWEMAX=.FALSE.
  XSWEMAX=500.
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
  !
  !* reading of namelist
  !  -------------------
  !
  !* default can be provided by ISBA scheme variables
  CALL POSNAM(ILUNAM,'NAM_PREP_ISBA_SNOW',GFOUND,ILUOUT)
  IF (GFOUND) THEN
    !
    CSNOW = 'D95'
    NSNOW_LAYER = 1
    CFILE_SNOW = '                         '
    CFILEPGD_SNOW = '                         '
    LSNOW_IDEAL = .FALSE.
    LSNOW_FRAC_TOT = .FALSE.
    XWSNOW(:) = XUNDEF
    XZSNOW(:) = XUNDEF
    XRSNOW(:) = XRHOSMAX
    XTSNOW(:) = XTT
    XASNOW = XANSMIN  
    XSG1SNOW(:) = XUNDEF
    XSG2SNOW(:) = XUNDEF
    XHISTSNOW(:) = XUNDEF
    XAGESNOW(:) = XUNDEF 
    XLWCSNOW(:) = 0. 
    !       
    READ(UNIT=ILUNAM,NML=NAM_PREP_ISBA_SNOW)
    CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW',CSNOW,'D95','3-L','EBA','NON','CRO')
    !
    CSNOW_GD = CSNOW
    NSNOW_LAYER_GD = NSNOW_LAYER
    CFILE_SNOW_GD = CFILE_SNOW
    CFILEPGD_SNOW_GD = CFILEPGD_SNOW
    LSNOW_IDEAL_GD = LSNOW_IDEAL
    XWSNOW_GD(:) = XWSNOW(:)
    XZSNOW_GD(:) = XZSNOW(:)    
    XRSNOW_GD(:) = XRSNOW(:)
    XTSNOW_GD(:) = XTSNOW(:)
    XLWCSNOW_GD(:) = XLWCSNOW(:)    
    XASNOW_GD = XASNOW
    XSG1SNOW_GD(:) = XSG1SNOW(:)
    XSG2SNOW_GD(:) = XSG2SNOW(:)
    XHISTSNOW_GD(:) = XHISTSNOW(:)
    XAGESNOW_GD(:) = XAGESNOW(:)
    !
  ENDIF
  !
  !* It is erased by GARDEN namelist if specified
  CALL POSNAM(ILUNAM,'NAM_PREP_GARDEN_SNOW',GFOUND,ILUOUT)
  IF (GFOUND) THEN
    READ(UNIT=ILUNAM,NML=NAM_PREP_GARDEN_SNOW)
    !crocus can't be used in garden if not used in isba scheme
    CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW',CSNOW_GD,'D95','3-L','EBA','NON','CRO')
  ENDIF
  !
  IF (CSNOW_GD=='NON') NSNOW_LAYER_GD = 0
  !
  IF (CSNOW_GD=='D95' .OR. CSNOW_GD=='EBA') NSNOW_LAYER_GD = 1
  ! not more than 3 layers for snow in garden
  IF (CSNOW_GD=='3-L' .AND. NSNOW_LAYER_GD<=2) NSNOW_LAYER_GD = 12
  !
  IF (NSNOW_LAYER_GD > NSNOW_LAYER_MAX) THEN
    WRITE(ILUOUT,*) '------------------------------------'
    WRITE(ILUOUT,*) 'Please update modd_prep_snow.f90 routine : '
    WRITE(ILUOUT,*) 'The maximum number of snow layers  '
    WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
    WRITE(ILUOUT,*) 'must be decreased to : ', NSNOW_LAYER_MAX
    WRITE(ILUOUT,*) '------------------------------------'
    CALL ABOR1_SFX('READ_PREP_GARDEN_SNOW: NUMBER OF SNOW LAYERS MUST BE INCREASED IN NAMELIST DECLARATION')
  ENDIF
  !
  ALLOCATE(XWSNOW_p(NSNOW_LAYER_GD))
  ALLOCATE(XRSNOW_p(NSNOW_LAYER_GD))
  ALLOCATE(XTSNOW_p(NSNOW_LAYER_GD))
  ALLOCATE(XAGESNOW_p(NSNOW_LAYER_GD))
  ALLOCATE(XLWCSNOW_p(NSNOW_LAYER_GD))
  !
  DO JLAYER=1,NSNOW_LAYER_GD
  
    IF ((XZSNOW_GD(JLAYER)>0) .AND.(XZSNOW_GD(JLAYER)/=XUNDEF )) THEN
      IF ((XWSNOW_GD(JLAYER)>0)  .AND.(XWSNOW_GD(JLAYER)/=XUNDEF )) THEN    
        WRITE(ILUOUT,*) 'XWSNOW and XZSNOW are both defined.'
        WRITE(ILUOUT,*) 'You must define only one of them.'
        WRITE(ILUOUT,*) '    PLEASE CORRECT THAT     '
        CALL ABOR1_SFX('READ_PREP_GARDEN_SNOW: ERROR IN INITIALIZATION OF SNOW DEPTH')
      ELSE
        XWSNOW_p(JLAYER)=XZSNOW_GD(JLAYER)*XRSNOW_GD(JLAYER)
      ENDIF
    ELSE
      XWSNOW_p(JLAYER)=XWSNOW_GD(JLAYER)
    ENDIF
  ENDDO

  XRSNOW_p=XRSNOW_GD(1:NSNOW_LAYER_GD)
  XTSNOW_p=XTSNOW_GD(1:NSNOW_LAYER_GD)
  XAGESNOW_p=XAGESNOW_GD(1:NSNOW_LAYER_GD)
  XLWCSNOW_p=XLWCSNOW_GD(1:NSNOW_LAYER_GD)
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
ENDIF
!
HSNOW = CSNOW_GD
!
KSNOW_LAYER = NSNOW_LAYER_GD
!
IF(ALL(XWSNOW_p(:)==XUNDEF).AND.PRESENT(OUNIF))THEN
    OUNIF=.FALSE.
ELSEIF(PRESENT(OUNIF))THEN
    OUNIF=.TRUE.
ENDIF
!
LFILE=(LEN_TRIM(CFILE_SNOW_GD)>0.AND.LEN_TRIM(CTYPE_SNOW)>0 &
       .AND.LEN_TRIM(CFILEPGD_SNOW_GD)>0.AND.LEN_TRIM(CTYPEPGD_SNOW)>0)
!
IF (PRESENT(OUNIF)) LFILE=(LFILE .AND. .NOT.OUNIF)
!
IF(PRESENT(HFILE))THEN 
  IF(LFILE)THEN
     HFILE = CFILE_SNOW_GD
  ELSE
     HFILE = '                         '
  ENDIF
ENDIF
IF(PRESENT(HFILETYPE))THEN 
  IF(LFILE)THEN
     HFILETYPE = CTYPE_SNOW
  ELSE
     HFILETYPE = '      '
  ENDIF
ENDIF
IF(PRESENT(HFILEPGDTYPE))THEN 
  IF(LFILE)THEN
     HFILEPGDTYPE = CTYPEPGD_SNOW
  ELSE
     HFILEPGDTYPE = '      '
  ENDIF
ENDIF
IF(PRESENT(HFILEPGD))THEN 
  IF(LFILE)THEN
     HFILEPGD = CFILEPGD_SNOW_GD
  ELSE
     HFILEPGD = '                         '
  ENDIF
ENDIF
IF (LFILE.AND.PRESENT(OUNIF)) OUNIF=.FALSE.
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_GARDEN_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_GARDEN_SNOW
