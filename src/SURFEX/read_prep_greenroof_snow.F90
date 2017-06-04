!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_GREENROOF_SNOW(HPROGRAM,HSNOW,KSNOW_LAYER,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,OUNIF)
!     #######################################################
!
!!****  *READ_PREP_GREENROOF_SNOW* - routine to read the configuration for snow
!!                                   in ISBA fields preparation
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
!!    Based on read_prep_garden_snow
!!
!!    AUTHOR
!!    ------
!!      C. de Munck   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2011 
!!     M. Lafaysse  08/2013 init XZSNOW or XLWCSNOW
!      B. Decharme  07/2013 ES snow grid layer can be > to 3 (default 12)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_PREP_GREENROOF_SNOW
USE MODD_READ_NAMELIST,        ONLY : LNAM_READ
!
USE MODD_SURF_PAR,             ONLY : XUNDEF
USE MODD_SNOW_PAR,             ONLY : XANSMIN, XRHOSMAX
USE MODD_CSTS,                 ONLY : XTT
!
USE MODE_POS_SURF
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_ABOR1_SFX
!
USE MODD_PREP_TEB_GREENROOF,   ONLY : CFILE_SNOW_GR, CTYPE_SNOW, CFILEPGD_SNOW_GR, &
                                      CTYPEPGD_SNOW, LSNOW_IDEAL_GR, &
                                      XWSNOW_p=>XWSNOW_GR, XTSNOW_p=>XTSNOW_GR, XLWCSNOW_p=>XLWCSNOW_GR, &
                                      XRSNOW_p=>XRSNOW_GR, XAGESNOW_p=>XAGESNOW_GR, XASNOW_GR
!
USE MODD_PREP_SNOW,            ONLY : NSNOW_LAYER_MAX, LSNOW_PREP_PERM
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
 CHARACTER(LEN=28), OPTIONAL, INTENT(OUT) :: HFILEPGD       ! file name
 CHARACTER(LEN=6),  OPTIONAL, INTENT(OUT) :: HFILEPGDTYPE    ! file type  
 LOGICAL,           OPTIONAL, INTENT(OUT) :: OUNIF  ! uniform snow
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
REAL, DIMENSION(NSNOW_LAYER_MAX) :: XWSNOW_GR, XZSNOW_GR, XRSNOW_GR, XTSNOW_GR, XLWCSNOW_GR, &
                                    XSG1SNOW_GR, XSG2SNOW_GR, XHISTSNOW_GR, XAGESNOW_GR
!
LOGICAL           :: LFILE
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output file logical unit
INTEGER           :: ILUNAM         ! namelist file logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
NAMELIST/NAM_PREP_ISBA_SNOW/CSNOW, NSNOW_LAYER, CFILE_SNOW, CTYPE_SNOW,  &
                            CFILEPGD_SNOW, CTYPEPGD_SNOW,                & 
                            LSNOW_IDEAL, LSNOW_FRAC_TOT, LSNOW_PREP_PERM,&
                            XWSNOW, XZSNOW, XTSNOW, XLWCSNOW, XRSNOW, XASNOW,              &
                            XSG1SNOW, XSG2SNOW, XHISTSNOW, XAGESNOW,     &
                            LSWEMAX, XSWEMAX

NAMELIST/NAM_PREP_GREENROOF_SNOW/CSNOW_GR, NSNOW_LAYER_GR, CFILE_SNOW_GR, CTYPE_SNOW, &
                            CFILEPGD_SNOW_GR, CTYPEPGD_SNOW,                & 
                            LSNOW_IDEAL_GR, XWSNOW_GR, XZSNOW_GR, XTSNOW_GR, &
                            XLWCSNOW_GR, XRSNOW_GR, XASNOW_GR
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_PREP_GREENROOF_SNOW',0,ZHOOK_HANDLE)
!
!* default for greenroofs
!  ----------------------
CSNOW_GR       = '3-L'
NSNOW_LAYER_GR = 3
!
IF (LNAM_READ) THEN
  !
  CSNOW_GR          = '3-L'
  NSNOW_LAYER_GR    = 3
  !
  CFILE_SNOW_GR     = '                         '
  CTYPE_SNOW     = '      '  
  CFILEPGD_SNOW_GR    = '                         '
  CTYPEPGD_SNOW    = '      '      
  !
  LSNOW_IDEAL_GR    = .FALSE.
  LSNOW_PREP_PERM = .TRUE.
  !
  XWSNOW_GR(:)      = 0.
  XZSNOW_GR(:) = XUNDEF  
  XRSNOW_GR(:)      = XRHOSMAX
  XTSNOW_GR(:)      = XTT
  XLWCSNOW_GR(:) = 0.
  XASNOW_GR         = XANSMIN  
  XSG1SNOW_GR(:)    = XUNDEF
  XSG2SNOW_GR(:)    = XUNDEF
  XHISTSNOW_GR(:)   = XUNDEF
  XAGESNOW_GR(:)    = XUNDEF  
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
    CSNOW = '3-L'
    NSNOW_LAYER = 3
    CFILE_SNOW = '                         '
    LSNOW_IDEAL = .FALSE.
    LSNOW_FRAC_TOT = .FALSE.
    XWSNOW(:) = XUNDEF
    XZSNOW(:) = XUNDEF
    XRSNOW(:) = XRHOSMAX
    XTSNOW(:) = XTT
    XLWCSNOW(:) = 0.
    XASNOW = XANSMIN  
    XSG1SNOW(:) = XUNDEF
    XSG2SNOW(:) = XUNDEF
    XHISTSNOW(:) = XUNDEF
    XAGESNOW(:) = XUNDEF    
    !
    READ(UNIT=ILUNAM,NML=NAM_PREP_ISBA_SNOW)
    CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW',CSNOW,'D95','3-L','EBA','NON','CRO')
    !
    CSNOW_GR = CSNOW
    NSNOW_LAYER_GR = NSNOW_LAYER
    CFILE_SNOW_GR = CFILE_SNOW
    LSNOW_IDEAL_GR = LSNOW_IDEAL
    XWSNOW_GR(:) = XWSNOW(:)
    XZSNOW_GR(:) = XZSNOW(:)
    XRSNOW_GR(:) = XRSNOW(:)
    XTSNOW_GR(:) = XTSNOW(:)
    XLWCSNOW_GR(:) = XLWCSNOW(:)
    XASNOW_GR = XASNOW
    XSG1SNOW_GR(:) = XSG1SNOW(:)
    XSG2SNOW_GR(:) = XSG2SNOW(:)
    XHISTSNOW_GR(:) = XHISTSNOW(:)
    XAGESNOW_GR(:) = XAGESNOW(:)
    !
  ENDIF
  !
  !* It is erased by GREENROOF namelist if specified
  CALL POSNAM(ILUNAM,'NAM_PREP_GREENROOF_SNOW',GFOUND,ILUOUT)
  IF (GFOUND) THEN
    READ(UNIT=ILUNAM,NML=NAM_PREP_GREENROOF_SNOW)
    !crocus can't be used in garden if not used in isba scheme
    CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW',CSNOW_GR,'D95','3-L','EBA','NON')
  ENDIF
  !
  IF ( CSNOW_GR=='NON')                                         NSNOW_LAYER_GR = 0
  IF ( CSNOW_GR=='D95' .OR. CSNOW_GR=='EBA')                       NSNOW_LAYER_GR = 1
  IF (CSNOW_GR=='3-L' .AND. NSNOW_LAYER_GR<=2) NSNOW_LAYER_GR = 12
  !  
  IF (NSNOW_LAYER_GR > NSNOW_LAYER_MAX) THEN
    WRITE(ILUOUT,*) '------------------------------------'
    WRITE(ILUOUT,*) 'Please update modd_prep_snow.f90 routine : '
    WRITE(ILUOUT,*) 'The maximum number of snow layers  '
    WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
    WRITE(ILUOUT,*) 'must be decreased to : ', NSNOW_LAYER_MAX
    WRITE(ILUOUT,*) '------------------------------------'
    CALL ABOR1_SFX('READ_PREP_GREENROOF_SNOW: NUMBER OF SNOW LAYERS MUST BE INCREASED IN NAMELIST DECLARATION')
  ENDIF
  !
  ALLOCATE(XWSNOW_p(NSNOW_LAYER_GR))
  ALLOCATE(XRSNOW_p(NSNOW_LAYER_GR))
  ALLOCATE(XTSNOW_p(NSNOW_LAYER_GR))
  ALLOCATE(XAGESNOW_p(NSNOW_LAYER_GR))
  ALLOCATE(XLWCSNOW_p(NSNOW_LAYER_GR))
  !
  DO JLAYER=1,NSNOW_LAYER_GR
    IF ((XZSNOW_GR(JLAYER)>0) .AND.(XZSNOW_GR(JLAYER)/=XUNDEF )) THEN
      IF ((XWSNOW_GR(JLAYER)>0)  .AND.(XWSNOW_GR(JLAYER)/=XUNDEF )) THEN
        WRITE(ILUOUT,*) 'XWSNOW and XZSNOW are both defined.'
        WRITE(ILUOUT,*) 'You must define only one of them.'
        WRITE(ILUOUT,*) '    PLEASE CORRECT THAT     '
        CALL ABOR1_SFX('READ_PREP_GREENROOF_SNOW: ERROR IN INITIALIZATION OF SNOW DEPTH')
      ELSE
        XWSNOW_p(JLAYER)=XZSNOW_GR(JLAYER)*XRSNOW_GR(JLAYER)
      ENDIF
    ELSE
      XWSNOW_p(JLAYER)=XWSNOW_GR(JLAYER)
    ENDIF
  ENDDO

  XRSNOW_p=XRSNOW_GR(1:NSNOW_LAYER_GR)
  XTSNOW_p=XTSNOW_GR(1:NSNOW_LAYER_GR)
  XAGESNOW_p=XAGESNOW_GR(1:NSNOW_LAYER_GR)
  XLWCSNOW_p=XLWCSNOW_GR(1:NSNOW_LAYER_GR)
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
ENDIF
!
HSNOW       = CSNOW_GR
KSNOW_LAYER = NSNOW_LAYER_GR
!
IF(ALL(XWSNOW_p(:)==XUNDEF).AND.PRESENT(OUNIF))THEN
    OUNIF=.FALSE.
ELSEIF(PRESENT(OUNIF))THEN
    OUNIF=.TRUE.
ENDIF
!
LFILE=(LEN_TRIM(CFILE_SNOW_GR)>0.AND.LEN_TRIM(CTYPE_SNOW)>0 &
        .AND.LEN_TRIM(CFILEPGD_SNOW_GR)>0.AND.LEN_TRIM(CTYPEPGD_SNOW)>0)
!
IF (PRESENT(OUNIF)) LFILE=(LFILE .AND. .NOT.OUNIF)
!
IF(PRESENT(HFILE))THEN 
  IF(LFILE)THEN
     HFILE = CFILE_SNOW_GR
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
     HFILEPGD = CFILEPGD_SNOW_GR
  ELSE
     HFILEPGD = '                         '
  ENDIF
ENDIF
IF (LFILE.AND.PRESENT(OUNIF)) OUNIF=.FALSE.
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_GREENROOF_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_GREENROOF_SNOW
