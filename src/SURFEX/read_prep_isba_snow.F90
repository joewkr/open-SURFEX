!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_ISBA_SNOW(HPROGRAM,HSNOW,KSNOW_LAYER,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,OUNIF)
!     #######################################################
!
!!****  *READ_PREP_ISBA_SNOW* - routine to read the configuration for snow
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
!!     B. Decharme  07/2012 Bug init uniform snow
!!      M. Lafaysse 11/2012, snow liquid water content
!!      M. Lafaysse 11/2012, possibility to prescribe snow depth instead of snow water equivalent
!!      M Lafaysse 04/2014 : LSNOW_PREP_PERM
!      B. Decharme  07/2013 ES snow grid layer can be > to 3 (default 12)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_PREP_ISBA_SNOW
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
USE MODD_PREP_ISBA, ONLY : CFILE_SNOW, CTYPE_SNOW, CFILEPGD_SNOW, &
                           CTYPEPGD_SNOW, LSNOW_IDEAL, &
                           XWSNOW_p=>XWSNOW, XTSNOW_p=>XTSNOW,  &
                           XLWCSNOW_p=>XLWCSNOW, &
                           XRSNOW_p=>XRSNOW, XASNOW,            &
                           XSG1SNOW_p=>XSG1SNOW, XSG2SNOW_p=>XSG2SNOW, &
                           XHISTSNOW_p=>XHISTSNOW, XAGESNOW_p=>XAGESNOW
                           
!
USE MODD_PREP_SNOW, ONLY : LSNOW_FRAC_TOT, NSNOW_LAYER_MAX , LSNOW_PREP_PERM
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
REAL, DIMENSION(NSNOW_LAYER_MAX) :: XWSNOW, XZSNOW, XRSNOW, XTSNOW, XLWCSNOW, &
                                    XSG1SNOW, XSG2SNOW, XHISTSNOW, XAGESNOW
INTEGER           :: JLAYER
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
                            LSNOW_IDEAL, LSNOW_FRAC_TOT,LSNOW_PREP_PERM, &
                            XWSNOW, XZSNOW, XTSNOW, XLWCSNOW, XRSNOW, XASNOW,  &
                            XSG1SNOW, XSG2SNOW, XHISTSNOW, XAGESNOW,     &
                            LSWEMAX,XSWEMAX
!-------------------------------------------------------------------------------
!* default
!  -------
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_ISBA_SNOW',0,ZHOOK_HANDLE)
IF (LNAM_READ) THEN
  !
  CSNOW = 'D95'
  NSNOW_LAYER = 1
  !
  CFILE_SNOW = '                         '
  CTYPE_SNOW = '      '
  CFILEPGD_SNOW = '                         '
  CTYPEPGD_SNOW = '      '  
  !
  LSNOW_IDEAL = .FALSE.
  LSNOW_FRAC_TOT = .FALSE.
  LSNOW_PREP_PERM = .TRUE.
  !
  XWSNOW(:) = XUNDEF
  XZSNOW(:) = XUNDEF
  XRSNOW(:) = XUNDEF  
  XTSNOW(:) = XTT  
  XLWCSNOW(:) = 0.
  XASNOW = XANSMIN
  XSG1SNOW(:) = XUNDEF
  XSG2SNOW(:) = XUNDEF
  XHISTSNOW(:) = XUNDEF
  XAGESNOW(:) = XUNDEF  
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
  CALL POSNAM(ILUNAM,'NAM_PREP_ISBA_SNOW',GFOUND,ILUOUT)
  IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PREP_ISBA_SNOW)
  !
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW',CSNOW,'D95','3-L','EBA','CRO','NON')
  !
  IF (CSNOW=='NON') NSNOW_LAYER = 0
  !
  IF (CSNOW=='D95' .OR. CSNOW=='EBA') NSNOW_LAYER = 1
  !
  IF ((CSNOW=='3-L' .OR. CSNOW=='CRO') .AND. NSNOW_LAYER<=2) NSNOW_LAYER = 12
  !
  IF (NSNOW_LAYER > NSNOW_LAYER_MAX) THEN
    WRITE(ILUOUT,*) '------------------------------------'
    WRITE(ILUOUT,*) 'Please update modd_prep_snow.f90 routine : '
    WRITE(ILUOUT,*) 'The maximum number of snow layers  '
    WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
    WRITE(ILUOUT,*) 'must be decreased to : ', NSNOW_LAYER_MAX
    WRITE(ILUOUT,*) '------------------------------------'
    CALL ABOR1_SFX('READ_PREP_ISBA_SNOW: NUMBER OF SNOW LAYERS MUST BE INCREASED IN NAMELIST DECLARATION')
  ENDIF
  !
  ! Convert prescribed snow depth and snow density in snow water equivalent
  DO JLAYER=1,NSNOW_LAYER
    IF (XZSNOW(JLAYER)/=XUNDEF) THEN
      IF (XWSNOW(JLAYER)/=XUNDEF) THEN
        WRITE(ILUOUT,*) '----------------------------'
        WRITE(ILUOUT,*) 'layer ',JLAYER,':'
        WRITE(ILUOUT,*) 'XWSNOW and XZSNOW are both defined.'
        WRITE(ILUOUT,*) 'You must define only one of them.'
        WRITE(ILUOUT,*) '    PLEASE CORRECT THAT     '
        WRITE(ILUOUT,*) '----------------------------'
        CALL ABOR1_SFX('READ_PREP_ISBA_SNOW: ERROR IN INITIALISATION OF SNOW PARAMETERS')
      ELSEIF (XRSNOW(JLAYER)==XUNDEF) THEN
        WRITE(ILUOUT,*) '----------------------------'
        WRITE(ILUOUT,*) 'layer ',JLAYER,':'
        WRITE(ILUOUT,*) 'XZSNOW is defined           '
        WRITE(ILUOUT,*) 'but XRSNOW is not.          '
        WRITE(ILUOUT,*) '    PLEASE CORRECT THAT     '
        WRITE(ILUOUT,*) '----------------------------'
        CALL ABOR1_SFX('READ_PREP_ISBA_SNOW: ERROR IN INITIALISATION OF SNOW PARAMETERS')
      ELSE
        XWSNOW(JLAYER)=XZSNOW(JLAYER)*XRSNOW(JLAYER)
      END IF
    ENDIF
  END DO

  IF(NSNOW_LAYER>=3)THEN
    IF(XWSNOW(1)/=XUNDEF.AND.ANY(XWSNOW(2:NSNOW_LAYER)==XUNDEF))THEN
      WHERE(XWSNOW(2:NSNOW_LAYER)==XUNDEF)XWSNOW(2:NSNOW_LAYER)=0.0
    ENDIF
    IF(XRSNOW(1)/=XUNDEF.AND.ANY(XRSNOW(2:NSNOW_LAYER)==XUNDEF))THEN
      WHERE(XRSNOW(2:NSNOW_LAYER)==XUNDEF)XRSNOW(2:NSNOW_LAYER)=XRSNOW(1)
    ENDIF    
  ENDIF
  !
  ALLOCATE(XWSNOW_p(NSNOW_LAYER))
  ALLOCATE(XRSNOW_p(NSNOW_LAYER))
  ALLOCATE(XTSNOW_p(NSNOW_LAYER))
  ALLOCATE(XLWCSNOW_p(NSNOW_LAYER))
  ALLOCATE(XAGESNOW_p(NSNOW_LAYER))
  !
  XWSNOW_p  =XWSNOW(1:NSNOW_LAYER)
  XRSNOW_p  =XRSNOW(1:NSNOW_LAYER)
  XTSNOW_p  =XTSNOW(1:NSNOW_LAYER)
  XAGESNOW_p=XAGESNOW(1:NSNOW_LAYER)
  XLWCSNOW_p=XLWCSNOW(1:NSNOW_LAYER)
  !

  !Coherence test between XTSNOW and XLWCSNOW
  DO JLAYER=1,NSNOW_LAYER
    IF  ((XLWCSNOW_p(JLAYER)>0.).AND.(XTSNOW_p(JLAYER)<XTT)) THEN
        WRITE(ILUOUT,*) '----------------------------'
        WRITE(ILUOUT,*) 'layer ',JLAYER,':'
        WRITE(ILUOUT,*) 'Incoherence between         '
        WRITE(ILUOUT,*) 'snow liquid water content   '
        WRITE(ILUOUT,*) 'and snow temperature.       '
        WRITE(ILUOUT,*) '    PLEASE CORRECT THAT     '
        WRITE(ILUOUT,*) '----------------------------'
        CALL ABOR1_SFX('READ_PREP_ISBA_SNOW: ERROR IN INITIALISATION OF SNOW PARAMETERS')
    END IF
  END DO

  IF (CSNOW=='CRO') THEN
    !
    ALLOCATE(XSG1SNOW_p (NSNOW_LAYER))
    ALLOCATE(XSG2SNOW_p (NSNOW_LAYER))
    ALLOCATE(XHISTSNOW_p(NSNOW_LAYER))
    !
    XSG1SNOW_p =XSG1SNOW (1:NSNOW_LAYER)
    XSG2SNOW_p =XSG2SNOW (1:NSNOW_LAYER)
    XHISTSNOW_p=XHISTSNOW(1:NSNOW_LAYER)
    !
    DO JLAYER=1,NSNOW_LAYER
      IF ((XSG1SNOW_p (JLAYER)==XUNDEF .OR. XSG2SNOW_p(JLAYER)==XUNDEF .OR. &
           XHISTSNOW_p(JLAYER)==XUNDEF .OR. XAGESNOW_p(JLAYER)==XUNDEF) &
           .AND. XWSNOW_p(JLAYER).NE.0. .AND. XWSNOW_p(JLAYER)/=XUNDEF ) THEN
        WRITE(ILUOUT,*) '----------------------------'
        WRITE(ILUOUT,*) 'WSNOW/=0 AND ONE OF SG1SNOW,'
        WRITE(ILUOUT,*) 'SG2SNOW, HISTSNOW OR AGESNOW'
        WRITE(ILUOUT,*) '         ==XUNDEF           '
        WRITE(ILUOUT,*) '    PLEASE CORRECT THAT     '
        WRITE(ILUOUT,*) '----------------------------'
        CALL ABOR1_SFX('READ_PREP_ISBA_SNOW: ERROR IN INITIALISATION OF SNOW PARAMETERS')
      ENDIF
    ENDDO
    !
  ELSE
    !
    ALLOCATE(XSG1SNOW_p (0))
    ALLOCATE(XSG2SNOW_p (0))
    ALLOCATE(XHISTSNOW_p(0))
    !
  ENDIF
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
ENDIF
!
HSNOW = CSNOW
!
KSNOW_LAYER = NSNOW_LAYER
!
IF(ALL(XWSNOW_p(:)==XUNDEF).AND.PRESENT(OUNIF))THEN
    OUNIF=.FALSE.
ELSEIF(PRESENT(OUNIF))THEN
    OUNIF=.TRUE.
ENDIF
!
LFILE=(LEN_TRIM(CFILE_SNOW)>0.AND.LEN_TRIM(CTYPE_SNOW)>0 &
        .AND.LEN_TRIM(CFILEPGD_SNOW)>0.AND.LEN_TRIM(CTYPEPGD_SNOW)>0)
!
IF(PRESENT(HFILE))THEN 
  IF(LFILE)THEN
     HFILE = CFILE_SNOW
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
     HFILEPGD = CFILEPGD_SNOW
  ELSE
     HFILEPGD = '                         '
  ENDIF
ENDIF
IF (LFILE.AND.PRESENT(OUNIF)) OUNIF=.FALSE.
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_ISBA_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_ISBA_SNOW
