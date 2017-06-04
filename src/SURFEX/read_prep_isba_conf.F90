!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_ISBA_CONF(HPROGRAM,HVAR,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                                     HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KLUOUT,OUNIF)
!     #######################################################
!
!!****  *READ_PREP_ISBA_CONF* - routine to read the configuration for ISBA
!!                              fields preparation
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
!!      P. Samuelsson  02/2012  MEB
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_READ_PREP_SURF_ATM_CONF
!
USE MODN_PREP_ISBA
USE MODD_PREP_ISBA,  ONLY : CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,        &
                            CFILE_HUG, CTYPE_HUG,                              &
                            CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP,    &
                            XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                   &
                            XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,                &
                            CFILE_TG, CTYPE_TG,                                &
                            CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,       &
                            XTG_SURF, XTG_ROOT, XTG_DEEP,                      &  
                            XWSNOW, XTSNOW, XRSNOW, XASNOW
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM    ! program calling ISBA
 CHARACTER(LEN=7),  INTENT(IN)  :: HVAR        ! variable treated
 CHARACTER(LEN=28), INTENT(OUT) :: HFILE       ! file name
 CHARACTER(LEN=6),  INTENT(OUT) :: HFILETYPE   ! file type
 CHARACTER(LEN=28), INTENT(OUT) :: HFILEPGD    ! file name
 CHARACTER(LEN=6),  INTENT(OUT) :: HFILEPGDTYPE! file type
 CHARACTER(LEN=28), INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=28), INTENT(IN)  :: HPGDFILE    ! atmospheric file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HPGDFILETYPE! atmospheric file type
INTEGER,           INTENT(IN)  :: KLUOUT      ! logical unit of output listing
LOGICAL,           INTENT(OUT) :: OUNIF       ! flag for prescribed uniform field

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_ISBA_CONF',0,ZHOOK_HANDLE)
HFILE    = '                         '
HFILETYPE    = '      '
!
HFILEPGD = '                         '
HFILEPGDTYPE = '      '
!
OUNIF    = .FALSE.
!
!-------------------------------------------------------------------------------
!
!* choice of input file
!  --------------------
!
SELECT CASE (HVAR)
  CASE ('WG     ','WGI    ')
    IF (LEN_TRIM(CFILE_HUG)>0 .AND. LEN_TRIM(CTYPE_HUG)>0 ) THEN
      HFILE     = CFILE_HUG
      HFILETYPE = CTYPE_HUG
    END IF
  CASE ('TG     ','TV     ','TC     ')
    IF (LEN_TRIM(CFILE_TG)>0 .AND. LEN_TRIM(CTYPE_TG)>0 ) THEN
      HFILE     = CFILE_TG
      HFILETYPE = CTYPE_TG
    END IF
END SELECT
!
IF (LEN_TRIM(HFILE)==0 .AND. LEN_TRIM(CFILE_ISBA)>0 .AND. LEN_TRIM(CTYPE)>0) THEN
  HFILE     = CFILE_ISBA
  HFILETYPE = CTYPE
END IF
!
IF (LEN_TRIM(HFILEPGD)==0 .AND. LEN_TRIM(CFILEPGD_ISBA)>0 .AND. LEN_TRIM(CTYPEPGD)>0) THEN
  HFILEPGD     = CFILEPGD_ISBA
  HFILEPGDTYPE = CTYPEPGD
END IF
!
!! If no file name in the scheme namelist,
!! try to find a name in NAM_SURF_ATM
!
IF (LEN_TRIM(HFILE)==0) THEN
!
 CALL READ_PREP_SURF_ATM_CONF(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                             HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KLUOUT)
!
END IF
!
!! If no file name in the scheme namelist,
!! nor in NAM_SURF_ATM, look if ascii input files are present
!
SELECT CASE (HVAR)
  CASE ('WG     ','WGI    ')
    IF ( LEN_TRIM(CTYPE_HUG )>0       .AND. &
           LEN_TRIM(CFILE_HUG_SURF)>0   .AND. &
           LEN_TRIM(CFILE_HUG_ROOT)>0   .AND. &
           LEN_TRIM(CFILE_HUG_DEEP)>0         ) THEN  
       HFILETYPE = CTYPE_HUG 
    END IF
    IF (HVAR=='WGI    ' .AND. HFILETYPE=='ASCLLV') THEN
       OUNIF = .TRUE.
       IF (XHUGI_SURF==XUNDEF) XHUGI_SURF = 0.
       IF (XHUGI_ROOT==XUNDEF) XHUGI_ROOT = 0.
       IF (XHUGI_DEEP==XUNDEF) XHUGI_DEEP = 0.
       IF (LHOOK) CALL DR_HOOK('READ_PREP_ISBA_CONF',1,ZHOOK_HANDLE)
       RETURN
    ENDIF
  CASE ('TG     ','TV     ','TC     ')
    IF ( LEN_TRIM(CTYPE_TG )>0       .AND. &
           LEN_TRIM(CFILE_TG_SURF)>0   .AND. &
           LEN_TRIM(CFILE_TG_ROOT)>0   .AND. &
           LEN_TRIM(CFILE_TG_DEEP)>0         ) THEN  
       HFILETYPE = CTYPE_TG 
    END IF
END SELECT
!
!-------------------------------------------------------------------------------
!
!* Is an uniform field prescribed?
!  ------------------------------
!
SELECT CASE (HVAR)
  CASE ('WG     ')
    OUNIF = (XHUG_SURF/=XUNDEF) .OR. (XHUG_ROOT/=XUNDEF) .OR. (XHUG_DEEP/=XUNDEF)
    IF (OUNIF .AND. (XHUG_SURF==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUG_SURF, XHUG_ROOT OR XHUG_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XHUG_SURF MUST BE SET')
    END IF
    IF (OUNIF .AND. (XHUG_ROOT==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUG_SURF, XHUG_ROOT OR XHUG_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XHUG_ROOT MUST BE SET')
    END IF
    IF (OUNIF .AND. (XHUG_DEEP==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUG_SURF, XHUG_ROOT OR XHUG_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XHUG_DEEP MUST BE SET')
    END IF
    !
      CASE ('WGI    ')
    OUNIF = (XHUGI_SURF/=XUNDEF) .OR. (XHUGI_ROOT/=XUNDEF) .OR. (XHUGI_DEEP/=XUNDEF)
    IF (OUNIF .AND. (XHUGI_SURF==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUGI_SURF, XHUGI_ROOT OR XHUGI_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XHUGI_SURF MUST BE SET')
    END IF
    IF (OUNIF .AND. (XHUGI_ROOT==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUGI_SURF, XHUGI_ROOT OR XHUGI_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XHUGI_ROOT MUST BE SET')
    END IF
    IF (OUNIF .AND. (XHUGI_DEEP==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUGI_SURF, XHUGI_ROOT OR XHUGI_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XHUGI_DEEP MUST BE SET')
    END IF
   !
  CASE ('TG     ','TV     ','TC     ')
    OUNIF = (XTG_SURF/=XUNDEF)  .OR. (XTG_ROOT/=XUNDEF)  .OR. (XTG_DEEP/=XUNDEF)
    IF (OUNIF .AND. (XTG_SURF==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XTG_SURF, XTG_ROOT OR XTG_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XTG_SURF MUST BE SET')
    END IF
    IF (OUNIF .AND. (XTG_ROOT==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XTG_SURF, XTG_ROOT OR XTG_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XTG_ROOT MUST BE SET')
    END IF
    IF (OUNIF .AND. (XTG_DEEP==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XTG_SURF, XTG_ROOT OR XTG_DEEP IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_ISBA_CONF: XTG_DEEP MUST BE SET')
    END IF
    !
END SELECT
!
!-------------------------------------------------------------------------------
!
!* no file given ? nor specific value in namelist? One takes the default value.
!
IF (HFILETYPE=='      ' .AND. .NOT. OUNIF) THEN
  IF (HVAR(1:2)/='ZS') WRITE(KLUOUT,*) 'NO FILE FOR FIELD ',HVAR, &
                                        ': UNIFORM DEFAULT FIELD IS PRESCRIBED'
  IF (HVAR(1:3)=='WGI') THEN
    XHUGI_SURF = 0.
    XHUGI_ROOT = 0.
    XHUGI_DEEP = 0.
  ENDIF                                     
  OUNIF = .TRUE.
END IF
IF (LHOOK) CALL DR_HOOK('READ_PREP_ISBA_CONF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_ISBA_CONF
