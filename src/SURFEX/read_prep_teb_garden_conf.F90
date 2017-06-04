!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_TEB_GARDEN_CONF(HPROGRAM,HVAR,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                                           HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KLUOUT,OUNIF)
!     #######################################################
!
!!****  *READ_PREP_TEB_GARDEN_CONF* - routine to read the configuration for ISBA
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_READ_PREP_SURF_ATM_CONF
!
USE MODN_PREP_TEB_GARDEN
USE MODD_PREP_TEB_GARDEN, ONLY : CFILE_GD, CTYPE, CFILEPGD_GD, CTYPEPGD,      &
                                 CFILE_HUG_GD, CTYPE_HUG,                            &
                                 CFILE_HUG_SURF_GD, CFILE_HUG_ROOT_GD, CFILE_HUG_DEEP_GD,  &
                                 XHUG_SURF_GD, XHUG_ROOT_GD, XHUG_DEEP_GD,                 &
                                 XHUGI_SURF_GD, XHUGI_ROOT_GD, XHUGI_DEEP_GD,              &
                                 CFILE_TG_GD, CTYPE_TG,                              &
                                 CFILE_TG_SURF_GD, CFILE_TG_ROOT_GD, CFILE_TG_DEEP_GD,     &
                                 XTG_SURF_GD, XTG_ROOT_GD, XTG_DEEP_GD
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM    ! program calling
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
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    ! at the open of the file in LFI  routines 
INTEGER           :: ILUNAM         ! Logical unit of namelist file
!
 CHARACTER(LEN=28) :: YNAMELIST      ! namelist file
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_GARDEN_CONF',0,ZHOOK_HANDLE)
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
    IF (LEN_TRIM(CFILE_HUG_GD)>0 .AND. LEN_TRIM(CTYPE_HUG)>0 ) THEN
      HFILE     = CFILE_HUG_GD
      HFILETYPE = CTYPE_HUG
    END IF
  CASE ('TG     ')
    IF (LEN_TRIM(CFILE_TG_GD)>0 .AND. LEN_TRIM(CTYPE_TG)>0 ) THEN
      HFILE     = CFILE_TG_GD
      HFILETYPE = CTYPE_TG
    END IF
END SELECT
!
IF (LEN_TRIM(HFILE)==0 .AND. LEN_TRIM(CFILE_GD)>0 .AND. LEN_TRIM(CTYPE)>0) THEN
  HFILE     = CFILE_GD
  HFILETYPE = CTYPE
END IF
!
IF (LEN_TRIM(HFILEPGD)==0 .AND. LEN_TRIM(CFILEPGD_GD)>0 .AND. LEN_TRIM(CTYPEPGD)>0) THEN
  HFILEPGD     = CFILEPGD_GD
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
           LEN_TRIM(CFILE_HUG_SURF_GD)>0   .AND. &
           LEN_TRIM(CFILE_HUG_ROOT_GD)>0   .AND. &
           LEN_TRIM(CFILE_HUG_DEEP_GD)>0         ) THEN  
       HFILETYPE = CTYPE_HUG 
    END IF
    IF (HVAR=='WGI    ' .AND. HFILETYPE=='ASCLLV') THEN
       OUNIF = .TRUE.
       IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_GARDEN_CONF',1,ZHOOK_HANDLE)
       RETURN
    ENDIF
  CASE ('TG     ')
    IF ( LEN_TRIM(CTYPE_TG )>0       .AND. &
           LEN_TRIM(CFILE_TG_SURF_GD)>0   .AND. &
           LEN_TRIM(CFILE_TG_ROOT_GD)>0   .AND. &
           LEN_TRIM(CFILE_TG_DEEP_GD)>0         ) THEN  
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
    OUNIF = (XHUG_SURF_GD/=XUNDEF) .OR. (XHUG_ROOT_GD/=XUNDEF) .OR. (XHUG_DEEP_GD/=XUNDEF)
    IF (OUNIF .AND. (XHUG_SURF_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUG_SURF_GD, XHUG_ROOT_GD OR XHUG_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XHUG_SURF_GD MUST BE SET')
    END IF
    IF (OUNIF .AND. (XHUG_ROOT_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUG_SURF_GD, XHUG_ROOT_GD OR XHUG_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XHUG_ROOT_GD MUST BE SET')
    END IF
    IF (OUNIF .AND. (XHUG_DEEP_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUG_SURF_GD, XHUG_ROOT_GD OR XHUG_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XHUG_DEEP_GD MUST BE SET')
    END IF
     !
      CASE ('WGI    ')
    OUNIF = (XHUGI_SURF_GD/=XUNDEF) .OR. (XHUGI_ROOT_GD/=XUNDEF) .OR. (XHUGI_DEEP_GD/=XUNDEF)
    IF (OUNIF .AND. (XHUGI_SURF_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUGI_SURF_GD, XHUGI_ROOT_GD OR XHUGI_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XHUGI_SURF_GD MUST BE SET')
    END IF
    IF (OUNIF .AND. (XHUGI_ROOT_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUGI_SURF_GD, XHUGI_ROOT_GD OR XHUGI_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XHUGI_ROOT_GD MUST BE SET')
    END IF
    IF (OUNIF .AND. (XHUGI_DEEP_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XHUGI_SURF_GD, XHUGI_ROOT_GD OR XHUGI_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XHUGI_DEEP_GD MUST BE SET')
    END IF
    !
  CASE ('TG     ')
    OUNIF = (XTG_SURF_GD/=XUNDEF)  .OR. (XTG_ROOT_GD/=XUNDEF)  .OR. (XTG_DEEP_GD/=XUNDEF)
    IF (OUNIF .AND. (XTG_SURF_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XTG_SURF_GD, XTG_ROOT_GD OR XTG_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XTG_SURF_GD MUST BE SET')
    END IF
    IF (OUNIF .AND. (XTG_ROOT_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XTG_SURF_GD, XTG_ROOT_GD OR XTG_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XTG_ROOT_GD MUST BE SET')
    END IF
    IF (OUNIF .AND. (XTG_DEEP_GD==XUNDEF)) THEN
       WRITE(KLUOUT,*)'ONE OF XTG_SURF_GD, XTG_ROOT_GD OR XTG_DEEP_GD IS GIVEN'
       CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: XTG_DEEP_GD MUST BE SET')
    END IF
    !

END SELECT
!
!-------------------------------------------------------------------------------
!
!* If no file and no uniform field is prescribed: default values used
!  ---------------------------------------------
!
IF (LEN_TRIM(HFILETYPE)==0 .AND. .NOT. OUNIF) THEN
  IF (HVAR(1:2)/='TG' .AND. HVAR(1:2)/='WG' .OR. HVAR(1:3)=='WGI') THEN
    IF (HVAR(1:2)/='ZS') WRITE(KLUOUT,*) 'NO FILE FOR FIELD ',HVAR, &
                                        ': UNIFORM DEFAULT FIELD IS PRESCRIBED'
    IF (HVAR(1:3)=='WGI') THEN
      XHUGI_SURF_GD = 0.
      XHUGI_ROOT_GD = 0.
      XHUGI_DEEP_GD = 0.
    ENDIF                                         
    OUNIF = .TRUE.
    IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_GARDEN_CONF',1,ZHOOK_HANDLE)
    RETURN
  ELSE
    WRITE(KLUOUT,*) 'AN INPUT FILE OR A UNIFORM VALUE IS REQUIRED FOR FIELD: ',HVAR
    WRITE(KLUOUT,*) 'Please complete NAM_PREP_TEB_GARDEN'
    CALL ABOR1_SFX('READ_PREP_TEB_GARDEN_CONF: AN INPUT FILE OR A UNIFORM VALUE IS REQUIRED FOR '//HVAR)
  END IF
END IF
IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_GARDEN_CONF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_TEB_GARDEN_CONF
