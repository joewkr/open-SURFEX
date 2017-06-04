!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_FLAKE_CONF(HPROGRAM,HVAR,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                                      HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KLUOUT,OUNIF)
!     #######################################################
!
!!****  *READ_PREP_FLAKE_CONF* - routine to read the configuration for
!!                                 FLAKE fields preparation
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
!!      P. Le Moigne 10/2005, Phasage Arome
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_READ_PREP_SURF_ATM_CONF
!
USE MODN_PREP_FLAKE
USE MODD_PREP_FLAKE, ONLY : CFILE_FLAKE, CFILEPGD_FLAKE, CTYPE, CTYPEPGD, &
                              XTS_UNIF,         &
                              XUNIF_T_SNOW,     &
                              XUNIF_T_ICE,      &
                              XUNIF_T_MNW,      &
                              XUNIF_T_WML,      &
                              XUNIF_T_BOT,      &
                              XUNIF_T_B1,       &
                              XUNIF_CT,         &
                              XUNIF_H_SNOW,     &
                              XUNIF_H_ICE,      &
                              XUNIF_H_ML,       &
                              XUNIF_H_B1,       &
                              LCLIM_LAKE   
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA
 CHARACTER(LEN=7),  INTENT(IN)  :: HVAR     ! variable treated
 CHARACTER(LEN=28), INTENT(OUT) :: HFILE    ! file name
 CHARACTER(LEN=6),  INTENT(OUT) :: HFILETYPE! file type
 CHARACTER(LEN=28), INTENT(OUT) :: HFILEPGD    ! file name
 CHARACTER(LEN=6),  INTENT(OUT) :: HFILEPGDTYPE! file type
 CHARACTER(LEN=28), INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=28), INTENT(IN)  :: HPGDFILE    ! atmospheric file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HPGDFILETYPE! atmospheric file type
INTEGER,           INTENT(IN)  :: KLUOUT   ! logical unit of output listing
LOGICAL,           INTENT(OUT) :: OUNIF    ! flag for prescribed uniform field

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
IF (LHOOK) CALL DR_HOOK('READ_PREP_FLAKE_CONF',0,ZHOOK_HANDLE)
HFILE = '                         '
HFILETYPE = '      '
!
HFILEPGD = '                         '
HFILEPGDTYPE = '      '
!
OUNIF     = .FALSE.
!
!-------------------------------------------------------------------------------
!
!* choice of input file
!  --------------------
!
IF (LEN_TRIM(HFILE)==0 .AND. LEN_TRIM(CFILE_FLAKE)>0 .AND. LEN_TRIM(CTYPE)>0) THEN
  HFILE     = CFILE_FLAKE
  HFILETYPE = CTYPE
END IF
!
IF (LEN_TRIM(HFILEPGD)==0 .AND. LEN_TRIM(CFILEPGD_FLAKE)>0 .AND. LEN_TRIM(CTYPEPGD)>0) THEN
  HFILEPGD     = CFILEPGD_FLAKE
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
!-------------------------------------------------------------------------------
!
!* Is an uniform field prescribed?
!  ------------------------------
SELECT CASE (HVAR)
 CASE('TS     ')
   OUNIF = (XTS_UNIF/=XUNDEF)
 CASE('T_SNOW ')
   OUNIF = (XUNIF_T_SNOW/=XUNDEF)
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN          ! all fields but TS 
      HFILE = '                         ' ! are not readed
      HFILETYPE = '      '                ! from grib files
   END IF    
 CASE('T_ICE  ')
   OUNIF = (XUNIF_T_ICE/=XUNDEF)
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
 CASE('T_MNW  ')
   OUNIF = .FALSE.
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
 CASE('T_WML  ')
   OUNIF = (XUNIF_T_WML/=XUNDEF)
   HFILE = '                         '
   HFILETYPE = '      '
 CASE('T_BOT  ')
   OUNIF = (XUNIF_T_BOT/=XUNDEF)
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
 CASE('T_B1   ')
   OUNIF = (XUNIF_T_B1/=XUNDEF)
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
 CASE('CT     ')
   OUNIF = (XUNIF_CT/=XUNDEF)
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
 CASE('H_SNOW ')
   OUNIF = (XUNIF_H_SNOW/=XUNDEF)
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
 CASE('H_ICE  ')
   OUNIF = (XUNIF_H_ICE/=XUNDEF)
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
 CASE('H_ML   ')
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
   OUNIF = (XUNIF_H_ML/=XUNDEF)
 CASE('H_B1   ')
   OUNIF = (XUNIF_H_B1/=XUNDEF)
   IF (HFILETYPE=='GRIB  '.OR.HFILETYPE=='ASCLLV') THEN
      HFILE = '                         '
      HFILETYPE = '      '
   END IF    
END SELECT
!
!
!-------------------------------------------------------------------------------
!
!* If no file and no uniform field is prescribed:  default values used
!  ---------------------------------------------
!
IF (LEN_TRIM(HFILETYPE)==0 .AND. .NOT. OUNIF) THEN
   SELECT CASE (HVAR)
     CASE ('ZS     ')
       OUNIF = .TRUE.
       IF (LHOOK) CALL DR_HOOK('READ_PREP_FLAKE_CONF',1,ZHOOK_HANDLE)
       RETURN
     CASE ('DATE   ')
       IF (LHOOK) CALL DR_HOOK('READ_PREP_FLAKE_CONF',1,ZHOOK_HANDLE)
       RETURN
     CASE('TS     ') ! an input file or a uniform value must be given for TS
       CALL ABOR1_SFX('READ_PREP_FLAKE_CONF: AN INPUT FILE OR A UNIFORM PRESCRIBED TS REQUIRED')
   END SELECT
END IF
IF (LHOOK) CALL DR_HOOK('READ_PREP_FLAKE_CONF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_FLAKE_CONF
