!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_TEB_CONF(HPROGRAM,HVAR,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,   &
                                    HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KLUOUT,OUNIF)
!     #######################################################
!
!!****  *READ_PREP_TEB_CONF* - routine to read the configuration for TEB
!!                             fields preparation
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
USE MODN_PREP_TEB, ONLY : CFILE_TS, CTYPE_TS
USE MODD_PREP_TEB, ONLY : CFILE_TEB, CTYPE, CFILEPGD_TEB, CTYPEPGD,                 &
                          CFILE_WS, CTYPE_WS, XWS_ROOF, XWS_ROAD,                   &
                          XTS_ROOF, XTS_ROAD, XTS_WALL, XTI_BLD, XTI_ROAD,          &
                          XT_CAN, XQ_CAN, XWS_ROOF_DEF, XWS_ROAD_DEF, XTI_BLD_DEF,  &
                          XHUI_BLD_DEF, XHUI_BLD  
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODE_THERMOS
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
IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_CONF',0,ZHOOK_HANDLE)
HFILE = '                            '
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
SELECT CASE (HVAR)
  CASE ('WS_ROOF','WS_ROAD')
    IF (LEN_TRIM(CFILE_WS)>0 .AND. LEN_TRIM(CTYPE_WS)>0 ) THEN
      HFILE     = CFILE_WS
      HFILETYPE = CTYPE_WS
    END IF
  CASE ('T_ROOF ','T_ROAD ','T_WALL ','T_WALLA','T_WALLB','T_FLOOR','T_MASS','T_WIN1 ','T_CAN  ','Q_CAN')
    IF (LEN_TRIM(CFILE_TS)>0 .AND. LEN_TRIM(CTYPE_TS)>0 ) THEN
      HFILE     = CFILE_TS
      HFILETYPE = CTYPE_TS
    END IF
END SELECT
!
IF (LEN_TRIM(HFILE)==0 .AND. LEN_TRIM(CFILE_TEB)>0 .AND. LEN_TRIM(CTYPE)>0) THEN
  HFILE     = CFILE_TEB
  HFILETYPE = CTYPE
END IF
!
IF (LEN_TRIM(HFILEPGD)==0 .AND. LEN_TRIM(CFILEPGD_TEB)>0 .AND. LEN_TRIM(CTYPEPGD)>0) THEN
  HFILEPGD     = CFILEPGD_TEB
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
!
SELECT CASE (HVAR)
  CASE ('WS_ROOF')
    OUNIF = (XWS_ROOF/=XUNDEF) 
  CASE ('WS_ROAD')
    OUNIF = (XWS_ROAD/=XUNDEF) 
  CASE ('TI_BLD ')
    OUNIF = (XTI_BLD/=XUNDEF)  
  CASE ('TI_ROAD')
    OUNIF = (XTI_ROAD/=XUNDEF)    
  CASE ('T_ROAD ')
    OUNIF = (XTS_ROAD/=XUNDEF) 
  CASE ('T_WALL ','T_WALLA','T_WALLB')
    OUNIF = (XTS_WALL/=XUNDEF)     
  CASE ('T_ROOF ')
    OUNIF = (XTS_ROOF/=XUNDEF) 
  CASE ('T_FLOOR')
    OUNIF = (XTI_ROAD/=XUNDEF)     
  CASE ('T_MASS') 
    OUNIF = (XTI_BLD/=XUNDEF)
  CASE ('T_WIN1') 
    OUNIF = (XTS_WALL/=XUNDEF)
  CASE ('T_WIN2') 
    OUNIF = (XTI_BLD/=XUNDEF)
  CASE ('QI_BLD ')
    OUNIF = (XHUI_BLD/=XUNDEF .AND. XTI_BLD/=XUNDEF)     
END SELECT
!
!-------------------------------------------------------------------------------
!
!* building temperature available for temperature profiles when file is present
!  ----------------------------------------------------------------------------
!
!IF (LEN_TRIM(HFILETYPE)>0 .AND. .NOT. OUNIF) THEN
!  IF (HVAR=='T_ROOF ' .OR. HVAR=='T_WALL' .OR. HVAR=='TI_BLD' .AND. XTI_BLD==XUNDEF) XTI_BLD=XTI_BLD_DEF
!END IF
!
!-------------------------------------------------------------------------------
!
!* If no file and no uniform field is prescribed: default values used
!  ---------------------------------------------
!
IF (LEN_TRIM(HFILETYPE)==0 .AND. .NOT. OUNIF) THEN
  SELECT CASE (HVAR)
    CASE ('ZS     ')
      OUNIF = .TRUE.
    CASE ('WS_ROOF')
      XWS_ROOF = XWS_ROOF_DEF
      OUNIF = .TRUE.
    CASE ('WS_ROAD')
      XWS_ROAD = XWS_ROAD_DEF
      OUNIF = .TRUE.
    CASE ('TI_BLD ')
      XTI_BLD  = XTI_BLD_DEF
      OUNIF = .TRUE.
    CASE ('Q_CAN  ')
      IF (XT_CAN/=XUNDEF) THEN
         XQ_CAN = XHUI_BLD_DEF * QSAT(XT_CAN, 100000.)
         OUNIF = .TRUE.
      ELSE 
         CALL ABOR1_SFX("READ_PREP_TEB_CONF: DON'T KNOW HOW TO INITIALIZE Q_CAN ")
      END IF            
    CASE ('T_CAN  ')  
      IF (XTS_ROAD/=XUNDEF) THEN
        XT_CAN = XTS_ROAD
      ELSE IF (XTS_WALL/=XUNDEF) THEN
        XT_CAN = XTS_WALL
      ELSE IF (XTS_ROOF/=XUNDEF) THEN
        XT_CAN = XTS_ROOF
      ELSE
        CALL ABOR1_SFX('READ_PREP_TEB_CONF: AN INPUT VALUE IS REQUIRED FOR '//HVAR)
      END IF 
    CASE ('T_WIN1  ')
      IF (XTS_WALL==XUNDEF) THEN
         CALL ABOR1_SFX('READ_PREP_TEB_CONF: AN INPUT VALUE IS REQUIRED FOR TS_WALL TO INITIALIZE T_WIN1')
      ELSE 
         OUNIF = .TRUE.
      ENDIF
    CASE ('T_WIN2  ') 
      XTI_BLD  = XTI_BLD_DEF
      OUNIF = .TRUE.
    CASE ('QI_BLD  ') 
      XHUI_BLD  = XHUI_BLD_DEF
      OUNIF = .TRUE.
    CASE ('DATE   ')
      IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_CONF',1,ZHOOK_HANDLE)
      RETURN
    CASE ('SN_ROOF','SN_ROAD')
      OUNIF = .TRUE.
    CASE DEFAULT
      CALL ABOR1_SFX('READ_PREP_TEB_CONF: AN INPUT FILE OR A UNIFORM VALUE IS REQUIRED FOR '//HVAR)
   END SELECT
END IF
!
!-------------------------------------------------------------------------------
!
!* no file given ? nor specific value in namelist? One takes the default value.
!
IF (HFILETYPE=='      ' .AND. .NOT. OUNIF) THEN
  IF (HVAR(1:2)/='ZS') WRITE(KLUOUT,*) 'NO FILE FOR FIELD ',HVAR, &
                                        ': UNIFORM DEFAULT FIELD IS PRESCRIBED'
  OUNIF = .TRUE.
END IF
IF (LHOOK) CALL DR_HOOK('READ_PREP_TEB_CONF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_TEB_CONF
