!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_INIT_WRITE_TXT
CONTAINS
      SUBROUTINE INIT_WRITE_TXT (HSELECT, HREC,OWFL)
!     ######################
!
!!****  *INIT_WRITE_TXT_n* Initialize array name to be written and associated
!!                         unit number
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      A. LEMONSU     *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_IO_SURF_TXT,ONLY:NMASK, NFULL, CMASK
USE MODD_WRITE_TXT,  ONLY:NVAR, CVAR, CVARN, JPVAR, NIND, NNUM_RECORDS
!
USE MODI_ABOR1_SFX
!USE MODI_TEST_RECORD_LEN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
!
 CHARACTER(LEN=12),   INTENT(IN)     :: HREC
LOGICAL,             INTENT(INOUT)  :: OWFL
INTEGER                             :: IP, IVAR, IVAR_UNIT, IFIELD, JFIELD
LOGICAL                             :: LFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_WRITE_TXT',0,ZHOOK_HANDLE)
!
LFOUND = .FALSE.
IVAR_UNIT=-1
DO IP=1, JPVAR
  IF (HREC==CVAR(IP)) THEN
    IVAR_UNIT=NVAR(IP)
    LFOUND = .TRUE.
    OWFL = .TRUE.
    EXIT
  ELSEIF(HREC==CVARN(IP)) THEN
    LFOUND = .TRUE.
    OWFL = .FALSE.
    EXIT
  ENDIF
ENDDO
!
!
IF (.NOT. LFOUND) THEN
!
  IF (CVAR(1) == '                ') THEN
    IVAR = 1
  ELSE
    IVAR = NNUM_RECORDS + 1
  ENDIF
!
!
  IF (SIZE(HSELECT)==0) THEN
!
    IF ( (HREC(5:7)/='_OC'                          ) .AND.  &
          (HREC(4:6)/='_OC'                          ) .AND.  &
          (HREC(1:3)/='SEA'                          ) .AND.  &
          (HREC(1:2)/='DX'                           ) .AND.  &
          (HREC(1:2)/='DY'                           ) .AND.  &
          (HREC(1:4)/='CLAY'                         ) .AND.  &
          (HREC(1:4)/='SAND'                         ) .AND.  &
          (HREC(1:2)/='ZS'                           ) .AND.  &
          (HREC(1:4)/='SSO_'                         ) .AND.  &
          (HREC(1:4)/='Q2M_'                         ) .AND.  &
          (HREC(1:4)/='RESA'                         ) .AND.  &
          (HREC(1:3)/='RI_'                          ) .AND.  &
          (HREC(1:5)/='REG_L'                        ) .AND.  &
          (HREC(1:3)/='AOS'                          ) .AND.  &
          (HREC(1:3)/='HO2'                          ) .AND.  &
          (HREC(1:3)/='RGL'                          ) .AND.  &
          (HREC(1:3)/='SWD'                          ) .AND.  &
          (HREC(1:3)/='SWU'                          ) .AND.  &
          (HREC(1:3)/='LWD'                          ) .AND.  &
          (HREC(1:3)/='LWU'                          ) .AND.  &
          (HREC(1:3)/='ALB'                          ) .AND.  &
          (HREC(1:2)/='DG'                           ) .AND.  &
          (HREC(1:5)/='DROOT'                        ) .AND.  &
          (HREC(1:4)/='DTOT'                         ) .AND.  &
          (HREC(1:7)/='RUNOFFD'                      ) .AND.  &
          (HREC(1:8)/='ROOTFRAC'                     ) .AND.  &
          (HREC(1:4)/='WSAT'                         ) .AND.  &
          (HREC(1:3)/='WFC'                          ) .AND.  &
          (HREC(1:5)/='WWILT'                        ) .AND.  &
          (HREC(1:4)/='DICE'                         ) .AND.  &
          (HREC(1:2)/='CV'                           ) .AND.  &
          (HREC(1:5)/='GAMMA'                        ) .AND.  &
          (HREC(1:5)/='RSMIN'                        ) .AND.  &
          (HREC(1:5)/='WRMAX'                        ) .AND.  &
          (HREC(1:5)/='Z0REL'                        ) .AND.  &
          (HREC(1:5)/='Z0SEA'                        ) .AND.  &
          (HREC(1:7)/='Z0WATER'                      ) .AND.  &
          (HREC(4:6)/='_ZS'                          ) .AND.  &
          (HREC(1:7)/='VEGTYPE'                      ) .AND.  &
          (HREC(1:5)/='COVER'                        ) .AND.  &
          (HREC(1:5)/='IRRIG'                        ) .AND.  &
          (HREC(1:4)/='TI_R'                         ) .AND.  &
          (HREC(1:3)/='CD_'                          ) .AND.  &
          (HREC(1:3)/='CE_'                          ) .AND.  &
          (HREC(1:3)/='CH_'                          ) .AND.  &
          (HREC(1:4)/='FMU_'                         ) .AND.  &
          (HREC(1:4)/='FMV_'                         ) .AND.  &
          (HREC(1:6)/='DRIVEG'                       ) .AND.  &
          (HREC(1:5)/='RRVEG'                        ) .AND.  &
          (HREC(1:8)/='BLD_DESC'                     ) .AND.  &
          (HREC(1:2)/='Z0'                           )        ) THEN

      IF (IVAR > JPVAR) THEN
        CALL ABOR1_SFX('TOO MANY FIELDS TO BE WRITTEN IN THE "TEXTE" TYPE TIMESERIES')
      END IF

      OPEN(NEWUNIT=IVAR_UNIT,FILE=TRIM(HREC)//'.TXT',FORM='FORMATTED')
      CVAR(IVAR) = HREC
      NVAR(IVAR) = IVAR_UNIT
      NNUM_RECORDS = IVAR
      OWFL=.TRUE.

    ELSE
      IP = 1
      DO WHILE (CVARN(IP).NE.'                ')
        IP=IP+1
      ENDDO
      CVARN(IP) = HREC
      OWFL=.FALSE.
    ENDIF
!
  ELSE
!
    IFIELD=0
    DO JFIELD=1,SIZE(HSELECT)
      IF (HSELECT(JFIELD)== '            ') EXIT
      IFIELD=IFIELD+1
    ENDDO

    !CALL TEST_RECORD_LEN("ASCII ",HREC,HSELECT,LMATCH)

    !IF (.NOT. LMATCH ) THEN


      IF (IVAR > JPVAR) THEN
        CALL ABOR1_SFX('TOO MANY FIELDS TO BE WRITTEN IN THE "TEXTE" TYPE TIMESERIES')
      END IF

      OPEN(NEWUNIT=IVAR_UNIT,FILE=TRIM(HREC)//'.TXT',FORM='FORMATTED')
      CVAR(IVAR) = HREC
      NVAR(IVAR) = IVAR_UNIT
      NNUM_RECORDS = IVAR
      OWFL=.TRUE.

    !ELSE
    !  OWFL=.FALSE.
    !ENDIF

  ENDIF
ENDIF

NIND=IVAR_UNIT
IF (LHOOK) CALL DR_HOOK('INIT_WRITE_TXT',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE INIT_WRITE_TXT
END MODULE MODI_INIT_WRITE_TXT
