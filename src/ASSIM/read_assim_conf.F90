!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_ASSIM_CONF(HPROGRAM)
!     #######################################################
!
!!****  *READ_ASSIM_CONF* - routine to read the configuration for assimilation
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
!!      T. Aspelien met.no
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_POS_SURF, ONLY : POSNAM
USE MODN_ASSIM,    ONLY : NAM_NACVEG,NAM_ASSIM,LASSIM,CASSIM,&
                          NAM_IO_VARASSIM,NAM_OBS,NAM_VAR,NAM_ENS
USE MODD_ASSIM,    ONLY : NVAR,NOBSTYPE,XTPRT,XTPRT_M,XSIGMA,&
                          XSIGMA_M,CVAR,CVAR_M,COBS,NNCO,&
                          NVARMAX,NNCV,LASSIM,CASSIM_ISBA,LPRT,&
                          NOBSMAX,COBS_M,XERROBS_M,XERROBS, &
                          XQCOBS_M,XQCOBS,&
                          XINFL_M,XINFL,XADDINFL_M,XADDINFL, &
                          XADDTIMECORR_M, XADDTIMECORR, NIE, &
                          CFILE_FORMAT_OBS
USE YOMHOOK,       ONLY : LHOOK,DR_HOOK
USE PARKIND1,      ONLY : JPRB

USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_TEST_NAM_VAR_SURF  
USE MODI_CLOSE_NAMELIST
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! logical unit of output file
INTEGER           :: INAM           ! logical unit of namelist file
INTEGER           :: I,J
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('READ_ASSIM_CONF',0,ZHOOK_HANDLE)
CALL GET_LUOUT(HPROGRAM,ILUOUT)

!* open namelist file
CALL OPEN_NAMELIST(HPROGRAM,INAM)

!* reading of namelist
CALL POSNAM(INAM,'NAM_ASSIM',      GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_ASSIM)
CALL POSNAM(INAM,'NAM_NACVEG',     GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_NACVEG)
CALL POSNAM(INAM,'NAM_IO_VARASSIM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_IO_VARASSIM)
CALL POSNAM(INAM,'NAM_OBS',        GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_OBS)
CALL POSNAM(INAM,'NAM_VAR',        GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_VAR)
CALL POSNAM(INAM,'NAM_ENS',        GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=INAM,NML=NAM_ENS)
!
CALL TEST_NAM_VAR_SURF(ILUOUT,'CASSIM',CASSIM,'PLUS ','2DVAR','AVERA')
CALL TEST_NAM_VAR_SURF(ILUOUT,'CASSIM_ISBA',CASSIM_ISBA,'OI   ','EKF  ','ENKF ')
!
!* close namelist file
CALL CLOSE_NAMELIST(HPROGRAM,INAM)

! Set EKF setup based on namelist input
IF ( ( CASSIM_ISBA == "EKF" .AND. ( LASSIM.OR.LPRT ) ) .OR. &
     ( CASSIM_ISBA == "ENKF" .AND. ( LASSIM.OR.NIE/=0 ) ) ) THEN
  !
  IF (.NOT.ALLOCATED(XTPRT))   ALLOCATE (XTPRT(NVAR))
  IF (.NOT.ALLOCATED(XSIGMA))  ALLOCATE (XSIGMA(NVAR))
  IF (.NOT.ALLOCATED(CVAR))    ALLOCATE (CVAR(NVAR))
  !
  IF (SUM(NNCV) /= NVAR) THEN
    WRITE(*,*) 'INCONSISTENCY in set-up of CONTROL VARIABLES',SUM(NNCV),NVAR
    CALL ABOR1_SFX('INCONSISTENCY in set-up of CONTROL VARIABLES')
  ENDIF
  !
  J = 1
  DO I = 1,NVARMAX
    IF (NNCV(I) == 1 .AND. J <= NVAR ) THEN
      XTPRT(J) = XTPRT_M(I)
      XSIGMA(J) = XSIGMA_M(I)
      CVAR(J) = CVAR_M(I)
      J = J + 1
    ENDIF
  ENDDO
  CVAR = ADJUSTL(CVAR)
ENDIF

IF ( ( CASSIM_ISBA == "EKF" .AND. ( LASSIM.OR.LPRT ) ) .OR. &
     ( CASSIM_ISBA == "ENKF" .AND. ( LASSIM.OR.NIE/=0 ) ) .OR. (TRIM(CFILE_FORMAT_OBS) == "ASCII") ) THEN

  IF (SUM(NNCO) /= NOBSTYPE) THEN
    WRITE(*,*) 'INCONSISTENCY in set-up of OBSERVATIONS',SUM(NNCO),NOBSTYPE
    CALL ABOR1_SFX('INCONSISTENCY in set-up ofOBSERVATIONS')
  ENDIF
  !
  IF (.NOT.ALLOCATED(COBS)) ALLOCATE (COBS(NOBSTYPE))
  COBS(:) = ''
  IF (.NOT.ALLOCATED(XERROBS)) ALLOCATE (XERROBS(NOBSTYPE))
  IF (.NOT.ALLOCATED(XQCOBS))  ALLOCATE (XQCOBS(NOBSTYPE))
  J = 1
  DO I = 1,NOBSMAX
    IF (NNCO(I) == 1 .AND. J <= NOBSTYPE ) THEN
      IF (J <= NOBSTYPE .AND. (TRIM(COBS_M(I)) == 'T2M' .OR. TRIM(COBS_M(I)) == 'HU2M' .OR. &
          TRIM(COBS_M(I)) == 'WG1' .OR. TRIM(COBS_M(I)) == 'WG2' .OR. TRIM(COBS_M(I)) == 'LAI' .OR. &
          TRIM(COBS_M(I)) == 'SWE') ) THEN
        COBS(J) = TRIM(COBS_M(I))
        XERROBS(J) = XERROBS_M(I)
        XQCOBS(J) = XQCOBS_M(I)
        J = J + 1
      ENDIF
    ENDIF
  ENDDO
ENDIF
 
IF ( CASSIM_ISBA == "ENKF" .AND. ( LASSIM.OR.NIE/=0 ) ) THEN
  !
  IF (.NOT.ALLOCATED(XINFL)) ALLOCATE (XINFL(NVAR))
  IF (.NOT.ALLOCATED(XADDINFL)) ALLOCATE (XADDINFL(NVAR))
  IF (.NOT.ALLOCATED(XADDTIMECORR)) ALLOCATE (XADDTIMECORR(NVAR))
  !
  J = 1
  DO I = 1,NVARMAX
    IF (NNCV(I) == 1 .AND. J <= NVAR ) THEN
      XINFL(J) = XINFL_M(I)
      XADDINFL(J) = XADDINFL_M(I)
      XADDTIMECORR(J) = XADDTIMECORR_M(I)
      J = J + 1
    ENDIF
  ENDDO
  !
ENDIF

IF (LHOOK) CALL DR_HOOK('READ_ASSIM_CONF',1,ZHOOK_HANDLE)
END SUBROUTINE READ_ASSIM_CONF
