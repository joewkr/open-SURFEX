!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_SEAFLUX_CONF (OMERCATOR, HPROGRAM,HVAR,HFILE,HFILETYPE,&
                                         HFILEPGD,HFILEPGDTYPE, HATMFILE,HATMFILETYPE,&
                                         HPGDFILE,HPGDFILETYPE,KLUOUT,OUNIF)
!     #######################################################
!
!!****  *READ_PREP_SEAFLUX_CONF* - routine to read the configuration for 
!!                                 SEAFLUX fields preparation
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
!!      S. Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      P. Le Moigne 10/2005, Phasage Arome
!!      C. Lebeaupin 01/2008  Add oceanic variables initialization
!!      Modified     09/2013  S. Senesi : introduce variables for sea-ice scheme 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_OCEAN_n, ONLY : OCEAN_t
!
USE MODN_PREP_SEAFLUX
!
USE MODI_READ_PREP_SURF_ATM_CONF
USE MODI_PREP_OCEAN_MERCATORVERGRID
!
USE MODD_PREP_SEAFLUX, ONLY : CFILE_SEAFLX, CTYPE_SEAFLX, CFILEPGD_SEAFLX, CTYPEPGD, &
                              XSST_UNIF, XSSS_UNIF, XSIC_UNIF
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
LOGICAL, INTENT(IN) :: OMERCATOR
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
IF (LHOOK) CALL DR_HOOK('READ_PREP_SEAFLUX_CONF',0,ZHOOK_HANDLE)
HFILE = '                         '
HFILETYPE = '      '
!
HFILEPGD = '                            '
HFILEPGDTYPE = '      '
!
OUNIF     = .FALSE.
!
!-------------------------------------------------------------------------------
!
!* Select seaflux files if they are defined
!  -----------------------------------------
!
IF (LEN_TRIM(HFILE)==0 .AND. LEN_TRIM(CFILE_SEAFLX)>0 .AND. LEN_TRIM(CTYPE_SEAFLX)>0) THEN
  HFILE     = CFILE_SEAFLX
  HFILETYPE = CTYPE_SEAFLX
END IF
!
IF (LEN_TRIM(HFILEPGD)==0 .AND. LEN_TRIM(CFILEPGD_SEAFLX)>0 .AND. LEN_TRIM(CTYPEPGD)>0) THEN
  HFILEPGD     = CFILEPGD_SEAFLX
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
   CASE ('SST    ') 
      OUNIF = (XSST_UNIF/=XUNDEF) 
   CASE ('SSS    ') 
      IF (CSEAICE_SCHEME == 'NONE  '.AND. &
         LEN_TRIM(HFILETYPE)==0.0   .AND. &
         XSSS_UNIF==XUNDEF                )THEN
         XSSS_UNIF=0.0
      ENDIF
      OUNIF = (XSSS_UNIF/=XUNDEF)
   CASE ('SIC    ') 
      OUNIF = (XSIC_UNIF/=XUNDEF) 
END SELECT

!
!-------------------------------------------------------------------------------
!
!* If no file and no uniform field is prescribed: error
!  ---------------------------------------------
!
IF (HVAR=='DATE   ' .OR. HVAR=='ZS     ') THEN
  OUNIF = (HFILETYPE=='      ')
  IF (LHOOK) CALL DR_HOOK('READ_PREP_SEAFLUX_CONF',1,ZHOOK_HANDLE)
  RETURN
END IF
!
IF (LEN_TRIM(HFILETYPE)==0 .AND. .NOT. OUNIF) THEN
   CALL ABOR1_SFX('READ_PREP_SEAFLUX_CONF: AN INPUT VALUE IS REQUIRED FOR '//HVAR)
END IF
!
!-------------------------------------------------------------------------------
!
!* If 1D coupling: ocean variables initializing
!  --------------------------------------------
!
IF (OMERCATOR) THEN
  WRITE(KLUOUT,*) 'LMERCATOR=T : initializing oceanic vertical grid'
  CALL PREP_OCEAN_MERCATORVERGRID(HPROGRAM,OUNIF)
END IF
IF (LHOOK) CALL DR_HOOK('READ_PREP_SEAFLUX_CONF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_SEAFLUX_CONF
