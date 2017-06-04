!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_SURF_ATM_CONF(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                                         HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KLUOUT)
!     #######################################################
!
!!****  *READ_PREP_SURF_ATM_CONF* - routine to read the configuration for
!!                                  the surface
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
!!      S.Malardel   *Meteo France*
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
USE MODE_POS_SURF
!
USE MODN_PREP_SURF_ATM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM    ! program calling ISBA
 CHARACTER(LEN=28), INTENT(OUT) :: HFILE       ! file name
 CHARACTER(LEN=6),  INTENT(OUT) :: HFILETYPE   ! file type
 CHARACTER(LEN=28), INTENT(OUT) :: HFILEPGD    ! file name
 CHARACTER(LEN=6),  INTENT(OUT) :: HFILEPGDTYPE! file type
 CHARACTER(LEN=28), INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=28), INTENT(IN)  :: HPGDFILE    ! atmospheric file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HPGDFILETYPE! atmospheric file type
INTEGER,           INTENT(IN)  :: KLUOUT      ! logical unit of output listing



!
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
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_SURF_ATM_CONF',0,ZHOOK_HANDLE)
HFILE     = CFILE
HFILETYPE = CFILETYPE
HFILEPGD     = CFILEPGD
HFILEPGDTYPE = CFILEPGDTYPE
!
IF (LEN_TRIM(HFILE)==0 .AND. LEN_TRIM(HATMFILE)>0 ) THEN
   HFILE     = HATMFILE
ENDIF
!
IF (LEN_TRIM(HFILEPGD)==0 .AND. LEN_TRIM(HPGDFILE)>0 ) THEN
   HFILEPGD  = HPGDFILE
ENDIF
!
IF (LEN_TRIM(HFILETYPE)==0 .AND. LEN_TRIM(HATMFILETYPE)>0 ) THEN
   HFILETYPE    = HATMFILETYPE
ENDIF
!
IF (LEN_TRIM(HFILEPGDTYPE)==0 .AND. LEN_TRIM(HPGDFILETYPE)>0 ) THEN
   HFILEPGDTYPE    = HPGDFILETYPE
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_SURF_ATM_CONF',1,ZHOOK_HANDLE)

!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_SURF_ATM_CONF
