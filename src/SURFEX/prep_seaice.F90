!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SEAICE (UG, DTCO, DTS, O, OR, KLAT, S, U, GCP, &
                        HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_SEAICE* - prepares variables for SEAICE scheme (for now : Gelato only)
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     S. Sénési 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014
!!------------------------------------------------------------------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_OCEAN_n, ONLY : OCEAN_t
USE MODD_OCEAN_REL_n, ONLY : OCEAN_REL_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODI_GET_LUOUT
USE MODI_GET_TYPE_DIM_N
USE MODI_GLTOOLS_READNAM
!
USE MODD_TYPES_GLT,   ONLY : T_GLT
!
USE MODE_PREP_CTL, ONLY : PREP_CTL
!
USE MODN_PREP_SEAFLUX,   ONLY : CPREP_SEAICE_SCHEME => CSEAICE_SCHEME
USE MODI_PREP_HOR_SEAFLUX_FIELD
!
USE MODD_GLT_PARAM, ONLY : nl, nt, nx, ny, nxglo, nyglo 
USE MODI_GLTOOLS_ALLOC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(OCEAN_t), INTENT(INOUT) :: O
TYPE(OCEAN_REL_t), INTENT(INOUT) :: OR
INTEGER, INTENT(IN) :: KLAT
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: IK,IL  ! loop counter on ice categories and layers 
INTEGER :: JMTH,INMTH
INTEGER :: ILUOUT
LOGICAL :: GFOUND         ! Return code when searching namelist
INTEGER :: ILUNAM         ! logical unit of namelist file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAICE',0,ZHOOK_HANDLE)
!
!*      0.     Default of configuration
!
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------------
!
!*      1.     Interpret namelist
!
S%CSEAICE_SCHEME=CPREP_SEAICE_SCHEME
IF ( S%CSEAICE_SCHEME == 'GELATO' ) THEN
  CALL GLTOOLS_READNAM(.FALSE.,ILUOUT)
ENDIF
!
S%LHANDLE_SIC = .FALSE.
IF(TRIM(S%CSEAICE_SCHEME)/='NONE' .OR. TRIM(S%CINTERPOL_SIC)/='NONE' )THEN
  S%LHANDLE_SIC=.TRUE.
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations of Seaice cover
!
IF (S%LHANDLE_SIC) THEN 
   CALL PREP_HOR_SEAFLUX_FIELD(DTCO, UG, U, GCP, DTS, O, OR, KLAT, S, &
                               HPROGRAM,'SIC    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      3.     Optional preparation of interpolation of monthly sea ice cover and sea 
!              ice thickness 
!
S%LINTERPOL_SIC=.FALSE.
IF(TRIM(S%CINTERPOL_SIC)/='NONE')THEN
   S%LINTERPOL_SIC=.TRUE.
ENDIF
!
IF(TRIM(S%CINTERPOL_SIT)/='NONE')THEN
   S%LINTERPOL_SIT=.TRUE.
ENDIF
!
IF(S%LINTERPOL_SIC)THEN
   !
   ! Precedent, Current, Next, and Second-next Monthly SIC
   INMTH=4
   !
   ALLOCATE(S%XSIC_MTH(SIZE(S%XSIC),INMTH))
   DO JMTH=1,INMTH
      S%XSIC_MTH(:,JMTH)=S%XSIC(:)
   ENDDO
!
ENDIF
!
IF(S%LINTERPOL_SIT)THEN
   !
   !Precedent, Current, Next, and Second-next Monthly SIT
   INMTH=4
   !
   ALLOCATE(S%XSIT_MTH(SIZE(S%XSIC),INMTH))
   DO JMTH=1,INMTH
      S%XSIT_MTH(:,JMTH)=XUNDEF
   ENDDO
!
ENDIF
!-------------------------------------------------------------------------------------
!
!*      Creating default initial state for Gelato 
!
!
CALL GET_TYPE_DIM_n(DTCO, U, 'SEA   ',nx)
ny=1
nyglo=1
nxglo=nx
CALL GLTOOLS_ALLOC(S%TGLT)
!
!*       G1    Prognostic fields with only space dimension(s) :
!
S%TGLT%ust(:,1)=0.
!
!*       G2     Prognostic fields with space and ice-category dimension(s) :
!
! sea ice age 
S%TGLT%sit(:,:,1)%age=0.
! melt pond volume 
S%TGLT%sit(:,:,1)%vmp=0.
! sea ice surface albedo 
S%TGLT%sit(:,:,1)%asn=0.
! sea ice fraction 
S%TGLT%sit(:,:,1)%fsi=0.
! sea ice thickness 
S%TGLT%sit(:,:,1)%hsi=1.*S%TGLT%sit(:,:,1)%fsi
! sea ice salinity 
S%TGLT%sit(:,:,1)%ssi=0.
! sea ice surface temperature 
S%TGLT%sit(:,:,1)%tsf=260.
! snow thickness 
S%TGLT%sit(:,:,1)%hsn=0.
! snow density 
S%TGLT%sit(:,:,1)%rsn=100.
!
!*       G3     Prognostic fields with space, ice-category and layer dimensions :
!
! sea ice vertical gltools_enthalpy profile for all types and levels
S%TGLT%sil(:,:,:,1)%ent=-1000. 
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAICE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_SEAICE
