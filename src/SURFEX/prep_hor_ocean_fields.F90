!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_OCEAN_FIELDS (DTCO, UG, U, GCP, O, OR, KLAT, PSEABATHY, &
                                  HPROGRAM,HSURF,HFILE,HFILETYPE,KLUOUT,OUNIF)
!     #######################################################
!
!
!!****  *PREP_HOR_OCEAN_FIELDS* - prepares all oceanic fields for the 1D oceanic model
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
!!     C. Lebeaupin Brossier 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!!      Modified    07/2012, P. Le Moigne : CMO1D phasing
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_OCEAN_n, ONLY : OCEAN_t
USE MODD_OCEAN_REL_n, ONLY : OCEAN_REL_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_OCEAN_CSTS,     ONLY : XRHOSWREF
USE MODD_OCEAN_GRID, ONLY : XDZ1,XZHOC,NOCKMIN,NOCKMAX
!
USE MODI_PREP_HOR_OCEAN_FIELD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(OCEAN_t), INTENT(INOUT) :: O
TYPE(OCEAN_REL_t), INTENT(INOUT) :: OR
REAL, DIMENSION(:), INTENT(IN) :: PSEABATHY
INTEGER, INTENT(IN) :: KLAT
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
LOGICAL,            INTENT(IN)  :: OUNIF     ! flag for prescribed uniform field
!
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=8)                    :: YSURF   ! type of field
 CHARACTER(LEN=28)                   :: YNCVARNAME   ! variable to read
!
INTEGER                             :: IL        ! number of points
INTEGER                             :: IK1
INTEGER                             :: J, JLEV   ! loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*      1.     Patch
!
!---------------------------------------------------------------------------
!
!*      3.     Treatment of oceanic temperature
IF (LHOOK) CALL DR_HOOK('PREP_HOR_OCEAN_FIELDS',0,ZHOOK_HANDLE)
YSURF='TEMP_OC'
YNCVARNAME='temperature'
 CALL PREP_HOR_OCEAN_FIELD(DTCO, UG, U, GCP, O, OR, KLAT, &
                           HPROGRAM,HFILE,HFILETYPE,KLUOUT,OUNIF,YSURF,YNCVARNAME)
!---------------------------------------------------------------------------
!
!*      4.     Treatment of oceanic salinity
YSURF='SALT_OC'
YNCVARNAME='salinity'
 CALL PREP_HOR_OCEAN_FIELD(DTCO, UG, U, GCP, O, OR, KLAT, &
                           HPROGRAM,HFILE,HFILETYPE,KLUOUT,OUNIF,YSURF,YNCVARNAME)
!---------------------------------------------------------------------------
!
!*      5.     Treatment of oceanic current
YSURF='UCUR_OC'
YNCVARNAME='u'
 CALL PREP_HOR_OCEAN_FIELD(DTCO, UG, U, GCP, O, OR, KLAT, &
                           HPROGRAM,HFILE,HFILETYPE,KLUOUT,OUNIF,YSURF,YNCVARNAME)
YSURF='VCUR_OC'
YNCVARNAME='v'
 CALL PREP_HOR_OCEAN_FIELD(DTCO, UG, U, GCP, O, OR, KLAT, &
                           HPROGRAM,HFILE,HFILETYPE,KLUOUT,OUNIF,YSURF,YNCVARNAME)
!---------------------------------------------------------------------------
!
IK1=NOCKMIN+1
IL = SIZE(O%XSEAT,1)
IF (IL/=0) THEN
  ALLOCATE(O%XSEAE      (SIZE(O%XSEAT,1),NOCKMIN:NOCKMAX))
  O%XSEAE(:,:)   =1.E-3
  ALLOCATE(O%XSEABATH   (SIZE(O%XSEAT,1),NOCKMIN:NOCKMAX))
  O%XSEABATH(:,:)=1.
  ALLOCATE(O%XSEAHMO    (SIZE(O%XSEAT,1)))
  O%XSEAHMO(:)   =XUNDEF
  ALLOCATE(O%XLE        (SIZE(O%XSEAT,1),NOCKMIN:NOCKMAX))
  ALLOCATE(O%XLK        (SIZE(O%XSEAT,1),NOCKMIN:NOCKMAX))
  ALLOCATE(O%XKMEL      (SIZE(O%XSEAT,1),NOCKMIN:NOCKMAX))
  ALLOCATE(O%XKMELM     (SIZE(O%XSEAT,1),NOCKMIN:NOCKMAX))
  O%XLE(:,:)    =XUNDEF
  O%XLK(:,:)    =XUNDEF
  O%XKMEL(:,:)  =XUNDEF
  O%XKMELM(:,:) =XUNDEF
  ALLOCATE(O%XSEATEND   (SIZE(O%XSEAT,1)))
  O%XSEATEND(:) =XUNDEF
  !
  ALLOCATE(O%XDTFNSOL   (SIZE(O%XSEAT,1)))
  O%XDTFNSOL(:) = XUNDEF
  ALLOCATE(O%XDTFSOL    (SIZE(O%XSEAT,1),NOCKMIN:NOCKMAX))
  O%XDTFSOL(:,:)= XUNDEF  
!!----------------------------------------------------------------------------
!!
!!*      6.     Treatment of bathymetry indice and 
!!              apply bathy mask
  DO J=1,IL
    DO JLEV=IK1+1,NOCKMAX
      IF (PSEABATHY(J)-XZHOC(JLEV)>0.) THEN
        O%XSEABATH(J,JLEV)=0.
        O%XSEAE(J,JLEV)  = XUNDEF
        O%XSEAU(J,JLEV)  = XUNDEF
        O%XSEAV(J,JLEV)  = XUNDEF
        O%XSEAT(J,JLEV)  = XUNDEF
        O%XSEAS(J,JLEV)  = XUNDEF
        !
        OR%XSEAT_REL(J,JLEV)  = XUNDEF
        OR%XSEAS_REL(J,JLEV)  = XUNDEF
        !
        OR%XSEAU_REL(J,JLEV)  = XUNDEF
        OR%XSEAV_REL(J,JLEV)  = XUNDEF
        !        
      ENDIF 
    ENDDO
  ENDDO
!
!---------------------------------------------------------------------------
ENDIF
IF (LHOOK) CALL DR_HOOK('PREP_HOR_OCEAN_FIELDS',1,ZHOOK_HANDLE)
!----------------------------------------------------------------------------
END SUBROUTINE PREP_HOR_OCEAN_FIELDS
