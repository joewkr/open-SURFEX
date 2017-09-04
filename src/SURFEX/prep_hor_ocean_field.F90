!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_OCEAN_FIELD (DTCO, UG, U, GCP, O, OR, KLAT, HPROGRAM,   &
                                 HFILE,HFILETYPE,KLUOUT,OUNIF,   &
                                 HSURF,HNCVARNAME                )
!     #######################################################
!
!!****  *PREP_HOR_OCEAN_FIELD* -reads, interpolates and prepares oceanic fields
!!
!!    PURPOSE
!!    -------
!!
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
!
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_OCEAN_n, ONLY : OCEAN_t
USE MODD_OCEAN_REL_n, ONLY : OCEAN_REL_t
!
USE MODD_CSTS,           ONLY : XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_OCEAN_GRID,   ONLY : NOCKMIN,NOCKMAX
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE, XLAT_OUT, XLON_OUT,&
                                XX_OUT, XY_OUT
!
USE MODI_PREP_OCEAN_UNIF
USE MODI_PREP_OCEAN_NETCDF
USE MODI_PREP_OCEAN_ASCLLV
!
USE MODI_HOR_INTERPOL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
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
INTEGER, INTENT(IN) :: KLAT
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
LOGICAL,            INTENT(IN)  :: OUNIF     ! flag for prescribed uniform field
 CHARACTER(LEN=7)                :: HSURF   ! type of field
 CHARACTER(LEN=28),  INTENT(IN), OPTIONAL :: HNCVARNAME!var to read
!
!
!*      0.2    declarations of local variables
!
REAL, POINTER, DIMENSION(:,:,:)    ::ZFIELDIN=>NULL()!field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:)      ::ZFIELD=>NULL()  !field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:)::ZFIELDOUT!field interpolated horizontally
!
INTEGER                       :: JLEV    ! loop on oceanic vertical level
INTEGER                       :: IK1
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!*      1.     Does the field exist?
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_OCEAN_FIELD',0,ZHOOK_HANDLE)
!
IF (OUNIF) THEN
   WRITE(KLUOUT,*) '*****warning*****: you ask for uniform oceanic variables'
   CALL PREP_OCEAN_UNIF(KLUOUT,HSURF,ZFIELDIN)
ELSE IF (HFILETYPE=='NETCDF') THEN
   CALL PREP_OCEAN_NETCDF(HPROGRAM,HSURF,HFILE,HFILETYPE,KLUOUT,HNCVARNAME,ZFIELDIN)
ELSE IF (HFILETYPE=='ASCII') THEN
   WRITE(KLUOUT,*) 'PERSONAL LIB TEST FOR READING ',HFILETYPE,'file type'
   WRITE(KLUOUT,*) 'ASCII FILE MUST CONTAIN LAT,LON,DEPTH,T,S,U,V'
   CALL PREP_OCEAN_ASCLLV(DTCO, UG, U, HPROGRAM,HSURF,HFILE,KLUOUT,ZFIELDIN)
ELSE
  CALL ABOR1_SFX('PREP_OCEAN_HOR_FIELD: data file type not supported : '//HFILETYPE)
END IF
!
!-------------------------------------------------------------------------------
!
!*      3.     Horizontal interpolation
!
ALLOCATE(ZFIELDOUT  (KLAT,SIZE(ZFIELDIN,2),SIZE(ZFIELDIN,3)) )
ALLOCATE(ZFIELD(SIZE(ZFIELDIN,1),SIZE(ZFIELDIN,3)))
!
DO JLEV=1,SIZE(ZFIELDIN,2)
  ZFIELD(:,:)=ZFIELDIN(:,JLEV,:)
  CALL HOR_INTERPOL(DTCO, U, GCP, KLUOUT,ZFIELD,ZFIELDOUT(:,JLEV,:))
ENDDO
!
!*      5.     Return to historical variable
!
IK1=NOCKMIN+1
SELECT CASE (HSURF)
  CASE('TEMP_OC')
    ALLOCATE(O%XSEAT(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    ALLOCATE(OR%XSEAT_REL(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    DO JLEV=IK1,NOCKMAX
      O%XSEAT(:,JLEV) = ZFIELDOUT(:,JLEV,1)
      !prevoir interpolation sur la grille verticale si niveau différents
    ENDDO
    O%XSEAT(:,NOCKMIN)=O%XSEAT(:,IK1)
    !
    ! Relaxation Profile = initial profile for the steady regime
    ! Change it for seasonal cycle!!
    OR%XSEAT_REL(:,:) = O%XSEAT(:,:)
    !
  CASE('SALT_OC')
    ALLOCATE(O%XSEAS(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    ALLOCATE(OR%XSEAS_REL(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    DO JLEV=IK1,NOCKMAX
      O%XSEAS(:,JLEV) = ZFIELDOUT(:,JLEV,1)
    ENDDO
    O%XSEAS(:,NOCKMIN)=O%XSEAS(:,IK1)
    !
    ! Relaxation Profile = initial profile for the steady regime
    ! Change it for seasonal cycle!!
    OR%XSEAS_REL(:,:) = O%XSEAS(:,:)
    !
  CASE('UCUR_OC')
    ALLOCATE(O%XSEAU(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    ALLOCATE(OR%XSEAU_REL(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    DO JLEV=IK1,NOCKMAX
      O%XSEAU(:,JLEV) = ZFIELDOUT(:,JLEV,1)
    ENDDO
    O%XSEAU(:,NOCKMIN)=O%XSEAU(:,IK1)
    !
    IF (.NOT.O%LCURRENT) O%XSEAU(:,:)=0.
    !
    OR%XSEAU_REL(:,:) = O%XSEAU(:,:)
    !
  CASE('VCUR_OC')
    ALLOCATE(O%XSEAV(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    ALLOCATE(OR%XSEAV_REL(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    DO JLEV=IK1,NOCKMAX
      O%XSEAV(:,JLEV) = ZFIELDOUT(:,JLEV,1)
    ENDDO
    O%XSEAV(:,NOCKMIN)=O%XSEAV(:,IK1)
    !
    IF (.NOT.O%LCURRENT) O%XSEAV(:,:)=0.
    !
    OR%XSEAV_REL(:,:) = O%XSEAV(:,:)
    !
END SELECT
!
!------------------------------------------------------------------------------
!
!*      6.     Deallocations
!
DEALLOCATE(ZFIELD   )
DEALLOCATE(ZFIELDOUT)
IF (LHOOK) CALL DR_HOOK('PREP_HOR_OCEAN_FIELD',1,ZHOOK_HANDLE)
!
END SUBROUTINE PREP_HOR_OCEAN_FIELD
