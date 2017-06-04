!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_EXTERN (DTCO, IO, U, GCP, &
                             HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,PFIELD,OKEY)
!     #################################################################################
!
!!****  *PREP_ISBA_EXTERN* - initializes ISBA fields from operational GRIB
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme  04/2014, external init with FA files
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK,NPIO
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_ISBA,      ONLY : XGRID_SOIL, XWR_DEF
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODE_READ_EXTERN
!
USE MODI_MAKE_CHOICE_ARRAY
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_INTERP_GRID_NAT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,            INTENT(IN)   :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER   :: PFIELD    ! field to interpolate horizontally (on final soil grid)
LOGICAL, OPTIONAL,  INTENT(INOUT):: OKEY
!
!*      0.2    declarations of local variables
!
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IRESP          ! reading return code
INTEGER           :: INI            ! total 1D dimension
INTEGER           :: IPATCH         ! number of patch
LOGICAL           :: GGLACIER
CHARACTER(LEN=3)  :: YPHOTO
!
REAL, DIMENSION(:,:,:), POINTER     :: ZFIELD=>NULL()         ! field read on initial MNH vertical soil grid, all patches
REAL, DIMENSION(:,:,:), POINTER     :: ZD=>NULL()            ! layer thicknesses
REAL, DIMENSION(:), ALLOCATABLE     :: ZMASK
INTEGER                             :: JP, JL       ! loop counter for patch
INTEGER :: IVERSION, IBUGFIX
LOGICAL :: GDIM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_EXTERN',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
!
 CALL PREP_GRID_EXTERN(GCP,HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
!
YRECFM='VERSION'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
!
IF (NRANK/=NPIO) INI = 0
!
ALLOCATE(ZMASK(INI))
IF (IVERSION>=7) THEN 
  YRECFM='FRAC_NATURE'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZMASK,IRESP,HDIR='A')
ELSE
  ZMASK(:) = 1.
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
 CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
YRECFM='VERSION'
 CALL READ_SURF(HFILETYPE,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(HFILETYPE,YRECFM,IBUGFIX,IRESP)
GDIM = (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>0)
IF (GDIM) CALL READ_SURF(HFILETYPE,'SPLIT_PATCH',GDIM,IRESP)
 CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
IF (NRANK/=NPIO) INI = 0
!
!---------------------------------------------------------------------------------------
!
!*      3.     Transformation into physical quantity to be interpolated
!              --------------------------------------------------------
!
SELECT CASE(HSURF)
!
!*     3.      Orography
!              ---------
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(INI,1,1))
    PFIELD(:,:,:) = XUNDEF
    YRECFM='ZS'
    CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
    CALL READ_SURF(HFILEPGDTYPE,YRECFM,PFIELD(:,1,1),IRESP,HDIR='E')
    CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!--------------------------------------------------------------------------
!
!
!*      3.1    Profile of temperature, water or ice in the soil
!
  CASE('TG    ','WG    ','WGI   ')
!* reading of the profile and its depth definition
     CALL READ_EXTERN_ISBA(U, DTCO, GCP, IO, HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                           KLUOUT,INI,HSURF,HSURF,ZFIELD,ZD,OKEY)
! 
    IF (INI>0) THEN
      ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_SOIL),SIZE(ZFIELD,3)))
      DO JP=1,SIZE(ZFIELD,3)
        CALL INTERP_GRID_NAT(ZD(:,:,JP),ZFIELD(:,:,JP),XGRID_SOIL,PFIELD(:,:,JP))
      END DO
      !
      DO JP=1,SIZE(PFIELD,3)
        DO JL=1,SIZE(PFIELD,2)
          WHERE (ZMASK(:)==0.) PFIELD(:,JL,JP) = XUNDEF
        ENDDO
      ENDDO
      !
    ENDIF
    !
    DEALLOCATE(ZFIELD)
    DEALLOCATE(ZD)
!
!--------------------------------------------------------------------------
!
!*      3.4    Water content intercepted on leaves, LAI
!
  CASE('WR     ')    
     !* number of tiles
     CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
     YRECFM='PATCH_NUMBER'
     CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP,HDIR='-')
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     ALLOCATE(PFIELD(INI,1,IPATCH))
     PFIELD(:,:,:) = XUNDEF
     YRECFM = 'WR'
     CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
     CALL MAKE_CHOICE_ARRAY(HFILETYPE, IPATCH, GDIM, YRECFM, PFIELD(:,1,:),HDIR='E')
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
     DO JP=1,SIZE(PFIELD,3)
       WHERE (ZMASK(:)==0.) PFIELD(:,1,JP) = XUNDEF
     ENDDO     
!
  CASE('LAI    ')
     !* number of tiles
     CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
     YRECFM='PATCH_NUMBER'
     CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP,HDIR='-')
     YRECFM='PHOTO'
     CALL READ_SURF(HFILEPGDTYPE,YRECFM,YPHOTO,IRESP,HDIR='-')     
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     ALLOCATE(PFIELD(INI,1,IPATCH))
     PFIELD(:,:,:) = XUNDEF     
     IF (YPHOTO=='NIT' .OR. YPHOTO=='NCB') THEN
       CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
       YRECFM = 'LAI'
       CALL MAKE_CHOICE_ARRAY(HFILETYPE, IPATCH, GDIM, YRECFM, PFIELD(:,1,:),HDIR='E')
       CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
       DO JP=1,SIZE(PFIELD,3)
         WHERE (ZMASK(:)==0.) PFIELD(:,1,JP) = XUNDEF
       ENDDO       
     ENDIF
!
  CASE('ICE_STO') 
      !* number of tiles
     CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
     YRECFM='PATCH_NUMBER'
     CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP,HDIR='-')
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
     YRECFM='GLACIER'
     CALL READ_SURF(HFILETYPE,YRECFM,GGLACIER,IRESP,HDIR='-')
     ALLOCATE(PFIELD(INI,1,IPATCH))
     PFIELD(:,:,:) = 0.0     
     IF(GGLACIER)THEN
       YRECFM = 'ICE_STO'
       CALL MAKE_CHOICE_ARRAY(HFILETYPE, IPATCH, GDIM, YRECFM, PFIELD(:,1,:),HDIR='E')
     ENDIF
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
     DO JP=1,SIZE(PFIELD,3)
       WHERE (ZMASK(:)==0.) PFIELD(:,1,JP) = XUNDEF
     ENDDO     
!
  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_EXTERN: '//TRIM(HSURF)//" initialization not implemented !")
!
END SELECT
!
DEALLOCATE(ZMASK)
!
!---------------------------------------------------------------------------
!
!*      6.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_EXTERN
