!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SNOW_BUFFER (G, U, HPROGRAM,HSURF,KLUOUT,KLAYER,PFIELD)
!     #################################################################################
!
!!****  *PREP_SNOW_BUFFER* - prepares snow field from operational BUFFER
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005
!!------------------------------------------------------------------
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODE_SNOW3L
!
USE MODE_READ_BUFFER
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_BUFFER_GRID
USE MODI_SNOW_T_WLIQ_TO_HEAT
#ifdef SFX_ARO
USE MODI_OI_HOR_EXTRAPOL_SURF
#endif
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
USE MODI_ABOR1_SFX
!
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_PREP_ISBA,      ONLY : LEXTRAP_SN
USE MODD_PREP_SNOW,      ONLY : XGRID_SNOW
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_GRID_BUFFER,    ONLY : NNI
USE MODD_SNOW_PAR,       ONLY : XANSMIN, XANSMAX, XRHOSMAX
USE MODD_CSTS,           ONLY : XTT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),   INTENT(IN)   :: HPROGRAM  ! program calling surf. schemes
CHARACTER(LEN=10),  INTENT(IN)   :: HSURF     ! type of field
INTEGER,            INTENT(IN)   :: KLUOUT    ! logical unit of output listing
INTEGER,            INTENT(IN)  :: KLAYER        ! Number of layer of output snow scheme
REAL,DIMENSION(:,:,:), POINTER   :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                 :: TZTIME_BUFFER    ! current date and time
CHARACTER(LEN=6)                 :: YINMODEL       ! model from which GRIB file originates
REAL, DIMENSION(:),   POINTER    :: ZFIELD1D       ! field read
REAL, DIMENSION(:),   POINTER    :: ZHEAT          ! heat in snow
REAL, DIMENSION(:),   POINTER    :: ZRHO           ! density of snow
REAL,DIMENSION(:),POINTER        :: ZLSM           ! Land/sea mask
INTEGER                          :: JVEGTYPE       ! loop counter on vegtypes
INTEGER                          :: JLAYER         ! loop on snow fine grid
REAL,ALLOCATABLE,DIMENSION(:)    :: ZFIELD_EP
REAL,ALLOCATABLE,DIMENSION(:)    :: ZFIELD_EP_IN
REAL,ALLOCATABLE,DIMENSION(:)    :: ZLSM_NATURE
LOGICAL,ALLOCATABLE,DIMENSION(:) :: OINTERP
INTEGER                          :: II
INTEGER,PARAMETER                :: IDIM2=10
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_BUFFER',0,ZHOOK_HANDLE)
CALL PREP_BUFFER_GRID(KLUOUT,YINMODEL,TZTIME_BUFFER)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of the physical field for urban areas
!              ---------------------------------------------
!
IF (HSURF(7:8)=='RO') THEN
  !
  SELECT CASE(HSURF(1:3))
    CASE('DEP')
      ALLOCATE(PFIELD(NNI,KLAYER,1))
    CASE('ALB','WWW')
      ALLOCATE(PFIELD(NNI,1,1))
    CASE('HEA','RHO')
      ALLOCATE(PFIELD(NNI,SIZE(XGRID_SNOW),1))
  END SELECT
  !
  PFIELD(:,:,:) = 0.
!
!-------------------------------------------------------------------------------------
!
!*      3.     Reading of the physical field for vegetated areas
!              -------------------------------------------------
!
ELSE
!
  SELECT CASE(HSURF(1:3))
!
!*      3.1    Total snow content (kg/m2)
!
  CASE('WWW')
     CALL READ_BUFFER_SNOW_VEG(KLUOUT,YINMODEL,ZFIELD1D)
     IF ( LEXTRAP_SN ) THEN
       IF ( SIZE(U%NR_NATURE) /= U%NSIZE_NATURE ) THEN
         CALL ABOR1_SFX('ABORT: PREP_ISBA_BUFFER - DIFFERENT SIZES')
       ELSE
         ! Allocate working arrays
         ALLOCATE(ZFIELD_EP(U%NSIZE_NATURE))
         ALLOCATE(ZFIELD_EP_IN(U%NSIZE_NATURE))
         ALLOCATE(OINTERP(U%NSIZE_NATURE))
         ALLOCATE(ZLSM_NATURE(U%NSIZE_NATURE))

         ! Read LSM
         CALL READ_BUFFER_LAND_MASK(KLUOUT,YINMODEL,ZLSM)

         ! Pack nature points to reduce dimension to nsize_nature
         CALL PACK_SAME_RANK(U%NR_NATURE,ZLSM,ZLSM_NATURE)

         ! Do extrapolation
         WRITE(KLUOUT,*) 'Extrapolating WWW from nearest land point in points where LSM < 0.5.'

         ! Pack nature points to reduce dimension
         CALL PACK_SAME_RANK(U%NR_NATURE,ZFIELD1D(:),ZFIELD_EP(:))

         ! Set values to be extrapolated
         OINTERP=.FALSE.
         DO II=1,U%NSIZE_NATURE
           IF ( ZLSM_NATURE(II) < 0.5 ) THEN
             OINTERP(II)  = .TRUE.
             ZFIELD_EP(II) = XUNDEF
           ENDIF
         ENDDO

         ZFIELD_EP_IN(:) = ZFIELD_EP
#ifdef SFX_ARO
         CALL OI_HOR_EXTRAPOL_SURF(U%NSIZE_NATURE,G%XLAT,G%XLON,ZFIELD_EP_IN(:),G%XLAT,G%XLON,ZFIELD_EP(:),OINTERP,NDIM2=IDIM2)
#endif

         ! Unpack to full rank
         CALL UNPACK_SAME_RANK(U%NR_NATURE,ZFIELD_EP(:),ZFIELD1D(:))
         DEALLOCATE(ZFIELD_EP)
         DEALLOCATE(ZFIELD_EP_IN)
         DEALLOCATE(ZLSM_NATURE)
         DEALLOCATE(OINTERP)
       ENDIF
     ENDIF
     !
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,1,1)=ZFIELD1D(:)
     DEALLOCATE(ZFIELD1D)
!
!
!*      3.2    Total snow depth (m) to snow layers ticknesses (m)
!
  CASE('DEP')
     CALL READ_BUFFER_SNOW_VEG_DEPTH(KLUOUT,YINMODEL,ZFIELD1D)
     IF ( LEXTRAP_SN ) THEN
       IF ( SIZE(U%NR_NATURE) /= U%NSIZE_NATURE ) THEN
         CALL ABOR1_SFX('ABORT: PREP_ISBA_BUFFER - DIFFERENT SIZES')
       ELSE
         ! Allocate working arrays
         ALLOCATE(ZFIELD_EP(U%NSIZE_NATURE))
         ALLOCATE(ZFIELD_EP_IN(U%NSIZE_NATURE))
         ALLOCATE(OINTERP(U%NSIZE_NATURE))
         ALLOCATE(ZLSM_NATURE(U%NSIZE_NATURE))

         ! Read LSM
         CALL READ_BUFFER_LAND_MASK(KLUOUT,YINMODEL,ZLSM)

         ! Pack nature points to reduce dimension to nsize_nature
         CALL PACK_SAME_RANK(U%NR_NATURE,ZLSM,ZLSM_NATURE)

         ! Do extrapolation
         WRITE(KLUOUT,*) 'Extrapolating DEP from nearest land point in points where LSM < 0.5.'

         ! Pack nature points to reduce dimension
         CALL PACK_SAME_RANK(U%NR_NATURE,ZFIELD1D(:),ZFIELD_EP(:))
         ! Set values to be extrapolated
         OINTERP=.FALSE.
         DO II=1,U%NSIZE_NATURE
           IF ( ZLSM_NATURE(II) < 0.5 ) THEN
             OINTERP(II)  = .TRUE.
             ZFIELD_EP(II) = XUNDEF
           ENDIF
         ENDDO

         ZFIELD_EP_IN(:) = ZFIELD_EP
#ifdef SFX_ARO
         CALL OI_HOR_EXTRAPOL_SURF(U%NSIZE_NATURE,G%XLAT,G%XLON,ZFIELD_EP_IN(:),G%XLAT,G%XLON,ZFIELD_EP(:),OINTERP,NDIM2=IDIM2)
#endif

         ! Unpack to full rank
         CALL UNPACK_SAME_RANK(U%NR_NATURE,ZFIELD_EP(:),ZFIELD1D(:))
         DEALLOCATE(ZFIELD_EP)
         DEALLOCATE(ZFIELD_EP_IN)
         DEALLOCATE(ZLSM_NATURE)
         DEALLOCATE(OINTERP)
       ENDIF
     ENDIF

     !
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,1,1) = ZFIELD1D(:)
     DEALLOCATE(ZFIELD1D)
!
!
!*      3.3    Profile of heat in the snow
!
  CASE('HEA')
     !* read temperature
     CALL READ_BUFFER_TS(KLUOUT,YINMODEL,ZFIELD1D)
     WHERE (ZFIELD1D/=XUNDEF) ZFIELD1D(:) = MIN(ZFIELD1D,XTT)
     !* assumes no liquid water in the snow
     ALLOCATE(ZHEAT(SIZE(ZFIELD1D)))
     ALLOCATE(ZRHO (SIZE(ZFIELD1D)))
     ZRHO(:) = XRHOSMAX
     !
     CALL SNOW_T_WLIQ_TO_HEAT(ZHEAT,ZRHO,ZFIELD1D)
     !
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,1,1)=ZHEAT(:)
     DEALLOCATE(ZFIELD1D)
     DEALLOCATE(ZHEAT   )
     DEALLOCATE(ZRHO    )
!
!*      3.4    Albedo
!
  CASE('ALB')
    ALLOCATE(PFIELD(NNI,1,1))
    PFIELD = 0.5 * ( XANSMIN + XANSMAX )
!
!*      3.5    Density
!
  CASE('RHO')
    ALLOCATE(PFIELD(NNI,1,1))
    PFIELD = XRHOSMAX

  END SELECT
  !
END IF
!
!-------------------------------------------------------------------------------------
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='BUFFER'
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_BUFFER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SNOW_BUFFER
