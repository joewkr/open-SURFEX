!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_BUFFER (G, U, HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_ISBA_BUFFER* - initializes ISBA fields from operational BUFFER
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
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODE_READ_BUFFER
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_BUFFER_GRID
USE MODI_INTERP_GRID_NAT
!
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_PREP_ISBA,      ONLY : XGRID_SOIL, XWR_DEF
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_GRID_BUFFER,    ONLY : NNI
USE MODN_PREP_ISBA,      ONLY : LEXTRAP_TG,LEXTRAP_WG,LEXTRAP_WGI
#ifdef SFX_ARO
USE MODI_OI_HOR_EXTRAPOL_SURF
#endif
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
USE MODI_ABOR1_SFX
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
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_BUF    ! current date and time
CHARACTER(LEN=6)                 :: YINMODEL       ! model from which buffer originates
REAL, DIMENSION(:,:), POINTER   :: ZFIELD         ! field read
REAL, DIMENSION(:),   POINTER   :: ZFIELD1D       ! field read
REAL, DIMENSION(:,:), POINTER   :: ZD             ! depth of field in the soil
REAL,DIMENSION(:),POINTER        :: ZLSM
REAL,DIMENSION(:),POINTER        :: ZALT
REAL,ALLOCATABLE,DIMENSION(:)    :: ZFIELD_EP
REAL,ALLOCATABLE,DIMENSION(:)    :: ZFIELD_EP_IN
REAL,ALLOCATABLE,DIMENSION(:)    :: ZLSM_NATURE
REAL,ALLOCATABLE,DIMENSION(:)    :: ZALT_NATURE
LOGICAL,ALLOCATABLE,DIMENSION(:) :: OINTERP
INTEGER                          :: ILAYER,II
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_BUFFER',0,ZHOOK_HANDLE)
 CALL PREP_BUFFER_GRID(KLUOUT,YINMODEL,TZTIME_BUF)

!
!*      2.     Reading of field
!              ----------------
!
!*      3.     Transformation into physical quantity to be interpolated
!              --------------------------------------------------------
!
SELECT CASE(HSURF)
!
!*      3.1    Profile of temperature in the soil
!
  CASE('TG    ')
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
     CASE('ALADIN')
        CALL READ_BUFFER_TG(KLUOUT,YINMODEL,ZFIELD,ZD)
        IF ( LEXTRAP_TG ) THEN
          IF ( SIZE(U%NR_NATURE) /= U%NSIZE_NATURE ) THEN
            CALL ABOR1_SFX('ABORT: PREP_ISBA_BUFFER - DIFFERENT SIZES')
          ELSE
            ! Allocate working arrays
            ALLOCATE(ZFIELD_EP(U%NSIZE_NATURE))
            ALLOCATE(ZFIELD_EP_IN(U%NSIZE_NATURE))
            ALLOCATE(OINTERP(U%NSIZE_NATURE))
            ALLOCATE(ZLSM_NATURE(U%NSIZE_NATURE))
            ALLOCATE(ZALT_NATURE(U%NSIZE_NATURE))

            ! Read LSM and ZS
            CALL READ_BUFFER_LAND_MASK(KLUOUT,YINMODEL,ZLSM)
            CALL READ_BUFFER_ZS(KLUOUT,YINMODEL,ZALT)

            ! Pack nature points to reduce dimension to nsize_nature
            CALL PACK_SAME_RANK(U%NR_NATURE,ZLSM,ZLSM_NATURE)
            CALL PACK_SAME_RANK(U%NR_NATURE,ZALT,ZALT_NATURE)

            ! Do extrapolations in all layers
            DO ILAYER=1,SIZE(ZFIELD,2)
              WRITE(KLUOUT,*) 'Extrapolating TG from nearest land point in points where LSM < 0.5. LAYER:',ILAYER

              ! Pack nature points to reduce dimension
              CALL PACK_SAME_RANK(U%NR_NATURE,ZFIELD(:,ILAYER),ZFIELD_EP(:))
              ! Set values to be extrapolated
              OINTERP=.FALSE.
              DO II=1,U%NSIZE_NATURE
                IF ( ZLSM_NATURE(II) < 0.5 ) THEN
                  OINTERP(II)   = .TRUE.
                  ZFIELD_EP(II) = XUNDEF
                ENDIF
              ENDDO

              ZFIELD_EP_IN(:) = ZFIELD_EP(:)
#ifdef SFX_ARO
              CALL OI_HOR_EXTRAPOL_SURF(U%NSIZE_NATURE,G%XLAT,G%XLON,ZFIELD_EP_IN(:), &
                                        G%XLAT,G%XLON,ZFIELD_EP(:),OINTERP,PZS=ZALT,NDIM2=10)
#endif

              ! Unpack to full rank
              CALL UNPACK_SAME_RANK(U%NR_NATURE,ZFIELD_EP(:),ZFIELD(:,ILAYER))
            ENDDO
            DEALLOCATE(ZFIELD_EP)
            DEALLOCATE(ZFIELD_EP_IN)
            DEALLOCATE(ZLSM_NATURE)
            DEALLOCATE(ZALT_NATURE)
            DEALLOCATE(OINTERP)
          ENDIF
        ENDIF 
     END SELECT
     
     CALL SOIL_PROFILE_BUFFER

  CASE('WG    ')
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
     CASE('ARPEGE','ALADIN','MOCAGE')
        CALL READ_BUFFER_WG(KLUOUT,YINMODEL,ZFIELD,ZD)
        IF ( LEXTRAP_WG ) THEN
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
   
            ! Do extrapolations in all layers
            DO ILAYER=1,SIZE(ZFIELD,2)
              WRITE(KLUOUT,*) 'Extrapolating WG from nearest land point in points where LSM < 0.5. LAYER:',ILAYER

              ! Pack nature points to reduce dimension
              CALL PACK_SAME_RANK(U%NR_NATURE,ZFIELD(:,ILAYER),ZFIELD_EP(:))
              ! Set values to be extrapolated
              OINTERP=.FALSE.
              
              DO II=1,U%NSIZE_NATURE
                IF ( ZLSM_NATURE(II) < 0.5 ) THEN
                  OINTERP(II)   = .TRUE.
                  ZFIELD_EP(II) = XUNDEF
                ENDIF
              ENDDO

              ZFIELD_EP_IN(:) = ZFIELD_EP
#ifdef SFX_ARO
              CALL OI_HOR_EXTRAPOL_SURF(U%NSIZE_NATURE,G%XLAT,G%XLON,ZFIELD_EP_IN(:), &
                                        G%XLAT,G%XLON,ZFIELD_EP(:),OINTERP,NDIM2=10)
#endif

              ! Unpack to full rank
              CALL UNPACK_SAME_RANK(U%NR_NATURE,ZFIELD_EP(:),ZFIELD(:,ILAYER))
            ENDDO
            DEALLOCATE(ZFIELD_EP)
            DEALLOCATE(ZFIELD_EP_IN)
            DEALLOCATE(ZLSM_NATURE)
            DEALLOCATE(OINTERP)
          ENDIF
        ENDIF

     END SELECT
     CALL SOIL_PROFILE_BUFFER


!*      3.3    Profile of soil ice content

  CASE('WGI   ')    
     !* reading of the profile and its depth definition
     SELECT CASE(YINMODEL)
       CASE('ALADIN')
         CALL READ_BUFFER_WGI(KLUOUT,YINMODEL,ZFIELD,ZD)
         IF ( LEXTRAP_WGI ) THEN

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

            ! Do extrapolations in all layers
            DO ILAYER=1,SIZE(ZFIELD,2)
              WRITE(KLUOUT,*) 'Extrapolating WGI from nearest land point in points where LSM < 0.5. LAYER:',ILAYER
        
              ! Pack nature points to reduce dimension
              CALL PACK_SAME_RANK(U%NR_NATURE,ZFIELD(:,ILAYER),ZFIELD_EP(:))
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
              CALL OI_HOR_EXTRAPOL_SURF(U%NSIZE_NATURE,G%XLAT,G%XLON,ZFIELD_EP_IN(:), &
                                        G%XLAT,G%XLON,ZFIELD_EP(:),OINTERP,NDIM2=10)
#endif

              ! Unpack to full rank
              CALL UNPACK_SAME_RANK(U%NR_NATURE,ZFIELD_EP(:),ZFIELD(:,ILAYER))
            ENDDO
            DEALLOCATE(ZFIELD_EP)
            DEALLOCATE(ZFIELD_EP_IN)
            DEALLOCATE(ZLSM_NATURE)
            DEALLOCATE(OINTERP)
          ENDIF
        ENDIF

     END SELECT
     CALL SOIL_PROFILE_BUFFER
!
!*      3.4    Water content intercepted on leaves, LAI
!
  CASE('WR     ')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = XWR_DEF
!
  CASE('LAI    ')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = XUNDEF
!
!
!*      3.5    Other fields
!
  CASE('ZS     ')
     CALL READ_BUFFER_ZS(KLUOUT,YINMODEL,ZFIELD1D)
     ALLOCATE(PFIELD(SIZE(ZFIELD1D,1),1,1))
     PFIELD(:,1,1)=ZFIELD1D(:)
     DEALLOCATE(ZFIELD1D)
!
  CASE('ICE_STO')
     ALLOCATE(PFIELD(NNI,1,1))
     PFIELD(:,:,:) = 0.0
!
  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_BUFFER: '//TRIM(HSURF)//" initialization not implemented !")     
!
END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='BUFFER'
!
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_BUFFER',1,ZHOOK_HANDLE)
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE SOIL_PROFILE_BUFFER
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZOUT   ! work array
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
     !
     !* interpolation on fine vertical grid
     IF (LHOOK) CALL DR_HOOK('SOIL_PROFILE_BUFFER',0,ZHOOK_HANDLE)
     ALLOCATE(ZOUT  (SIZE(ZFIELD,1),SIZE(XGRID_SOIL)))
     CALL INTERP_GRID_NAT(ZD,ZFIELD,XGRID_SOIL,ZOUT)
     !
     !* extends definition to all vegtypes.
     ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_SOIL),1))
     PFIELD(:,:,1)=ZOUT(:,:)
     !* end
     DEALLOCATE(ZOUT)
     DEALLOCATE(ZFIELD)
     DEALLOCATE(ZD)
IF (LHOOK) CALL DR_HOOK('SOIL_PROFILE_BUFFER',1,ZHOOK_HANDLE)

END SUBROUTINE SOIL_PROFILE_BUFFER
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_BUFFER
