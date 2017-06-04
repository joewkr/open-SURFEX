!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_SURF_VAR_n (FM, IM, SM, TM, WM, DGO, D, UG, U, USS,        &
                                 HPROGRAM, KI, KS,PSEA, PWATER, PNATURE, PTOWN, &
                                 PT2M, PQ2M, PQS, PZ0, PZ0H, PZ0EFF, PZ0_SEA,   &
                                 PZ0_WATER, PZ0_NATURE, PZ0_TOWN, PZ0H_SEA,     &
                                 PZ0H_WATER, PZ0H_NATURE, PZ0H_TOWN, PQS_SEA,   &
                                 PQS_WATER, PQS_NATURE, PQS_TOWN, PPSNG, PPSNV, &
                                 PZS, PSERIES, PTWSNOW, PSSO_STDEV, PLON, PLAT, &
                                 PBARE, PLAI_TREE, PH_TREE                    )  
!     #######################################################################
!
!!****  *GET_SURF_VAR_n* - gets some surface fields on atmospheric grid
!!
!!    PURPOSE
!!    -------
!!
!!    This program returns some surface variables neede by the atmosphere
!!
!!**  METHOD
!!    ------
!!
!!    Several functions are called in order to initialize surface variables
!!    needed by the atmospheric model. These functions fill the required arrays by
!!    the diagnosed values computed during the run. Since all arrays are optional,
!!    this program may be called with any of the arguments described above.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2006
!       S. Riette   06/2010 PSSO_STDEV and PTWSNOW added
!       B. Decharme 09/2012 Argument added in GET_FLUX_n
!       B. Decharme 05/2013 Argument added in GET_FLUX_n for debug in ARP/AL/AR
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n, ONLY : FLAKE_MODEL_t, ISBA_MODEL_t, SEAFLUX_MODEL_t, &
                          TEB_MODEL_t, WATFLUX_MODEL_t
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODI_GET_LUOUT
USE MODI_GET_FLUX_n
USE MODI_GET_FRAC_n
USE MODI_GET_Z0_n
USE MODI_GET_QS_n
USE MODI_GET_VAR_SEA_n
USE MODI_GET_VAR_WATER_n
USE MODI_GET_VAR_NATURE_n
USE MODI_GET_VAR_TOWN_n
USE MODI_GET_ZS_n
USE MODI_GET_SERIES_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_GET_SSO_STDEV_n
USE MODI_GET_1D_MASK
USE MODI_GET_COORD_n
USE MODI_GET_VEG_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(FLAKE_MODEL_t), INTENT(INOUT) :: FM
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(SEAFLUX_MODEL_t), INTENT(INOUT) :: SM
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(WATFLUX_MODEL_t), INTENT(INOUT) :: WM
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),   INTENT(IN)            :: HPROGRAM    
INTEGER,            INTENT(IN)            :: KI         ! number of points
INTEGER,            INTENT(IN)            :: KS         ! number of points
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PSEA       ! sea fraction
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PWATER     ! water fraction
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PNATURE    ! nature fraction
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PTOWN      ! town fraction
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PT2M       ! Air temperature at 2 meters         (K)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQ2M       ! Air humidity at 2 meters            (kg/kg)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0        ! surface roughness length            (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H       ! surface roughness length for heat   (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0EFF     ! effective roughness length for heat (m)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0_SEA    ! surface roughness length over sea   (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0_WATER  ! surface roughness length over water (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0_NATURE ! surface roughness length over nature(m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0_TOWN   ! surface roughness length over town  (m)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H_SEA    ! surface roughness length for heat over sea   (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H_WATER  ! surface roughness length for heat over water (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H_NATURE ! surface roughness length for heat over nature(m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H_TOWN   ! surface roughness length for heat over town  (m)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS_SEA    ! surface humidity over sea           (kg/kg)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS_WATER  ! surface humidity over water         (kg/kg)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS_NATURE ! surface humidity over nature        (kg/kg)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS_TOWN   ! surface humidity over town          (kg/kg)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PPSNG      ! snow fraction over ground           (-)        
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PPSNV      ! snow fraction over vegetation       (-)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZS        ! surface orography                   (m)    
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSERIES  ! any surface field for which 
!                                                       ! mesoNH series are required
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PTWSNOW    ! Snow total reservoir
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PSSO_STDEV ! S.S.O. standard deviation           (m)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLON       ! longitude
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLAT       ! latitude
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PBARE      ! bare soil fraction on grid mesh     (-)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLAI_TREE       ! Leaf Area Index    on grid mesh     (-)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PH_TREE        ! Height of trees    on grid mesh     (-)
!
!-------------------------------------------------------------------------------
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI)    :: ZFIELD1, ZFIELD2, ZFIELD3, ZFIELD4, ZFIELD5, ZFIELD6
REAL, DIMENSION(KI)    :: ZFIELD7, ZFIELD8
REAL, DIMENSION(KI,KS) :: ZSERIES
INTEGER, DIMENSION(KI) :: IMASK
!
INTEGER :: KI_SEA    ! dimension of sea tile
INTEGER :: KI_WATER  ! dimension of water tile
INTEGER :: KI_NATURE ! dimension of nature tile
INTEGER :: KI_TOWN   ! dimension of town tile
!
INTEGER                            :: JI           ! loop index over tiles
INTEGER                            :: ILUOUT       ! unit number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*   0. Logical unit for writing out
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_VAR_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*   1. Fraction of each tile
!
IF (PRESENT(PSEA) .OR. PRESENT(PWATER) .OR. PRESENT(PNATURE) .OR. PRESENT(PTOWN)) THEN
   !
   CALL GET_FRAC_n(U, HPROGRAM, KI, ZFIELD1, ZFIELD2, ZFIELD3, ZFIELD4)
   !
   IF (PRESENT(PSEA)   ) PSEA    = ZFIELD1
   IF (PRESENT(PWATER) ) PWATER  = ZFIELD2
   IF (PRESENT(PNATURE)) PNATURE = ZFIELD3
   IF (PRESENT(PTOWN)  ) PTOWN   = ZFIELD4
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*   2. Parameters at 2 meters
!
IF ( PRESENT(PT2M) .OR. PRESENT(PQ2M) ) THEN
   !
   CALL GET_FLUX_n(DGO, D, HPROGRAM, KI, &
                   ZFIELD1, ZFIELD1, ZFIELD1, ZFIELD1, ZFIELD1, ZFIELD2, &
                   ZFIELD3, ZFIELD4, ZFIELD4, ZFIELD4, ZFIELD4, ZFIELD4, &
                   ZFIELD4, ZFIELD4, ZFIELD4                             )
   !
   IF (PRESENT(PT2M)   ) PT2M    = ZFIELD2
   IF (PRESENT(PQ2M)   ) PQ2M    = ZFIELD3
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*   3. Roughness lengths
!
IF ( PRESENT(PZ0) .OR. PRESENT(PZ0H) ) THEN
   !
   CALL GET_Z0_n(DGO, D, HPROGRAM, KI, ZFIELD1, ZFIELD2)
   !
   IF (PRESENT(PZ0)    ) PZ0    = ZFIELD1
   IF (PRESENT(PZ0H)   ) PZ0H   = ZFIELD2
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*   3. Specific humidity
!
IF ( PRESENT(PQS) ) THEN
   !
   CALL GET_QS_n(DGO, D, HPROGRAM, KI, PQS)
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*   4. Surface humidity for each tile (qs is not aggregated)
!
IF ( PRESENT(PQS_SEA) .OR. PRESENT(PZ0_SEA) .OR. PRESENT(PZ0H_SEA) ) THEN
   !
   ! Get parameters over sea tile
   !
   IF ( .NOT.PRESENT(PSEA) ) THEN
      !
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PSEA MISSING')
      !
   ENDIF
   !
   KI_SEA  = COUNT(PSEA    (:) > 0.0)
   !
   IMASK(:)=0
   CALL GET_1D_MASK(KI_SEA, KI, PSEA, IMASK(1:KI_SEA))
   !
   CALL GET_VAR_SEA_n(SM%SD%O, SM%SD%D, &
                      HPROGRAM, KI_SEA, ZFIELD1(1:KI_SEA), ZFIELD2(1:KI_SEA), ZFIELD3(1:KI_SEA))
   !
   IF(PRESENT(PQS_SEA))THEN
      PQS_SEA    (:) = XUNDEF
      DO JI = 1, KI_SEA
         PQS_SEA(IMASK(JI))  = ZFIELD1(JI)
      END DO
   ENDIF
   !   
   IF(PRESENT(PZ0_SEA))THEN
      PZ0_SEA    (:) = XUNDEF
      DO JI = 1, KI_SEA
         PZ0_SEA(IMASK(JI))  = ZFIELD2(JI)
      END DO
   ENDIF
   !
   IF(PRESENT(PZ0H_SEA))THEN
      PZ0H_SEA   (:) = XUNDEF
      DO JI = 1, KI_SEA
         PZ0H_SEA(IMASK(JI)) = ZFIELD3(JI)
      END DO
   ENDIF
   !
ENDIF
   !
   !-------------------------------------------------------------------------------
   !
IF ( PRESENT(PQS_WATER) .OR. PRESENT(PZ0_WATER) .OR. PRESENT(PZ0H_WATER) ) THEN
   !
   ! Get parameters over water tile
   !
   IF ( .NOT.PRESENT(PWATER) ) THEN
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PWATER MISSING')
   ENDIF
   !
   KI_WATER  = COUNT(PWATER  (:) > 0.0)
   !
   IMASK(:)=0
   CALL GET_1D_MASK(KI_WATER, KI, PWATER, IMASK(1:KI_WATER))
   !
   CALL GET_VAR_WATER_n(FM%DFO, FM%DF, WM%DWO, WM%DW, &
                        HPROGRAM, KI_WATER, U%CWATER, ZFIELD1(1:KI_WATER), &
                        ZFIELD2(1:KI_WATER), ZFIELD3(1:KI_WATER))
   !
   IF(PRESENT(PQS_WATER))THEN
      PQS_WATER    (:) = XUNDEF
      DO JI = 1, KI_WATER
         PQS_WATER(IMASK(JI))  = ZFIELD1(JI)
      END DO
   ENDIF
   !   
   IF(PRESENT(PZ0_WATER))THEN
      PZ0_WATER    (:) = XUNDEF
      DO JI = 1, KI_WATER
         PZ0_WATER(IMASK(JI))  = ZFIELD2(JI)
      END DO
   ENDIF
   !
   IF(PRESENT(PZ0H_WATER))THEN
      PZ0H_WATER   (:) = XUNDEF
      DO JI = 1, KI_WATER
         PZ0H_WATER(IMASK(JI)) = ZFIELD3(JI)
      END DO
   ENDIF
   !
ENDIF
   !
   !-------------------------------------------------------------------------------
   !
IF ( PRESENT(PQS_NATURE) .OR. PRESENT(PPSNG) .OR. PRESENT(PPSNV) .OR.  PRESENT(PZ0EFF).OR. &
     PRESENT(PTWSNOW) .OR. PRESENT(PBARE) .OR. PRESENT(PLAI_TREE) .OR. PRESENT(PH_TREE) ) THEN
   !
   ! Get parameters over nature tile
   !
   !
   IF ( .NOT.PRESENT(PNATURE) ) THEN
      !
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PNATURE MISSING')
      !
   ENDIF
   !   
   KI_NATURE = COUNT(PNATURE (:) > 0.0)
   !
   IMASK(:)=0
   CALL GET_1D_MASK(KI_NATURE, KI, PNATURE, IMASK(1:KI_NATURE))
   !
   IF (KI_NATURE>0) THEN
     CALL GET_VAR_NATURE_n(IM%S, IM%ID%O, IM%ID%D, IM%ID%DM, HPROGRAM, KI_NATURE, &
                           ZFIELD1(1:KI_NATURE), ZFIELD2(1:KI_NATURE), ZFIELD3(1:KI_NATURE), &
                           ZFIELD4(1:KI_NATURE), ZFIELD5(1:KI_NATURE), ZFIELD6(1:KI_NATURE), &
                           ZFIELD7(1:KI_NATURE), ZFIELD8(1:KI_NATURE))
   ENDIF
   !
   IF(PRESENT(PQS_NATURE))THEN
     PQS_NATURE    (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PQS_NATURE(IMASK(JI))  = ZFIELD1(JI)
     END DO
   ENDIF
   !   
   IF(PRESENT(PZ0_NATURE))THEN
     PZ0_NATURE    (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PZ0_NATURE(IMASK(JI))  = ZFIELD5(JI)
     END DO
   ENDIF
   !
   IF(PRESENT(PZ0H_NATURE))THEN
     PZ0H_NATURE   (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PZ0H_NATURE(IMASK(JI)) = ZFIELD6(JI)
     END DO
   ENDIF
   !  
   IF (PRESENT(PPSNG)) THEN
     PPSNG      (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PPSNG     (IMASK(JI)) = ZFIELD2(JI)
     END DO
   ENDIF
   !
   IF (PRESENT(PPSNV)) THEN
     PPSNV      (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PPSNV     (IMASK(JI)) = ZFIELD3(JI)
     END DO
   ENDIF
   !
   IF ( PRESENT(PZ0EFF) ) THEN
     PZ0EFF     (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PZ0EFF    (IMASK(JI)) = ZFIELD4(JI)
     END DO
   ENDIF
   !
   IF(PRESENT(PTWSNOW)) THEN
     PTWSNOW    (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PTWSNOW   (IMASK(JI)) = ZFIELD7(JI)
     ENDDO
   ENDIF
   !
   !* bare soil fraction
   !
   IF(PRESENT(PBARE)) THEN
     PBARE    (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PBARE   (IMASK(JI)) = ZFIELD8(JI)
     ENDDO
     PBARE(:) = PBARE(:) * U%XNATURE(:) ! averages bare soil fraction on whole grid mesh
   ENDIF
   !
   !*   LAI and height of trees
   !
   IF (PRESENT(PLAI_TREE) .OR. PRESENT(PH_TREE) ) THEN
     !
     CALL GET_VEG_n(HPROGRAM, KI_NATURE, U, IM%O, IM%S, IM%NP, IM%NPE, ZFIELD1(1:KI_NATURE), ZFIELD2(1:KI_NATURE))
     !
     IF (PRESENT(PLAI_TREE)) THEN
       PLAI_TREE(:) = XUNDEF
       DO JI = 1, KI_NATURE
         PLAI_TREE   (IMASK(JI)) = ZFIELD1(JI)
       ENDDO
       PLAI_TREE(:) = PLAI_TREE(:) * U%XNATURE(:) ! averages tree LAI on whole grid mesh
     END IF
     !
     IF (PRESENT(PH_TREE)) THEN
       PH_TREE(:) = 0.
       DO JI = 1, KI_NATURE
         PH_TREE   (IMASK(JI)) = ZFIELD2(JI)
       ENDDO
     END IF
     !
   END IF   
   !   
ENDIF
   !
   !-------------------------------------------------------------------------------
   !
IF ( PRESENT(PQS_TOWN) .OR. PRESENT(PZ0_TOWN) .OR. PRESENT(PZ0H_TOWN) ) THEN
   !
   ! Get parameters over town tile
   !
   IF ( .NOT.PRESENT(PTOWN) ) THEN
      !
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PTOWN MISSING')
      !
   ENDIF
   !
   KI_TOWN   = COUNT(PTOWN   (:) > 0.0)
   !
   IMASK(:)=0
   CALL GET_1D_MASK(KI_TOWN, KI, PTOWN, IMASK(1:KI_TOWN))
   !
   CALL GET_VAR_TOWN_n(TM%TD%O, TM%TD%D, HPROGRAM, KI_TOWN, &
                       ZFIELD1(1:KI_TOWN), ZFIELD2(1:KI_TOWN), ZFIELD3(1:KI_TOWN))
   !
   IF(PRESENT(PQS_TOWN))THEN
      PQS_TOWN    (:) = XUNDEF
      DO JI = 1, KI_TOWN
         PQS_TOWN(IMASK(JI))  = ZFIELD1(JI)
      END DO
   ENDIF
   !   
   IF(PRESENT(PZ0_TOWN))THEN
      PZ0_TOWN    (:) = XUNDEF
      DO JI = 1, KI_TOWN
         PZ0_TOWN(IMASK(JI))  = ZFIELD2(JI)
      END DO
   ENDIF
   !
   IF(PRESENT(PZ0H_TOWN))THEN
      PZ0H_TOWN   (:) = XUNDEF
      DO JI = 1, KI_TOWN
         PZ0H_TOWN(IMASK(JI)) = ZFIELD3(JI)
      END DO
   ENDIF
   !
END IF
!
!*   5. Orography
!
IF (PRESENT(PZS)) THEN
   !
   CALL GET_ZS_n(U, HPROGRAM, KI, ZFIELD1)
   !
   PZS = ZFIELD1 
   !
END IF
!
!*   6. Series
!
IF (PRESENT(PSERIES)) THEN
   !
   IF ( .NOT.PRESENT(PWATER) ) THEN
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PWATER REQUIRED FOR WATER SERIES')
   ENDIF        
   !
   IF ( COUNT(PWATER  (:) > 0.0) > 0.0 ) THEN
     !   
     CALL GET_SERIES_n(FM%F, HPROGRAM, KI, KS, ZSERIES)
     !
     PSERIES = ZSERIES
     !
   ELSE
     PSERIES = XUNDEF
   ENDIF
   !
END IF
!
!*   7. Subgrid orography standard deviation
!
IF (PRESENT(PSSO_STDEV)) THEN
   !
   CALL GET_SSO_STDEV_n(USS, 'ASCII ', KI, ZFIELD1)
   !
   PSSO_STDEV = ZFIELD1
   !
END IF
!
!*   8. Longitude et Latitude
!
IF (PRESENT(PLON).OR.PRESENT(PLAT)) THEN
   !
   CALL GET_COORD_n(UG, HPROGRAM, KI, ZFIELD1, ZFIELD2)
   !
   IF (PRESENT(PLON)   ) PLON    = ZFIELD1
   IF (PRESENT(PLAT)   ) PLAT    = ZFIELD2
   !
END IF
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_VAR_N',1,ZHOOK_HANDLE)
!==============================================================================
!
END SUBROUTINE GET_SURF_VAR_n
