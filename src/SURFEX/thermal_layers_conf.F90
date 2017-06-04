!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################################################################
      SUBROUTINE THERMAL_LAYERS_CONF(HTYPE,PD,PD_OUT,PHC,PHC_OUT,PTC,PTC_OUT)
!     ######################################################################
!
!!****  *THERMAL_LAYERS_CONF* 
!!
!!    PURPOSE
!!    -------
!     Adjust the thermal characteristics of the layers in road, wall, roof or
!     floor depending on the number of layers that the user wants to use during
!     the simulations.
!     Initial data are prescribed depending on user preference.
!     They have to be averaged on the layers use in the simulation
!  
!!
!!**  METHOD
!!    ------
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2012
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=5),     INTENT(IN)  :: HTYPE     ! type of surface
REAL, DIMENSION(:,:), INTENT(IN)  :: PD        ! input Layer Thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PD_OUT    ! output Layer Thickness
REAL, DIMENSION(:,:), INTENT(IN), OPTIONAL  :: PHC       ! input Heat Capacity
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PHC_OUT   ! output Heat Capacity
REAL, DIMENSION(:,:), INTENT(IN), OPTIONAL  :: PTC       ! input Thermal conductivity
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTC_OUT   ! output Thermal conductivity
!
!*       0.2   Declarations of local variables
!
REAL :: ZD_TOT    ! Total depth
REAL :: ZD_HALF   ! Depth of the half of the total surface
!                                           ! (excluding central layer in case
!                                           ! of odd number of layers)
REAL  :: ZD_MID    ! Thickness of the layer in the middle
!                                           ! in case of odd number of layers
REAL, DIMENSION(0:SIZE(PD    ,2))::ZD_IN  ! Depth from the surface 
!                                          ! to the layer bottom
REAL, DIMENSION(0:SIZE(PD_OUT,2))::ZD_OUT ! Depth from the surface 
!                                                      ! to the layer bottom
REAL, DIMENSION(SIZE(PD,2))     :: ZW, ZHC     ! 1/TC
REAL, DIMENSION(SIZE(PD_OUT,2)) :: ZW_OUT, ZHC_OUT ! 1/TC
INTEGER                        :: IIN       ! Number of layer in input data
INTEGER                        :: IOUT      ! Number of layer in output fields
INTEGER                        :: JIN, JI       ! Loop counter on input layers
INTEGER                        :: JOUT      ! Loop counter on output layers
!
REAL, PARAMETER                :: ZD_G1 = 0.001  ! uppermost soil layer 
!                                                ! thickness/depth       ( m)
!                                                ! Can not be too thin as 
!                                                ! then definition of soil
!                                                ! properties (i.e. phyiscal
!                                                ! representation of) and 
!                                                ! accuarcy of
!                                                ! numerical solution come
!                                                ! into question. If it is too
!                                                ! thick, then resolution of
!                                                ! diurnal cycle not as valid.
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('THERMAL_LAYER_CONF_1',0,ZHOOK_HANDLE)
!
IF (PRESENT(PHC_OUT).AND..NOT.PRESENT(PHC)) THEN
  CALL ABOR1_SFX("THERMAL_LAYERS_CONF:IF HC_OUT IS PRESENT, HC MUST BE PRESENT TOO.")
ELSEIF (PRESENT(PTC_OUT).AND..NOT.PRESENT(PTC)) THEN
  CALL ABOR1_SFX("THERMAL_LAYERS_CONF:IF TC_OUT IS PRESENT, TC MUST BE PRESENT TOO.")
ENDIF
!
IOUT= SIZE(PD_OUT,2)
!
IIN = SIZE(PD,2)
!
IF (LHOOK) CALL DR_HOOK('THERMAL_LAYER_CONF_1',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('THERMAL_LAYER_CONF_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JI,ZD_IN,ZD_TOT,ZD_OUT,ZD_HALF,ZD_MID,ZW,ZHC,ZW_OUT,ZHC_OUT)
DO JI=1,SIZE(PD_OUT,1)
  !
  ZD_IN(0) = 0.
  DO JIN=1,IIN
    ZD_IN(JIN) = ZD_IN(JIN-1) + PD(JI,JIN)
  END DO
  ZD_TOT = ZD_IN(IIN)
  !
  !* Depths for the computational grid
  !
  !* surface like road or floor (thin grid at the surface, coarse at the bottom)
  !
  ZD_OUT(0) = 0.
  !
  IF (HTYPE=='ROAD ' .OR. HTYPE=='FLOOR') THEN
    !
    CALL TEBGRID(ZD_TOT,ZD_OUT(1:),ZD_G1)
    !
    DO JOUT=1,IOUT
      PD_OUT(JI,JOUT) = ZD_OUT(JOUT) - ZD_OUT(JOUT-1) ! Depths => Thickness of layer
    END DO
    !
  ELSE
    !
    !* surface like roof or wall (thin grid on both sides, coarse in the middle)
    !
    IF (MOD(IOUT,2)==0) THEN   ! even number of output layers
      ZD_HALF = ZD_TOT / 2.
    ELSE                       ! odd  number of output layers
      ZD_MID = 2. * ZD_TOT / IOUT ! middle layer is arbitrarily fixed
      IF (IOUT==3) ZD_MID = MAX(ZD_MID,ZD_TOT-2.*ZD_G1) ! to impose layers equal
                                                                   ! to ZD_G1 on both sides
      ZD_HALF = (ZD_TOT-ZD_MID) / 2.
      PD_OUT (JI,IOUT/2+1) = ZD_MID
    END IF
    !
    CALL TEBGRID(ZD_HALF,ZD_OUT(1:IOUT/2),ZD_G1)
    !
    DO JOUT=1,IOUT
      !
      IF (JOUT<=IOUT/2) THEN
        PD_OUT(JI,JOUT) = ZD_OUT(JOUT) - ZD_OUT(JOUT-1) ! Depths => Thickness of layer
        PD_OUT(JI,IOUT+1-JOUT) = PD_OUT(JI,JOUT)
      ENDIF
      !
      !* recomputes Depths for further averagings
      IF (JOUT>1) ZD_OUT(JOUT) = ZD_OUT(JOUT-1) + PD_OUT(JI,JOUT)
      !
    END DO
    !
  ENDIF
  !
  IF (PD(JI,1)==XUNDEF)  PD_OUT(JI,1:IOUT) = XUNDEF
  !
  !-------------------------------------------------------------------------------
  !
  !* Averaging of the Heat Capacity and the Thermal conductivity
  !
  IF (PRESENT(PTC)) THEN
    ZW(:) =1./PTC(JI,:)
  ELSE
    ZW(:) =1.
  ENDIF
  IF (PRESENT(PHC)) THEN
    ZHC(:) = PHC(JI,:)
  ELSE
    ZHC(:) = 1.
  ENDIF
  !
  CALL AV_THERMAL_DATA(IOUT,PD(JI,:),ZD_IN(1:IIN),ZD_OUT(1:IOUT),ZHC,ZW,ZHC_OUT,ZW_OUT)
  !
  IF (PRESENT(PTC_OUT)) THEN
    PTC_OUT(JI,:)=XUNDEF
    WHERE (ZW_OUT(:)/=XUNDEF) PTC_OUT(JI,:)=1./ZW_OUT(:)
  ENDIF
  IF (PRESENT(PHC_OUT)) PHC_OUT(JI,:) = ZHC_OUT(:)
  !
ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('THERMAL_LAYER_CONF_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
SUBROUTINE AV_THERMAL_DATA(KOUT,PDD,PDD_IN,PDD_OUT,PF1,PF2,PF1_OUT,PF2_OUT)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KOUT
REAL, DIMENSION(:), INTENT(IN) :: PDD
REAL, DIMENSION(:), INTENT(IN) :: PDD_IN
REAL, DIMENSION(:), INTENT(IN) :: PDD_OUT
REAL, DIMENSION(:), INTENT(IN)  :: PF1
REAL, DIMENSION(:), INTENT(IN)  :: PF2
REAL, DIMENSION(:), INTENT(OUT) :: PF1_OUT
REAL, DIMENSION(:), INTENT(OUT) :: PF2_OUT
!
REAL    :: ZF1! ponderated field
REAL    :: ZF2! ponderated field
REAL    :: ZS ! sum of weights
REAL    :: ZC ! coefficient of ponderation
REAL    :: ZD_LIM ! limit of previous layer that has been treated
!
REAL    :: ZEPS=1.E-6
!
INTEGER :: JOUT, JIN
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!* total depth:
!
!
IF (PDD(1)==XUNDEF) THEN
  PF1_OUT(:) = XUNDEF
  PF2_OUT(:) = XUNDEF
  !
ELSE
  !
  ZF1 = 0.
  ZF2 = 0.
  ZS  = 0.
  JIN = 1
  JOUT= 1
  ZD_LIM = 0.
  !
  DO
    !
    IF (JOUT>KOUT) EXIT
    !
    IF (PDD_IN(JIN)< PDD_OUT(JOUT)-ZEPS) THEN
      !
      !ZC = PDD_IN(JIN) - MAX(PDD_IN(JIN-1),PDD_OUT(JOUT-1))
      ZC = PDD_IN(JIN) - ZD_LIM
      ZF1 = ZF1 + ZC * PF1(JIN)
      ZF2 = ZF2 + ZC * PF2(JIN)
      ZS = ZS + ZC
      ZD_LIM = PDD_IN(JIN)
      !
      JIN=JIN+1
      !
    ELSE
      !
      !ZC = PDD_OUT(JOUT) - MAX(PDD_IN(JIN-1),PDD_OUT(JOUT-1))
      ZC = PDD_OUT(JOUT) - ZD_LIM
      ZF1 = ZF1 + ZC * PF1(JIN)
      ZF2 = ZF2 + ZC * PF2(JIN)
      ZS = ZS + ZC
      PF1_OUT(JOUT) = ZF1/ZS
      PF2_OUT(JOUT) = ZF2/ZS
      ZD_LIM = PDD_OUT(JOUT)
      !
      JOUT = JOUT+1
      ZF1 = 0.
      ZF2 = 0.
      ZS  = 0.
      !
    END IF
    !
  END DO
  !
ENDIF
!
END SUBROUTINE AV_THERMAL_DATA
!
!     #########
      SUBROUTINE TEBGRID( PSOILDEPTH, PD_G, PD_G1 )

!     ##########################################################################
!
!!****  *TEBGRID*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the soil grid configuration using a simple
!     geometric relation for all sub-surface layers.
!     This algorithm assumes the total soil depth > 0 m
!         
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    Boone (2000)
!!    Boone et al. (2000)
!!    Habets et al. (2003)
!!      
!!    AUTHOR
!!    ------
!!	A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/04/03
!!      B. Decharme    12/10 uppermost soil layer set to 1cm
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
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
REAL,  INTENT(IN)  :: PSOILDEPTH  ! total soil depth            (m)
!                                   
REAL, DIMENSION(:), INTENT(OUT) :: PD_G        ! depth of base of soil layers (m)
REAL, OPTIONAL,       INTENT(IN)  :: PD_G1       ! depth of first layer
!
!
!*      0.2    declarations of local variables
!
INTEGER                           :: JJ, JI, JNLVL
!
!
REAL, PARAMETER                   :: ZGRIDFACTOR = 3.0 ! soil depth factor
!                                                      ! of increase with depth
!                                                      ! for all *sub-surface* 
!                                                      ! layers. Note, uppermost
!                                                      ! layer fixed by other
!                                                      ! constraints.          (-)
!
REAL                              :: ZD_G1 = 0.01      ! uppermost soil layer 
!                                                      ! thickness/depth       (m)
!                                                      ! Can not be too thin as 
!                                                      ! then definition of soil
!                                                      ! properties (i.e. phyiscal
!                                                      ! representation of) and 
!                                                      ! accuarcy of
!                                                      ! numerical solution come
!                                                      ! into question. If it is too
!                                                      ! thick, then resolution of
!                                                      ! diurnal cycle not as valid.
!                                                      ! Also chosen to comply with
!                                                      ! remotely sensed soil moisture.
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!        0.     Initialization
!               --------------
!
JNLVL = SIZE(PD_G)
!
IF (PRESENT(PD_G1)) ZD_G1 = PD_G1
!
IF (PSOILDEPTH < JNLVL*ZD_G1) THEN
  !
  !*       3.     In the LIMIT For extremely thin soils
  !               ------------------------------------------
  !               This should be a RARE occurance, but 
  !               accounted for none-the-less ...:
  !               hold the ratio between all layer 
  !               thicknesses constant. 
  DO JJ = 1,JNLVL
    PD_G(JJ) = JJ*PSOILDEPTH/JNLVL
  ENDDO
  !
ELSE
  !
  PD_G(1)     = ZD_G1
  PD_G(JNLVL) = PSOILDEPTH
  !
  DO JJ=JNLVL-1,2,-1
    !*       1.     Assign soil layer depths
    !               ------------------------
    !               using a geometric relation
    !               for layers 2...N
    !               This is GENERAL rule.
    !               Note that the first soil layer
    !               is FIXED except for VERY thin
    !               soils (see #3 below).
    PD_G(JJ) = PD_G(JJ+1)/ZGRIDFACTOR
    !*       2.     When the soil is sufficiently thin
    !               ------------------------------------------
    !               We recalculate layer depths such
    !               that all layer thicknesses are >= ZD_G1
    !               We favor keeping a minimum grid thickness
    !               OVER maintaining geometric relation
    !               for increasingly thin soils. This means
    !               that uppermost soil moisture is readily
    !               comparable (i.e. for same layer thickness)
    !               EVERYWHERE except for most thin soils (below).
    PD_G(JJ) = MAX(PD_G(JJ), JJ*ZD_G1)
    !
  ENDDO     
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TEBGRID
!
END SUBROUTINE THERMAL_LAYERS_CONF
