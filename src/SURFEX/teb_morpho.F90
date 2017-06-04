!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################################################################
      SUBROUTINE TEB_MORPHO(HPROGRAM, T   )
!     ###########################################################################################################
!
!!****  *TEB_MORPHO* 
!!
!!    PURPOSE
!!    -------
!!**** routine to verify and compute the canyon/building morphology in TEB
!!
!!**  METHOD
!!    ------
!! the routine controls the canyon/building morphology
!!    - in the case of low building fraction (lower than 10^-4)
!!    - in the case of high building fraction (higher than 0.9999)
!!    - building height
!!    - in the case of low road fraction
!!    - in the case of low/hight wall surface ratio 
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
!!      G. Pigeon   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2011
!!      C. de Munck and A. lemonsu 05/2013 : - corrections in case of too high WALL_O_HOR (6.)
!!                                           - final check of parameters range added
!----------------------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TEB_n, ONLY : TEB_t
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
!
TYPE(TEB_t), INTENT(INOUT) :: T
!
!*       0.2   Declarations of local variables
!
INTEGER :: JJ
INTEGER :: ILUOUT
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZWALL_O_BLD   ! Initial wall to built surface ratio
REAL, DIMENSION(SIZE(T%XBLD)) :: ZWALL_O_HOR   ! Initial wall to horizontal surface ratio
!
REAL, DIMENSION(2) :: ZRANGE_BLD        = (/ 0.0001  ,   0.9999 /) ! Range allowed for T%XBLD variation
REAL, DIMENSION(2) :: ZRANGE_ROAD       = (/ 0.0001  ,   0.9999 /) ! Range allowed for T%XROAD variation
REAL, DIMENSION(2) :: ZRANGE_BLD_HEIGHT = (/ 3.      , 829.84   /) ! Range allowed for T%XBLD_HEIGHT variation
REAL, DIMENSION(2) :: ZRANGE_WALL_O_HOR = (/ 0.00012 , 322.     /) ! Range allowed for T%XWALL_O_HOR variation
!
!
!*       1.   Get listing file for warnings
!
CALL GET_LUOUT(HPROGRAM, ILUOUT)
!

ZWALL_O_BLD(:) = 0.
ZWALL_O_HOR(:) = 0.

DO JJ=1,SIZE(T%XBLD)
   !
   !*    2.   Control building height no lower than 3.m and no higher than 829.84m
   !          reference: http://en.wikipedia.org/wiki/List_of_tallest_buildings_and_structures_in_the_world (2011)
   !          and control Z0_TOWN
   !
   IF (T%XBLD_HEIGHT(JJ) < ZRANGE_BLD_HEIGHT(1) ) THEN
      T%XBLD_HEIGHT(JJ) = ZRANGE_BLD_HEIGHT(1)
   ENDIF
   IF (T%XBLD_HEIGHT(JJ) > ZRANGE_BLD_HEIGHT(2)) &
           CALL ABOR1_SFX('TEB_MORPHO: T%XBLD_HEIGHT higher than 829.84, highest building in the world, should be lower')
   !
   IF (T%XZ0_TOWN(JJ) > T%XBLD_HEIGHT(JJ)) THEN
      CALL ABOR1_SFX('TEB_MORPHO: T%XZ0_TOWN higher than T%XBLD_HEIGHT, should be lower')
   ENDIF
   !
   !*    3.   Control no and almost no building in the cell
   !          authorize building up to 10m and W_O_H 0.001
   !
   IF (T%XBLD(JJ) < ZRANGE_BLD(1) ) THEN
      T%XBLD(JJ) = ZRANGE_BLD(1)
      T%XGARDEN(JJ) = MIN(T%XGARDEN(JJ), 1.-2.*T%XBLD(JJ))
   ENDIF
   !
   !*    4.   Control only building in the cell: could occur for high resolution 
   !          theoretically W_O_H could be 0. -> impose that at least the wall surface is equal to the mesh perimeter x building 
   !          height for a mesh size of 100 x 100m; the waste heat is released at the roof level
   !
   IF (T%XBLD(JJ) > ZRANGE_BLD(2)) THEN
      T%XBLD(JJ) = ZRANGE_BLD(2)
      IF (T%XGARDEN(JJ) > 0.) THEN
         T%XGARDEN(JJ) = 0. 
      ENDIF
   ENDIF
   !
   !*    5.   Control wall surface low respective to building density and building height: pb of the input
   !          Evaluation of the minimum woh is done for mesh size of 1000. m
   !          wall surface of the building evaluated considering 1 square building
   !
   IF (T%XWALL_O_HOR(JJ) < 4. * SQRT(T%XBLD(JJ))*T%XBLD_HEIGHT(JJ)/1000.) THEN
      T%XWALL_O_HOR(JJ) = 4. * SQRT(T%XBLD(JJ))*T%XBLD_HEIGHT(JJ)/1000. 
   ENDIF
   !
   !*    6.   Control facade surface vs building height, case of too high WALL_O_HOR
   !
   T%XWALL_O_BLD(JJ) = T%XWALL_O_HOR(JJ)/T%XBLD(JJ)
   !
   IF (T%XWALL_O_BLD(JJ) > (0.4 * T%XBLD_HEIGHT(JJ))) THEN ! <=> side_of_building < 10 m
      !     
      ZWALL_O_HOR(JJ) = T%XWALL_O_HOR(JJ)
      ZWALL_O_BLD(JJ) = T%XWALL_O_BLD(JJ)
      !
      T%XWALL_O_HOR(JJ) = 0.4 * T%XBLD (JJ) * T%XBLD_HEIGHT(JJ) ! correction WOHOR v2.1
      T%XWALL_O_BLD(JJ) = T%XWALL_O_HOR(JJ) / T%XBLD       (JJ) ! correction WOHOR v2.1

   ENDIF
   !
   !*    7.   Verify road
   !
   T%XROAD      (JJ) = 1.-(T%XGARDEN(JJ)+T%XBLD(JJ))
   IF (T%XROAD(JJ) <= ZRANGE_ROAD(1) ) THEN
      T%XROAD(JJ) = ZRANGE_ROAD(1)
      T%XGARDEN(JJ) = MAX(T%XGARDEN(JJ) - ZRANGE_ROAD(1), 0.)
      IF (T%XH_TRAFFIC(JJ) > 0. .OR. T%XLE_TRAFFIC(JJ) > 0.) THEN
         T%XH_TRAFFIC(JJ)  = 0.
         T%XLE_TRAFFIC(JJ) = 0.
      ENDIF
   ENDIF
   !
   !*    8.   Final check of parameters range
   !
   IF ( T%XBLD(JJ) < ZRANGE_BLD(1) .OR. T%XBLD(JJ) > ZRANGE_BLD(2) ) THEN
        WRITE(ILUOUT,*) 'WARNING : T%XBLD is still out of range after final corrections &
        &for grid mesh',JJ,' : ',T%XBLD(JJ)
   ENDIF
   !
   IF ( T%XBLD_HEIGHT(JJ) < ZRANGE_BLD_HEIGHT(1) .OR. T%XBLD_HEIGHT(JJ) > ZRANGE_BLD_HEIGHT(2) ) THEN
        WRITE(ILUOUT,*) 'WARNING : T%XBLD_HEIGHT is still out of range after final corrections &
        &for grid mesh',JJ,' : ',T%XBLD_HEIGHT(JJ)
   ENDIF
   !
   IF ( T%XWALL_O_HOR(JJ) < ZRANGE_WALL_O_HOR(1) .OR. T%XWALL_O_HOR(JJ) > ZRANGE_WALL_O_HOR(2) ) THEN
        WRITE(ILUOUT,*) 'WARNING : T%XWALL_O_HOR is still out of range after final corrections &
        &for grid mesh',JJ,' : ',T%XWALL_O_HOR(JJ)
   ENDIF
   !
   IF ( T%XWALL_O_BLD(JJ) - (0.4 * T%XBLD_HEIGHT(JJ)) > 10E-16 ) THEN
        WRITE(ILUOUT,*) 'WARNING : T%XWALL_O_BLD is still too high after final corrections &
        &for grid mesh',JJ,' : ',T%XWALL_O_BLD(JJ)
   ENDIF
   !
ENDDO
!
!
!*    9.   Compute morphometric parameters 
!
T%XCAN_HW_RATIO(:)    = 0.5 * T%XWALL_O_HOR(:) / (1.-T%XBLD(:))
!
!* relative surface fraction
!
T%XROAD_O_GRND(:)   = T%XROAD(:)       / (T%XROAD(:) + T%XGARDEN(:))
T%XGARDEN_O_GRND(:) = T%XGARDEN(:)     / (T%XROAD(:) + T%XGARDEN(:))
T%XWALL_O_GRND(:)   = T%XWALL_O_HOR(:) / (T%XROAD(:) + T%XGARDEN(:))
!
!* Sky-view-factors:
!
T%XSVF_ROAD  (:) = (SQRT(T%XCAN_HW_RATIO(:)**2+1.) - T%XCAN_HW_RATIO(:))
T%XSVF_GARDEN(:) = T%XSVF_ROAD(:)
T%XSVF_WALL  (:) =  0.5*(T%XCAN_HW_RATIO(:)+1.-SQRT(T%XCAN_HW_RATIO(:)**2+1.))/T%XCAN_HW_RATIO(:)
!
END SUBROUTINE TEB_MORPHO
