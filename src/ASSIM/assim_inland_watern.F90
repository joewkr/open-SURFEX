!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE ASSIM_INLAND_WATER_n (NPE, W, U, HPROGRAM, KI, PTS_IN, PITM, HTEST, &
                                 OLKEEPEXTZONE, OD_MASKEXT, PLON_IN, PLAT_IN)

!     ###############################################################################
!
!!****  *ASSIM_INLAND_WATER_n * - Chooses the surface assimilation schemes for INLAND_WATER parts  
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
!!     T. Aspelien
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012
!!      Trygve Aspelien, Separating IO  06/2013
!!--------------------------------------------------------------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_NPE_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ASSIM,          ONLY : NPRINTLEV,LEXTRAP_WATER,LWATERTG2
!
USE YOMHOOK,             ONLY : LHOOK,DR_HOOK
USE PARKIND1,            ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_OI_HOR_EXTRAPOL_SURF
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,            INTENT(IN) :: KI
REAL,DIMENSION(KI), INTENT(IN) :: PTS_IN
REAL,DIMENSION(KI), INTENT(IN) :: PITM
CHARACTER(LEN=2),   INTENT(IN) :: HTEST ! must be equal to 'OK'
LOGICAL, INTENT(IN) :: OLKEEPEXTZONE
LOGICAL, DIMENSION(KI), INTENT(IN) :: OD_MASKEXT
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLON_IN
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLAT_IN
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION(KI)              :: ZLST
REAL, DIMENSION(KI)              :: ZLST0
REAL, DIMENSION(KI)              :: ZLSTINC
REAL, DIMENSION(:), ALLOCATABLE  :: ZLST01, ZLST1, ZLON1, ZLAT1, ZALT1 
!
LOGICAL,DIMENSION(KI) :: GINTERP_LST
LOGICAL, DIMENSION(:), ALLOCATABLE :: GINTERP_LST1
INTEGER  :: IRESP,JI,JJ,IS1,J1
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ASSIM_INLAND_WATER_N',0,ZHOOK_HANDLE)

IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_INLAND_WATER_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF

IF (NRANK==NPIO) WRITE(*,*) 'UPDATING LST FOR INLAND_WATER: ',TRIM(U%CWATER)
IF (U%CWATER=="NONE") THEN
  IF (LHOOK) CALL DR_HOOK('ASSIM_INLAND_WATER_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!*     ZLST updated!
!
ZLST(:) = XUNDEF
IF (.NOT.LWATERTG2 ) THEN
  !*     ZLST updated from from CANARI analysis
  DO JI=1,KI
    IF ( PITM(JI)<0.5 ) ZLST(JI) = PTS_IN(JI)
  ENDDO
  !
ELSE
  ! Set TG2 from global array
  DO JI=1,KI
    IF ( PITM(JI)>0.5 ) THEN
      !*     ZLST updated from LAND values of climatological TS
      DO JJ=1,U%NSIZE_NATURE
        IF ( U%NR_WATER(JI)==U%NR_NATURE(JJ) ) THEN
          ZLST(JI) = NPE%AL(1)%XTG(JJ,2)
          EXIT
        ENDIF
      ENDDO
    ENDIF
  ENDDO
  !
ENDIF
!
! Set local array from global
GINTERP_LST(:) = .FALSE.
DO JI=1,KI
  IF ( ZLST(JI)/=XUNDEF ) THEN
    ZLST0(JI) = ZLST(JI)
  ELSEIF ( LEXTRAP_WATER ) THEN
    ! Keep ZLST or do extrapolation from neighbour points
    ZLST0(JI) = XUNDEF
    GINTERP_LST(JI) = .TRUE.  
  ELSE
    ZLST0(JI) = W%XTS(JI)
  ENDIF
ENDDO
!
IF ( LEXTRAP_WATER ) THEN
  !
  IF (OLKEEPEXTZONE) THEN
    !     
    ZLST(:) = ZLST0(:)
    WHERE ( OD_MASKEXT(:) ) ZLST0(:) = XUNDEF
    CALL OI_HOR_EXTRAPOL_SURF(KI,PLAT_IN,PLON_IN,ZLST0,PLAT_IN,PLON_IN,ZLST,GINTERP_LST,W%XZS)
    !
  ELSE
    !
    IS1 = COUNT (.NOT.OD_MASKEXT)
    ALLOCATE (ZLST1(IS1), ZLST01(IS1), ZLAT1(IS1), ZLON1(IS1), ZALT1(IS1), GINTERP_LST1(IS1))
    !
    ! remove extension zone
    JJ = 1
    DO J1 = 1, KI
      IF ( .NOT.OD_MASKEXT(J1) )  THEN
        ZLST01(JJ) = ZLST0(J1)
        ZLAT1 (JJ) = PLAT_IN (J1)
        ZLON1 (JJ) = PLON_IN (J1)
        ZALT1 (JJ) = W%XZS  (J1)
        GINTERP_LST1(JJ) = GINTERP_LST(J1)
        JJ = JJ + 1
      ENDIF
    ENDDO
       
    ZLST1(:) = ZLST01(:)
    CALL OI_HOR_EXTRAPOL_SURF(IS1,ZLAT1,ZLON1,ZLST01,ZLAT1,ZLON1,ZLST1,GINTERP_LST1,ZALT1)
    !
    ! copy back
    JJ = 1
    DO J1 = 1, KI
      IF ( .NOT.OD_MASKEXT(J1) ) THEN
        ZLST(J1) = ZLST1(JJ)
        JJ = JJ + 1
      ENDIF
    ENDDO
    !
    DEALLOCATE (ZLST01, ZLST1, ZLAT1, ZLON1, ZALT1, GINTERP_LST1)
    !
  ENDIF
  !
ENDIF
!
!*     Print values produced by OI_HO_EXTRAPOL_SURF
IF ( NPRINTLEV > 2 ) THEN
  DO JI=1,KI
    IF (GINTERP_LST(JI)) THEN
      PRINT *,'Lake surface temperature set to ',ZLST(JI),'from nearest neighbour at I=',U%NR_WATER(JI)
    ENDIF
  ENDDO
ENDIF
!
! Sum the increments
IF (ALL(ZLST(:)/=XUNDEF)) THEN
  ZLSTINC(:) = ZLST(:) - W%XTS(:)
  ! Setting modified variables
  W%XTS(:) = ZLST(:)
ELSE
  ZLSTINC(:) = 0.
ENDIF
!
WRITE(*,*) 'Mean LST increments over inland water   ',SUM(ZLSTINC)/KI
!
IF (LHOOK) CALL DR_HOOK('ASSIM_INLAND_WATER_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_INLAND_WATER_n
