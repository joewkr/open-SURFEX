!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE ASSIM_SEA_n (S, U, HPROGRAM,KI,PTS_IN,PSST_IN,PSIC_IN,PITM,HTEST, &
                        OLKEEPEXTZONE,OD_MASKEXT,PLON_IN,PLAT_IN)

!     ###############################################################################
!
!!****  *ASSIM_SEA_n * - Chooses the surface assimilation schemes for SEA tile
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
!!      Original       04/2012
!!      Trygve Aspelien, Separating IO  06/2013 
!!--------------------------------------------------------------------
!
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ASSIM,          ONLY : NPRINTLEV,LAESST,LEXTRAP_SEA
!
!
USE YOMHOOK,             ONLY : LHOOK,DR_HOOK
USE PARKIND1,            ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_PACK_SAME_RANK
USE MODI_OI_HOR_EXTRAPOL_SURF
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,            INTENT(IN) :: KI
REAL,DIMENSION(KI), INTENT(IN) :: PTS_IN
REAL,DIMENSION(KI), INTENT(IN) :: PSST_IN
REAL,DIMENSION(KI), INTENT(IN) :: PSIC_IN
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
REAL, DIMENSION(KI) :: ZALT
REAL, DIMENSION(KI) :: ZSST
REAL, DIMENSION(KI) :: ZSST0
REAL, DIMENSION(KI) :: ZSSTINC
REAL, DIMENSION(:), ALLOCATABLE :: ZSST01, ZSST1, ZLON1, ZLAT1, ZALT1 
REAL :: ZFMAX, ZFMIN, ZFMEAN
LOGICAL, DIMENSION(KI) :: GINTERP_SST
LOGICAL, DIMENSION(:), ALLOCATABLE :: GINTERP_SST1
INTEGER  :: IRESP, I, J, J1, IS1
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ASSIM_SEA_N',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_SEA_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
IF (NRANK==NPIO) WRITE(*,*) 'UPDATING SST FOR SCHEME: ',TRIM(U%CSEA)
IF (U%CSEA=="NONE") THEN
  IF (LHOOK) CALL DR_HOOK('ASSIM_SEA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CALL PACK_SAME_RANK(U%NR_SEA,U%XZS,ZALT)
!
! Read SST from file or set it to input SST
IF ( .NOT.LAESST ) THEN
  ! Set SST to input
  ZSST(:) = PSST_IN(:)
  !
ELSE
  ! SST analysed in CANARI 
  ZSST(:) = XUNDEF
  DO I=1,KI
    IF (PITM(I)<0.5 .AND. U%XSEA(U%NR_SEA(I))/=0. ) THEN
     ZSST(I) = PTS_IN(I)   ! set SST analysis from CANARI
    ENDIF
  END DO
  !
  ZFMIN = MINVAL(ZSST)
  ZFMAX = MAXVAL(ZSST)
  ZFMEAN = SUM(ZSST)/FLOAT(KI)
  WRITE(*,*) '  SST analysis from CANARI '
  WRITE(*,'("  ZSST            - min, mean, max: ",3E13.4)') ZFMIN, ZFMEAN, ZFMAX
ENDIF
!*     PSST updated at all sea points with ZSST where ZSST is available
GINTERP_SST(:) = .FALSE.
! Set SST from watfluxn
DO I=1,KI
  !
  IF ( ZSST(I)/=XUNDEF ) THEN
    ZSST0(I) = ZSST(I)
  ELSEIF ( LEXTRAP_SEA ) THEN
    ZSST0(I) = XUNDEF
    GINTERP_SST(I) = .TRUE.
  ELSE
    ZSST0(I) = S%XSST(I)
  ENDIF
  !
ENDDO
!
IF ( LEXTRAP_SEA ) THEN
  !
  IF (OLKEEPEXTZONE) THEN
    !     
    ZSST(:) = ZSST0(:)
    WHERE ( OD_MASKEXT(:) ) ZSST0(:) = XUNDEF
    CALL OI_HOR_EXTRAPOL_SURF(KI,PLAT_IN,PLON_IN,ZSST0,PLAT_IN,PLON_IN,ZSST,GINTERP_SST,ZALT)
    !
  ELSE
    !
    IS1 = COUNT (.NOT. OD_MASKEXT)
    ALLOCATE (ZSST1(IS1), ZSST01(IS1), ZLAT1(IS1), ZLON1(IS1), ZALT1(IS1), GINTERP_SST1(IS1))
    !
    ! remove extension zone
    J = 1
    DO J1 = 1, KI
      IF (.NOT. OD_MASKEXT (J1)) THEN
        ZSST01(J) = ZSST0(J1)
        ZLAT1 (J) = PLAT_IN (J1)
        ZLON1 (J) = PLON_IN (J1)
        ZALT1 (J) = U%XZS  (J1)
        GINTERP_SST1(J) = GINTERP_SST(J1)
        J = J + 1
      ENDIF
    ENDDO
      
    ZSST1(:) = ZSST01(:)
    CALL OI_HOR_EXTRAPOL_SURF(IS1,ZLAT1,ZLON1,ZSST01,ZLAT1,ZLON1,ZSST1,GINTERP_SST1,ZALT1)
 
    ! copy back
    J = 1
    DO J1 = 1, KI
      IF (.NOT. OD_MASKEXT(J1)) THEN
        ZSST(J1) = ZSST1(J)
        J = J + 1
      ENDIF
    ENDDO
    !
    DEALLOCATE (ZSST01, ZSST1, ZLAT1, ZLON1, ZALT1, GINTERP_SST1)
    !
  ENDIF
  !
ENDIF
!
!*     Print values produced by OI_HO_EXTRAPOL_SURF
IF ( NPRINTLEV > 2 ) THEN
  DO I=1,KI
    IF (GINTERP_SST(I)) THEN
      PRINT *,'Sea surface temperature set to ',ZSST(I),'from nearest neighbour at I=',U%NR_SEA(I)
    ENDIF
  ENDDO
ENDIF
!
! Sum the increments
ZSSTINC(:) = ZSST(:) - S%XSST(:)
IF (KI>0) WRITE(*,*) 'Mean SST increments over SEA   ',SUM(ZSSTINC)/KI
!
! Setting modified variables
S%XSST(:) = ZSST(:)
!
IF (LHOOK) CALL DR_HOOK('ASSIM_SEA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_SEA_n
