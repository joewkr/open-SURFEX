!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SBL_n (HSELECT, OSBL, SB, HPROGRAM, HWRITE, HSURF)
!     ####################################
!
!!****  *WRITE_FLAKE_n* - writes FLAKE fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      E. Martin   01/2012 avoid write of XUNDEF fields
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
USE MODI_INIT_IO_SURF_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT 
 LOGICAL, INTENT(IN) :: OSBL
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
 CHARACTER(LEN=6), INTENT(IN) :: HSURF
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=8) :: YBASE
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=13) :: YFORMAT 
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
INTEGER :: JL  ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.     Prognostic fields:
!               -----------------
!
!* flag to define if SBL is computed
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SBL_N',0,ZHOOK_HANDLE)
!
IF (HSURF=="TOWN  ") THEN
  YRECFM='TEB_CANOPY'
ELSEIF (HSURF=="WATER ") THEN
  YRECFM='WAT_SBL'
ELSEIF (HSURF=="NATURE") THEN
  YRECFM='ISBA_CANOPY'
ELSEIF (HSURF=="SEA   ") THEN
  YRECFM='SEA_SBL'
ENDIF
YCOMMENT='flag to use SBL levels'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,OSBL,IRESP,HCOMMENT=YCOMMENT)
!
IF (.NOT. OSBL .AND. LHOOK) CALL DR_HOOK('WRITESURF_SBL_N',1,ZHOOK_HANDLE)
IF (.NOT. OSBL) RETURN
!
IF (HSURF=="TOWN  ") THEN
  YBASE = "TEB_CAN "
ELSEIF (HSURF=="WATER ") THEN
  YBASE = "WAT_SBL "
ELSEIF (HSURF=="NATURE") THEN
  YBASE = "ISBA_CAN"
ELSEIF (HSURF=="SEA   ") THEN
  YBASE = "SEA_SBL "
ENDIF
!
IF (HSURF=="NATURE") THEN
  YFORMAT='(A10,I2.2)'
ELSE
  YFORMAT='(A9,I2.2) '
ENDIF
!
!* number of levels
!
YRECFM=TRIM(YBASE)//'_LVL'
YCOMMENT='number of SBL levels'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%NLVL,IRESP,HCOMMENT=YCOMMENT)
!
!* altitudes
!
DO JL=1,SB%NLVL
  WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_Z',JL
  YCOMMENT='altitudes of SBL levels (m)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XZ(:,JL),IRESP,HCOMMENT=YCOMMENT)
END DO
!
IF (HWRITE/='PRE') THEN
  !
  !* wind in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_U',JL
    YCOMMENT='wind at SBL levels (m/s)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XU(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* temperature in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_T',JL
    YCOMMENT='temperature at SBL levels (K)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XT(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* humidity in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_Q',JL
    YCOMMENT='humidity at SBL levels (kg/m3)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XQ(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Tke in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_E',JL
    YCOMMENT='Tke at SBL levels (m2/s2)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XTKE(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Monin-Obhukov length
  !
  IF (HSURF=="TOWN  ") THEN
    !
    DO JL=1,SB%NLVL
      WRITE(YRECFM,'(A10,I2.2)') TRIM(YBASE)//'_MO',JL
      YCOMMENT='Monin-Obukhov length (m)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XLMO(:,JL),IRESP,HCOMMENT=YCOMMENT)
    END DO 
    !  
    !* mixing length
    !
    IF (ASSOCIATED(SB%XLM)) THEN
      DO JL=1,SB%NLVL
        WRITE(YRECFM,'(A10,I2.2)') TRIM(YBASE)//'_LM',JL
        YCOMMENT='mixing length (m)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XLM(:,JL),IRESP,HCOMMENT=YCOMMENT)
     END DO
    END IF
    !
    !* dissipative length
    !
    IF (ASSOCIATED(SB%XLEPS)) THEN
      DO JL=1,SB%NLVL
        WRITE(YRECFM,'(A10,I2.2)') TRIM(YBASE)//'_LE',JL
        YCOMMENT='mixing length (m)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XLEPS(:,JL),IRESP,HCOMMENT=YCOMMENT)
      END DO
    END IF 
    !   
  ELSE
    YRECFM=TRIM(YBASE)//'_LMO     '
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XLMO(:,SB%NLVL),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !* Air pressure in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_P',JL
    YCOMMENT='Pressure at SBL levels (Pa)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XP(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SBL_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SBL_n
