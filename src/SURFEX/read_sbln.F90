!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SBL_n (DTCO, U, SB, OSBL, HPROGRAM, HSURF)
!     #########################################
!
!!****  *READ_SBL_n* - reads TEB fields
!!                        
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
!!      E. Martin   01/2012 Add LSBL_COLD_START
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_READ_SURF
USE MODI_CANOPY_GRID
USE MODI_GET_TYPE_DIM_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CANOPY_t), INTENT(INOUT) :: SB
LOGICAL, INTENT(INOUT) :: OSBL
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=6), INTENT(IN) :: HSURF
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
 CHARACTER(LEN=8) :: YBASE
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=13) :: YFORMAT
 CHARACTER(LEN=3)  :: YREAD
INTEGER :: JLAYER  ! loop counter on layers
INTEGER :: ILU     ! 1D physical dimension
INTEGER :: IRESP   ! Error code after redding
INTEGER           :: IVERSION, IBUGFIX   ! surface version
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_SBL_N',0,ZHOOK_HANDLE)
!
 CALL GET_TYPE_DIM_n(DTCO, U, HSURF, ILU)
!
!* flag to use or not canopy levels
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
IF (IVERSION<3) THEN
  OSBL = .FALSE.
ELSE
  IF (HSURF=="TOWN  ") THEN
    YRECFM='TEB_CANOPY'
  ELSEIF (HSURF=="WATER ") THEN
    YRECFM='WAT_SBL'
  ELSEIF (HSURF=="NATURE") THEN
    YRECFM='ISBA_CANOPY'
  ELSEIF (HSURF=="SEA   ") THEN
    YRECFM='SEA_SBL'
  ENDIF
  CALL READ_SURF(HPROGRAM,YRECFM,OSBL,IRESP)
END IF
!
IF (.NOT.OSBL) THEN
  ALLOCATE(SB%XZ  (0,0))
  ALLOCATE(SB%XU  (0,0))
  ALLOCATE(SB%XT  (0,0))
  ALLOCATE(SB%XQ  (0,0))
  ALLOCATE(SB%XTKE(0,0))
  ALLOCATE(SB%XLMO(0,0))
  ALLOCATE(SB%XP  (0,0))
  IF (HSURF=="TOWN  ") THEN
    ALLOCATE(SB%XLM (0,0))
    ALLOCATE(SB%XLEPS(0,0))  
  ENDIF
  ALLOCATE(SB%XDZ (0,0))
  ALLOCATE(SB%XZF (0,0))
  ALLOCATE(SB%XDZF(0,0))
  IF (LHOOK) CALL DR_HOOK('READ_SBL_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!* number of vertical levels
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
YRECFM=TRIM(YBASE)//'_LVL'
 CALL READ_SURF(HPROGRAM,YRECFM,SB%NLVL,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* altitudes
!
ALLOCATE(SB%XZ(ILU,SB%NLVL))
!
DO JLAYER=1,SB%NLVL
  WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_Z',JLAYER
  CALL READ_SURF(HPROGRAM,YRECFM,SB%XZ(:,JLAYER),IRESP)
END DO
!
ALLOCATE(SB%XU  (ILU,SB%NLVL))
ALLOCATE(SB%XT  (ILU,SB%NLVL))
ALLOCATE(SB%XQ  (ILU,SB%NLVL))
ALLOCATE(SB%XTKE(ILU,SB%NLVL))
ALLOCATE(SB%XLMO(ILU,SB%NLVL))
ALLOCATE(SB%XP  (ILU,SB%NLVL))
!
IF (IVERSION>7 .OR. IVERSION==7 .AND.IBUGFIX>=2) THEN
  YRECFM='STORAGETYPE'
  CALL READ_SURF(HPROGRAM,YRECFM,YREAD,IRESP)
ELSE
  YREAD = 'ALL'
ENDIF
!
IF(YREAD=='ALL') THEN
  !
  !* wind in SBL
  DO JLAYER=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_U',JLAYER
    CALL READ_SURF(HPROGRAM,YRECFM,SB%XU(:,JLAYER),IRESP)
  END DO
  !
  !* theta in SBL
  DO JLAYER=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_T',JLAYER
    CALL READ_SURF(HPROGRAM,YRECFM,SB%XT(:,JLAYER),IRESP)
  END DO
  !
  !* humidity in SBL
  DO JLAYER=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_Q',JLAYER
    CALL READ_SURF( HPROGRAM,YRECFM,SB%XQ(:,JLAYER),IRESP)
  END DO
  !
  !* Tke in SBL
  DO JLAYER=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_E',JLAYER
    CALL READ_SURF(HPROGRAM,YRECFM,SB%XTKE(:,JLAYER),IRESP)
  END DO
  !
  !* Monin-Obhukov length
  IF (IVERSION<7 .OR. HSURF/="TOWN  ") THEN
    YRECFM=TRIM(YBASE)//'_LMO ' 
    CALL READ_SURF(HPROGRAM,YRECFM,SB%XLMO(:,1),IRESP) 
    DO JLAYER = 2,SB%NLVL
      SB%XLMO(:,JLAYER) = SB%XLMO(:,1)
    ENDDO    
  ELSE
    DO JLAYER=1,SB%NLVL
      WRITE(YRECFM,'(A10,I2.2)') TRIM(YBASE)//'_MO',JLAYER
      CALL READ_SURF(HPROGRAM,YRECFM,SB%XLMO(:,JLAYER),IRESP)
    ENDDO
  ENDIF    
  !
  !* Pressure
  DO JLAYER=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_P',JLAYER
    CALL READ_SURF(HPROGRAM,YRECFM,SB%XP(:,JLAYER),IRESP)
  END DO
  !
ELSE
  SB%XU  (:,:) = XUNDEF
  SB%XT  (:,:) = XUNDEF
  SB%XQ  (:,:) = XUNDEF
  SB%XTKE(:,:) = XUNDEF
  SB%XLMO(:,:) = XUNDEF
  SB%XP  (:,:) = XUNDEF
ENDIF
!
IF (HSURF=="TOWN  ") THEN
  !
  !* mixing length
  !
  ALLOCATE(SB%XLM(ILU,SB%NLVL))
  !
  !* dissipative length
  !
  ALLOCATE(SB%XLEPS(ILU,SB%NLVL))
  !
ENDIF
!
!
!* Grid characteristics
!
!
!  --------------------------------- XZ(k+1)                     XDZ(k+1)
!                                                                           ^
!                                                                           |
!                                                                           |
!  - - - - - - - - - - - - - - - - - XZf(k+1)                               | XDZf(k+1)
!                                                              ^            |
!                                                              |            |
!  --------------------------------- XZ(k), XU, XT, XQ, XTKE   | XDZ(k)     V
!                                                              |            ^
!  - - - - - - - - - - - - - - - - - XZf(k)                    V            | XDZf(k)
!  --------------------------------- XZ(k-1)                     XDZ(k-1)   V
!  - - - - - - - - - - - - - - - - - XZf(k-1)
!
ALLOCATE(SB%XDZ (ILU,SB%NLVL))
ALLOCATE(SB%XZF (ILU,SB%NLVL))
ALLOCATE(SB%XDZF(ILU,SB%NLVL))
!
 CALL CANOPY_GRID(ILU,SB)
!
IF (LHOOK) CALL DR_HOOK('READ_SBL_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_SBL_n
