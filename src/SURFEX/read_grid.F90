!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_GRID (HPROGRAM,G,KRESP,PDIR)
!     #########################################
!
!!****  *READ_GRID* - routine to initialise the horizontal grid of a scheme
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_LATLON_GRID
USE MODI_READ_GRIDTYPE
!
USE MODD_ASSIM, ONLY : LREAD_ALL, LASSIM
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
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM   ! calling program
TYPE(GRID_t), INTENT(INOUT) :: G
INTEGER,            INTENT(OUT) :: KRESP      ! error return code
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PDIR ! heading of main axis of grid compared to North (degrees)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
LOGICAL :: GREAD_ALL
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Reading of type of grid
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GRID',0,ZHOOK_HANDLE)
!
IF (LASSIM) THEN
  GREAD_ALL = LREAD_ALL
  LREAD_ALL = .TRUE.
ENDIF
!
 CALL READ_SURF(HPROGRAM,'GRID_TYPE',G%CGRID,KRESP)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading parameters of the grid
!              ------------------------------
!
 CALL READ_GRIDTYPE(HPROGRAM,G%CGRID,G%NGRID_PAR,SIZE(G%XLAT),.FALSE.)
!
ALLOCATE(G%XGRID_PAR(G%NGRID_PAR))
 CALL READ_GRIDTYPE(HPROGRAM,G%CGRID,G%NGRID_PAR,SIZE(G%XLAT),.TRUE.,G%XGRID_PAR,KRESP)
!
!---------------------------------------------------------------------------
!
!*       3.    Latitude, longitude, mesh size
!              ------------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
SELECT CASE (G%CGRID)
  CASE("NONE      ")
    IF (PRESENT(PDIR)) PDIR(:) = 0.
    !
    CALL READ_SURF(HPROGRAM,'LON',      G%XLON,KRESP)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('READ_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
    CALL READ_SURF(HPROGRAM,'LAT',      G%XLAT,KRESP)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('READ_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN
    CALL READ_SURF(HPROGRAM,'MESH_SIZE',G%XMESH_SIZE,KRESP)
    IF (KRESP/=0 .AND. LHOOK) CALL DR_HOOK('READ_GRID',1,ZHOOK_HANDLE)
    IF (KRESP/=0) RETURN

  CASE DEFAULT
    IF (PRESENT(PDIR)) THEN
      CALL LATLON_GRID(G,SIZE(G%XLAT),PDIR)
    ELSE
      CALL LATLON_GRID(G,SIZE(G%XLAT))
    END IF

END SELECT
!
IF (LASSIM) LREAD_ALL = GREAD_ALL
!
IF (LHOOK) CALL DR_HOOK('READ_GRID',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_GRID
