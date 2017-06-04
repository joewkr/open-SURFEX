!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
      MODULE MODE_GRIDTYPE_LONLATVAL
!     ##############################
!
!############################################################################
!############################################################################
!############################################################################
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE PUT_GRIDTYPE_LONLATVAL(PGRID_PAR,PX,PY,PDX,PDY)
!     ####################################################################
!
!!****  *PUT_GRIDTYPE_LONLATVAL* - routine to store in PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      S.Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2010 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PX       ! X coordinate of grid mesh center
REAL, DIMENSION(:), INTENT(IN)  :: PY       ! Y coordinate of grid mesh center
REAL, DIMENSION(:), INTENT(IN)  :: PDX      ! X grid mesh size
REAL, DIMENSION(:), INTENT(IN)  :: PDY      ! Y grid mesh size
REAL, DIMENSION(:), POINTER     :: PGRID_PAR! parameters defining this grid
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL ! number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLATVAL:PUT_GRIDTYPE_LONLATVAL',0,ZHOOK_HANDLE)
IL = SIZE(PX)
ALLOCATE(PGRID_PAR(1+4*IL))

PGRID_PAR(1) = FLOAT(IL)
PGRID_PAR(2:IL+1)        = PX(:)
PGRID_PAR(IL+2:2*IL+1)   = PY(:)
PGRID_PAR(2*IL+2:3*IL+1) = PDX(:)
PGRID_PAR(3*IL+2:4*IL+1) = PDY(:)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLATVAL:PUT_GRIDTYPE_LONLATVAL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRIDTYPE_LONLATVAL
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRIDTYPE_LONLATVAL(PGRID_PAR,KL,PX,PY,PDX,PDY)
!     ####################################################################
!
!!****  *GET_GRIDTYPE_LONLATVAL* - routine to get from PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2010 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,            INTENT(OUT), OPTIONAL  :: KL ! number of points
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PX       ! X coordinate of grid mesh center
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PY       ! Y coordinate of grid mesh center
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PDX      ! X grid mesh size
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PDY      ! Y grid mesh size
REAL, DIMENSION(:), INTENT(IN)            :: PGRID_PAR! parameters defining this grid
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLATVAL:GET_GRIDTYPE_LONLATVAL',0,ZHOOK_HANDLE)
IF (PRESENT(KL))        KL       = NINT(PGRID_PAR(1))
!
IF (PRESENT(PX)) THEN
  IL = NINT(PGRID_PAR(1))
  PX(:) = PGRID_PAR(2:1+IL)
END IF

IF (PRESENT(PY)) THEN
  IL = NINT(PGRID_PAR(1))
  PY(:) = PGRID_PAR(1+IL+1:1+2*IL)
END IF

IF (PRESENT(PDX)) THEN
  IL = NINT(PGRID_PAR(1))
  PDX(:)= PGRID_PAR(1+2*IL+1:1+3*IL)
END IF

IF (PRESENT(PDY)) THEN
  IL = NINT(PGRID_PAR(1))
  PDY(:)= PGRID_PAR(1+3*IL+1:1+4*IL)
END IF
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLATVAL:GET_GRIDTYPE_LONLATVAL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRIDTYPE_LONLATVAL
!############################################################################
!############################################################################
!############################################################################
!      ###################################################
       SUBROUTINE LATLON_LONLATVAL(PX,PY,PLAT,PLON)
!      ###################################################
!
!!****  *LATLON_LONLATVAL * - Routine to compute geographical coordinates
!!
!!     PURPOSE
!!     -------
!        This routine computes the latitude and longitude of
!      an array given in LAMBERT coordinates
!
!
!!**   METHOD
!!     ------
!!
!!     EXTERNAL
!!     --------
!!       None
!!
!!     REFERENCE
!!     ---------
!!       
!!     AUTHOR
!!     ------
!!      S.Faroux   *Meteo-France*
!!
!!     MODIFICATION
!!     ------------
!!       Original  02/2010        
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!       
REAL, DIMENSION(:),   INTENT(IN) :: PX,PY
                                           ! given conformal coordinates of the 
                                           ! processed points (meters);
REAL, DIMENSION(:),   INTENT(OUT):: PLAT,PLON    
REAL(KIND=JPRB) :: ZHOOK_HANDLE
                                           ! returned geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
!
!
      IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLATVAL:LATLON_LONLATVAL',0,ZHOOK_HANDLE)
      PLON(:)=PX(:)
!   
      PLAT(:)=PY(:)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_LONLATVAL:LATLON_LONLATVAL',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------------
END SUBROUTINE LATLON_LONLATVAL
!---------------------------------------------------------------------------------
!
END MODULE MODE_GRIDTYPE_LONLATVAL
