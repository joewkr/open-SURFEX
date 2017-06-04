!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE READ_GRIDTYPE_CONF_PROJ (HPROGRAM,KGRID_PAR,KLU,OREAD,KSIZE,PGRID_PAR,KRESP,HDIR)
!     ################################################################
!
!!****  *READ_GRIDTYPE_CONF_PROJ* - routine to initialise the horizontal grid
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
!!      Original    01/2004 
!!         M.Moge   02/2015 parallelization : using local fields
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_READ_SURF
USE MODI_GET_LUOUT
!
USE MODE_GRIDTYPE_CONF_PROJ
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
#ifdef MNH_PARALLEL
USE MODE_TOOLS_ll, ONLY : GET_DIM_PHYS_ll
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
 CHARACTER(LEN=6),       INTENT(IN)    :: HPROGRAM   ! calling program
INTEGER,                INTENT(INOUT) :: KGRID_PAR  ! real size of PGRID_PAR
INTEGER,                INTENT(IN)    :: KLU        ! number of points
LOGICAL,                INTENT(IN)    :: OREAD      ! flag to read the grid
INTEGER,                INTENT(IN)    :: KSIZE      ! estimated size of PGRID_PAR
REAL, DIMENSION(KSIZE), INTENT(OUT)   :: PGRID_PAR  ! parameters defining this grid
INTEGER,                INTENT(OUT)   :: KRESP      ! error return code
 CHARACTER(LEN=1),       INTENT(IN)    :: HDIR       ! reading directive
!                                                   ! 'A' : all field
!                                                   ! 'H' : field on this processor only
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZLON0    ! reference longitude
REAL                              :: ZRPK     ! projection parameter 
!                                             !   K=1 : stereographic north pole
!                                             ! 0<K<1 : Lambert, north hemisphere
!                                             !   K=0 : Mercator
!                                             !-1<K<0 : Lambert, south hemisphere
!                                             !   K=-1: stereographic south pole
REAL                              :: ZBETA    ! angle between grid and reference longitude
REAL                              :: ZLATORI  ! latitude  of point of coordinates X=0, Y=0
REAL                              :: ZLONORI  ! longitude of point of coordinates X=0, Y=0
INTEGER                           :: IIMAX    ! number of points in I direction
INTEGER                           :: IJMAX    ! number of points in J direction
REAL, DIMENSION(KLU)              :: ZX       ! X conformal coordinate of grid mesh
REAL, DIMENSION(KLU)              :: ZY       ! Y conformal coordinate of grid mesh
REAL, DIMENSION(KLU)              :: ZDX      ! X grid mesh size
REAL, DIMENSION(KLU)              :: ZDY      ! Y grid mesh size
!
#ifdef SFX_MNH
INTEGER                           :: IIMAX_LOC    ! number of points in I direction local
INTEGER                           :: IJMAX_LOC    ! number of points in J direction local
INTEGER                           :: IINFO
#endif
!
INTEGER                           :: ILUOUT
!---------------------------------------------------------------------------
REAL, DIMENSION(:),   POINTER     :: ZGRID_PAR=>NULL()
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Reading of projection parameters
!              --------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_CONF_PROJ',0,ZHOOK_HANDLE)
!
 CALL READ_SURF(HPROGRAM,'LAT0',ZLAT0,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'LON0',ZLON0,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'RPK ',ZRPK, KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'BETA',ZBETA,KRESP,HDIR=HDIR)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading parameters of the grid
!              ------------------------------
!
 CALL READ_SURF(HPROGRAM,'LATORI',ZLATORI,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'LONORI',ZLONORI,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'IMAX ',IIMAX, KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'JMAX ',IJMAX, KRESP,HDIR=HDIR)
!
#ifdef MNH_PARALLEL
 CALL GET_DIM_PHYS_ll('B',IIMAX_LOC,IJMAX_LOC)
#endif
!
 CALL READ_SURF(HPROGRAM,'XX',ZX,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'YY',ZY,KRESP,HDIR=HDIR)
!
 CALL READ_SURF(HPROGRAM,'DX',ZDX,KRESP,HDIR=HDIR)
 CALL READ_SURF(HPROGRAM,'DY',ZDY,KRESP,HDIR=HDIR)
!
!---------------------------------------------------------------------------
!
!*       4.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
#ifdef MNH_PARALLEL
 CALL PUT_GRIDTYPE_CONF_PROJ(ZGRID_PAR,ZLAT0,ZLON0,ZRPK,ZBETA,&
                              ZLATORI,ZLONORI,IIMAX_LOC,IJMAX_LOC,  &
                              ZX,ZY,ZDX,ZDY                    )
#else
 CALL PUT_GRIDTYPE_CONF_PROJ(ZGRID_PAR,ZLAT0,ZLON0,ZRPK,ZBETA,&
                              ZLATORI,ZLONORI,IIMAX,IJMAX,     &
                              ZX,ZY,ZDX,ZDY                    )  
#endif 
!
!---------------------------------------------------------------------------
IF (OREAD) THEN
  IF (SIZE(PGRID_PAR) /= SIZE(ZGRID_PAR)) THEN
    CALL GET_LUOUT(HPROGRAM,ILUOUT)
    WRITE(ILUOUT,*)'size of PGRID_PAR =', SIZE(PGRID_PAR)
    WRITE(ILUOUT,*)'size of ZGRID_PAR =', SIZE(ZGRID_PAR)
    CALL ABOR1_SFX('READ_GRIDTYPE_CONF_PROJ: SIZE OF PGRID_PAR IS NOT CORRECT')
  END IF
  !
  PGRID_PAR = ZGRID_PAR
ELSE
  KGRID_PAR = SIZE(ZGRID_PAR)
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_GRIDTYPE_CONF_PROJ',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_GRIDTYPE_CONF_PROJ
