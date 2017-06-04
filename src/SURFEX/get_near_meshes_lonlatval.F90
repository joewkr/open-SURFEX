!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_NEAR_MESHES_LONLATVAL(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)
!     ##############################################################
!
!!**** *GET_NEAR_MESHES_LONLATVAL* get the near grid mesh indices
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_LONLATVAL
!
USE MODD_SURFEX_MPI, ONLY : NINDEX, NRANK, NNUM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
INTEGER,                         INTENT(IN)    :: KNEAR_NBR ! number of nearest points wanted
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
INTEGER, DIMENSION(:,:),POINTER  :: KNEAR     ! near mesh indices
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(KL) :: ZDIS
REAL,DIMENSION(KL)    :: ZX
REAL,DIMENSION(KL)    :: ZY
REAL,DIMENSION(KL)    :: ZDX
REAL,DIMENSION(KL)    :: ZDY
REAL :: ZDMAX
INTEGER :: ID0
INTEGER :: JP1, JP2, JN, IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_LONLATVAL',0,ZHOOK_HANDLE)
!
 CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR,IL,ZX,ZY,ZDX,ZDY)
!
KNEAR  (:,:) = 0
!
! calcul de la distance de tous les points 2 à 2
!
ZDIS = 1.E20
!
DO JP1=1,KL
  !
  IF (NINDEX(JP1)==NRANK) THEN
    !
    DO JP2=1,KL
      ZDIS(JP2) = SQRT((ZX(JP1)-ZX(JP2))**2+(ZY(JP1)-ZY(JP2))**2)
    ENDDO
    ZDMAX = MAXVAL(ZDIS(:)) + 1.
    ZDIS(JP1) = ZDMAX
    !
    ! on prend les knear_nbr premiers, pour chaque
    !
    DO JN=1,MIN(KL-1,KNEAR_NBR)
      !
      ID0 = MAXVAL(MINLOC(ZDIS(:)))       
      !
      KNEAR(NNUM(JP1),JN) = ID0
      ZDIS(ID0) = ZDMAX
      !
    ENDDO
    !
  ENDIF
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_LONLATVAL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_NEAR_MESHES_LONLATVAL
