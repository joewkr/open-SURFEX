!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GRID_MODIF(U,KLUOUT,KLUNAM,HGRID,KGRID_PAR,PGRID_PAR,KL)
!     ##########################################################
!!
!!    PURPOSE
!!    -------
!!   Reads in namelist the grid type and parameters.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/2004
!!    J.Escobar    09/02/05 bug init IGRID_PAR
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GRID_MODIF_CARTESIAN
!
USE MODI_GRID_MODIF_CONF_PROJ
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
INTEGER,           INTENT(IN)   :: KLUOUT     ! output listing logical unit
INTEGER,           INTENT(IN)   :: KLUNAM     ! namelist file logical unit
 CHARACTER(LEN=10), INTENT(IN)   :: HGRID      ! type of horizontal grid
INTEGER,           INTENT(INOUT):: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
INTEGER,           INTENT(INOUT):: KL         ! number of points
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                         :: IGRID_PAR ! modified grid vector size
INTEGER                         :: IL        ! number of points in modified grid
REAL, DIMENSION(:), ALLOCATABLE :: ZGRID_PAR ! modified grid vector
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GRID_MODIF',0,ZHOOK_HANDLE)
!
IF (HGRID=="NONE      ".OR.HGRID=="LONLAT REG".OR.HGRID=="GAUSS     ".OR.&
        HGRID=="LONLATVAL ") THEN
  IF (LHOOK) CALL DR_HOOK('GRID_MODIF',1,ZHOOK_HANDLE)
  RETURN
END IF
!
IGRID_PAR = 0
ALLOCATE(ZGRID_PAR(0))
 CALL GRID_MODIFICATION(KLUOUT,KLUNAM,HGRID,KGRID_PAR,KL,PGRID_PAR,IGRID_PAR,IL,.FALSE.,ZGRID_PAR)
DEALLOCATE(ZGRID_PAR)
!
ALLOCATE(ZGRID_PAR(IGRID_PAR))
 CALL GRID_MODIFICATION(KLUOUT,KLUNAM,HGRID,KGRID_PAR,KL,PGRID_PAR,IGRID_PAR,IL,.TRUE.,ZGRID_PAR)
!
DEALLOCATE(PGRID_PAR)
!
KGRID_PAR = IGRID_PAR
KL        = IL
ALLOCATE(PGRID_PAR(KGRID_PAR))
PGRID_PAR = ZGRID_PAR
!
DEALLOCATE(ZGRID_PAR)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GRID_MODIF',1,ZHOOK_HANDLE)
CONTAINS
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     ##########################################################
      SUBROUTINE GRID_MODIFICATION(KLUOUT,KLUNAM,HGRID,KGRID_PAR,KL,PGRID_PAR, &
                                     KGRID_PAR2,KL2,OMODIF,PGRID_PAR2            )  
!     ##########################################################
!!
!!    PURPOSE
!!    -------
!!   Modification of grid parameters
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/2004
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
INTEGER,                      INTENT(IN)    :: KLUOUT     ! output listing logical unit
INTEGER,                      INTENT(IN)    :: KLUNAM     ! namelist file logical unit
 CHARACTER(LEN=10),            INTENT(IN)    :: HGRID      ! type of horizontal grid
INTEGER,                      INTENT(IN)    :: KL         ! number of points
INTEGER,                      INTENT(IN)    :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(:),           INTENT(IN)    :: PGRID_PAR  ! parameters defining the grid
INTEGER,                      INTENT(INOUT) :: KL2        ! number of points in modified grid
INTEGER,                      INTENT(INOUT) :: KGRID_PAR2 ! size of PGRID_PAR2
LOGICAL,                      INTENT(IN)    :: OMODIF     ! flag to modify the grid
REAL, DIMENSION(:), OPTIONAL, INTENT(OUT)   :: PGRID_PAR2 ! parameters defining the modified grid
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                         :: IGRID_PAR2
REAL, DIMENSION(:), ALLOCATABLE :: ZGRID_PAR2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GRID_MODIFICATION',0,ZHOOK_HANDLE)
IF (OMODIF) THEN
  IGRID_PAR2 = KGRID_PAR2
ELSE
  IGRID_PAR2 = 0
END IF
!
ALLOCATE(ZGRID_PAR2(IGRID_PAR2))
!
SELECT CASE (HGRID)
  CASE ("CONF PROJ ")
    CALL GRID_MODIF_CONF_PROJ(U,KLUOUT,KLUNAM,KGRID_PAR,KL,PGRID_PAR, &
                                       KGRID_PAR2,KL2,OMODIF,ZGRID_PAR2      )  

  CASE ("CARTESIAN ")
    CALL GRID_MODIF_CARTESIAN(U,KLUOUT,KLUNAM,KGRID_PAR,KL,PGRID_PAR, &
                                       KGRID_PAR2,KL2,OMODIF,ZGRID_PAR2      )  

END SELECT
!
IF (OMODIF) PGRID_PAR2 = ZGRID_PAR2
!
DEALLOCATE(ZGRID_PAR2)
IF (LHOOK) CALL DR_HOOK('GRID_MODIFICATION',1,ZHOOK_HANDLE)
!
END SUBROUTINE GRID_MODIFICATION
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END SUBROUTINE GRID_MODIF
