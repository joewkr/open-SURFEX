!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_SURF_SIZE_n (DTCO, U, &
                                  HTYPE,KL)
!     #####################################################
!
!!****  *GET_SURF_SIZE_n* - routine to define the masks between all surface 
!!    points and each of the four surface types
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    -------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
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
!
USE MODI_CONVERT_COVER_FRAC
!
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
!
 CHARACTER(LEN=*),  INTENT(IN)    :: HTYPE    ! Type of surface
INTEGER,           INTENT(OUT)   :: KL       ! number of points of this surface type
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZSEA   ! sea cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZNATURE! nature cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZTOWN  ! town cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZWATER ! water cover
!
INTEGER           :: ILU    ! total horizontal size
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*        1.    Fractions
!              ---------
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_SIZE_N',0,ZHOOK_HANDLE)
ILU = SIZE(U%XCOVER,1)
!
ALLOCATE(ZSEA   (ILU))
ALLOCATE(ZNATURE(ILU))
ALLOCATE(ZTOWN  (ILU))
ALLOCATE(ZWATER (ILU))
IF (.NOT.ASSOCIATED(U%XSEA)) THEN
  CALL CONVERT_COVER_FRAC(DTCO, &
                          U%XCOVER,U%LCOVER,ZSEA,ZNATURE,ZTOWN,ZWATER)
ELSE
  ZSEA    = U%XSEA
  ZNATURE = U%XNATURE
  ZWATER  = U%XWATER
  ZTOWN   = U%XTOWN
END IF
!
SELECT CASE (HTYPE)
  CASE ('FULL')
   KL = ILU
   !
  CASE ('NATURE')
   KL = COUNT(ZNATURE(:) > 0.)
   !
  CASE ('SEA')
   KL = COUNT(ZSEA(:) > 0.)
   !
  CASE ('TOWN')
   KL = COUNT(ZTOWN(:) > 0.)
   !
  CASE ('WATER')
   !       
   KL = COUNT(ZWATER(:) > 0.)
   !
  CASE ('LAND')
   !
   KL = COUNT(ZNATURE(:) + ZTOWN(:) > 0.)
   !
END SELECT
!-------------------------------------------------------------------------------
DEALLOCATE(ZSEA   )
DEALLOCATE(ZNATURE)
DEALLOCATE(ZTOWN  )
DEALLOCATE(ZWATER )
IF (LHOOK) CALL DR_HOOK('GET_SURF_SIZE_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_SURF_SIZE_n
