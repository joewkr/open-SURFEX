!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################################
      SUBROUTINE GET_TYPE_DIM_n (DTCO, U, &
                                 HTYPE,KDIM)
!     #####################################
!
!!****  *GET_TYPE_DIM_n* - routine to get the number of point for any surface type
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
 CHARACTER(LEN=6),  INTENT(IN)      :: HTYPE    ! Type of surface
INTEGER,           INTENT(INOUT)   :: KDIM     ! size of the mask
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZSEA   ! sea cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZNATURE! nature cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZTOWN  ! town cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZWATER ! water cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZLAND  ! land cover
REAL, DIMENSION(:),   ALLOCATABLE :: ZFULL  ! total cover
!
INTEGER           :: ILU    ! total horizontal size
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_TYPE_DIM_N',0,ZHOOK_HANDLE)
!
IF (.NOT. ASSOCIATED(U%XCOVER).AND..NOT.ASSOCIATED(U%XSEA)) THEN
  IF (LHOOK) CALL DR_HOOK('GET_TYPE_DIM_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!*        1.    Fractions
!              ---------
!
IF (ASSOCIATED(U%XCOVER)) THEN
  ILU = SIZE(U%XCOVER,1)
ELSEIF (ASSOCIATED(U%XSEA)) THEN
  ILU = SIZE(U%XSEA)
ENDIF
!
ALLOCATE(ZSEA   (ILU))
ALLOCATE(ZNATURE(ILU))
ALLOCATE(ZTOWN  (ILU))
ALLOCATE(ZWATER (ILU))
ALLOCATE(ZLAND (ILU))
IF (.NOT. ASSOCIATED(U%XSEA)) THEN
  CALL CONVERT_COVER_FRAC(DTCO, &
                          U%XCOVER,U%LCOVER,ZSEA,ZNATURE,ZTOWN,ZWATER)
ELSE
  ZSEA    = U%XSEA
  ZNATURE = U%XNATURE
  ZWATER  = U%XWATER
  ZTOWN   = U%XTOWN
END IF
ZLAND = ZTOWN + ZNATURE
!
ALLOCATE(ZFULL(ILU))
ZFULL=1.
!
SELECT CASE (HTYPE)
  CASE ('FULL  ')
   KDIM = ILU
   !
  CASE ('EXTZON')
   KDIM = ILU
   !
  CASE ('NATURE')
   KDIM = COUNT(ZNATURE(:) > 0.)
   !
  CASE ('SEA   ')
   KDIM = COUNT(ZSEA(:) > 0.)
   !
  CASE ('TOWN  ')
   KDIM = COUNT(ZTOWN(:) > 0.)
   !
  CASE ('WATER ')
   KDIM = COUNT(ZWATER(:) > 0.)
   !
  CASE ('LAND  ')
   KDIM = COUNT(ZLAND(:) > 0.)
   !
END SELECT
!-------------------------------------------------------------------------------
DEALLOCATE(ZSEA   )
DEALLOCATE(ZNATURE)
DEALLOCATE(ZTOWN  )
DEALLOCATE(ZWATER )
DEALLOCATE(ZFULL  )
DEALLOCATE(ZLAND  )
IF (LHOOK) CALL DR_HOOK('GET_TYPE_DIM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_TYPE_DIM_n
