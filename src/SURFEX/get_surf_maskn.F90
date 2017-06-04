!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################################################
      SUBROUTINE GET_SURF_MASK_n (DTCO, U, &
                                  HTYPE,KDIM,KMASK,KLU,KLUOUT)
!     #####################################################
!
!!****  *GET_SURF_MASK_n* - routine to define the masks between all surface 
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
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURf_PAR,   ONLY : NUNDEF
!
USE MODI_CONVERT_COVER_FRAC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_1D_MASK
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
INTEGER, INTENT(IN)              :: KDIM     ! dimension of mask
INTEGER, DIMENSION(KDIM), INTENT(OUT) :: KMASK    ! mask for reading of the files
INTEGER, INTENT(INOUT)           :: KLU      ! expected physical size of full surface array
INTEGER, INTENT(IN)              :: KLUOUT   ! output listing logical unit 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: ILU    ! total horizontal size
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.     Dimension initializations:
!               -------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_MASK_N',0,ZHOOK_HANDLE)
IF (.NOT. ASSOCIATED(U%XCOVER).AND..NOT.ASSOCIATED(U%XSEA)) THEN
  CALL GET_MASK(KLU,'FULL',KMASK)
  IF (LHOOK) CALL DR_HOOK('GET_SURF_MASK_N',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
!*        2.    Fractions
!              ---------
!
IF (ASSOCIATED(U%XCOVER)) THEN
  ILU = SIZE(U%XCOVER,1)
ELSEIF (ASSOCIATED(U%XSEA)) THEN
  ILU = SIZE(U%XSEA)
ENDIF
!
IF (KLU==NUNDEF .OR. KLU==0) KLU = ILU
!
IF (ILU/=KLU) THEN
  WRITE(KLUOUT,*) 'Error in initialization of masks for reading or writing'
  WRITE(KLUOUT,*) 'size expected for reading/writing from atmosphere files : ',KLU
  WRITE(KLUOUT,*) 'size of surface array in module MODD_SURF_ATM           : ',ILU
  CALL ABOR1_SFX('GET_SURF_MASK: ERROR IN INITIALIZATION OF MASK FOR READING OR WRITING')
END IF
!
 CALL GET_MASK(ILU,HTYPE,KMASK)
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_MASK_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE GET_MASK(KLU,YTYPE,IMASK)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KLU
 CHARACTER(LEN=*),  INTENT(IN)  :: YTYPE    ! Type of surface
INTEGER, DIMENSION(:), INTENT(OUT) :: IMASK
!
REAL, DIMENSION(KLU) :: ZSEA   ! sea cover
REAL, DIMENSION(KLU) :: ZNATURE! nature cover
REAL, DIMENSION(KLU) :: ZTOWN  ! town cover
REAL, DIMENSION(KLU) :: ZWATER ! water cover
REAL, DIMENSION(KLU) :: ZLAND  ! land cover
REAL, DIMENSION(KLU) :: ZSURF
INTEGER :: ILU2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_MASK_N:GET_MASK',0,ZHOOK_HANDLE)
!
IF (YTYPE.NE.'FULL' .AND. YTYPE.NE.'EXTZON') THEN
  !      
  IF (.NOT. ASSOCIATED(U%XSEA)) THEN
    CALL CONVERT_COVER_FRAC(DTCO, &
                            U%XCOVER,U%LCOVER,ZSEA,ZNATURE,ZTOWN,ZWATER)
  ELSE
    ZSEA    = U%XSEA
    ZNATURE = U%XNATURE
    ZWATER  = U%XWATER
    ZTOWN   = U%XTOWN
  END IF
  ZLAND =  ZNATURE + ZTOWN
  !
  SELECT CASE (YTYPE)
    CASE ('NATURE')
      ZSURF = ZNATURE
    CASE ('SEA')
      ZSURF = ZSEA
    CASE ('TOWN')
      ZSURF = ZTOWN
    CASE ('WATER')
      ZSURF = ZWATER
    CASE ('LAND')
      ZSURF = ZLAND
  END SELECT
  !
  ILU2 = COUNT(ZSURF(:) > 0.)
  !
ELSE
  !
  ZSURF(:) = 1.
  ILU2 = KLU
  !
ENDIF
!
 CALL GET_1D_MASK(ILU2,KLU,ZSURF,IMASK)
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_MASK_N:GET_MASK',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_MASK
!
END SUBROUTINE GET_SURF_MASK_n
