!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################################
      SUBROUTINE GET_VEG_n(HPROGRAM, KI, U, IO, S, NP, NPE, PLAI, PVH)
!     #######################################################################
!
!!****  *GET_VEG_n* - gets some veg fields on atmospheric grid
!!
!!    PURPOSE
!!    -------
!!
!!    This program returns some veg variables needed by the atmosphere
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	P. Aumond	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_P_t, ISBA_PE_t, ISBA_NP_t, ISBA_NPE_t
!
USE MODD_SURF_PAR,         ONLY : XUNDEF
USE MODD_DATA_COVER_PAR

USE MODI_GET_LUOUT
USE MODI_VEGTYPE_TO_PATCH
!                                
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),   INTENT(IN)   :: HPROGRAM    
INTEGER,            INTENT(IN)   :: KI         ! number of points
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
REAL, DIMENSION(KI), INTENT(OUT) :: PVH    ! Tree height 
REAL, DIMENSION(KI), INTENT(OUT) :: PLAI   
!-------------------------------------------------------------------------------
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!  Arrays defined for each tile
!  
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
INTEGER                               :: JI,JJ           ! loop index over tiles
INTEGER                               :: ILUOUT       ! unit numberi
REAL, DIMENSION(U%NSIZE_FULL)      :: ZH_TREE_FULL, ZLAI_FULL
REAL, DIMENSION(U%NSIZE_NATURE)    :: ZH_TREE, ZLAI,ZWORK
INTEGER:: IPATCH_TRBE, IPATCH_TRBD, IPATCH_TEBE, IPATCH_TEBD, IPATCH_TENE, &
          IPATCH_BOBD, IPATCH_BONE, IPATCH_BOND, IMASK, JP
! 
!-------------------------------------------------------------------------------
!
!*   0. Logical unit for writing out
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*       1. Passage dur le masque global
!              -------------------------------


ZH_TREE_FULL(:) = 0.
ZLAI_FULL   (:) = XUNDEF

IPATCH_TRBE = VEGTYPE_TO_PATCH(NVT_TRBE, IO%NPATCH)
IPATCH_TRBD = VEGTYPE_TO_PATCH(NVT_TRBD, IO%NPATCH)
IPATCH_TEBE = VEGTYPE_TO_PATCH(NVT_TEBE, IO%NPATCH)
IPATCH_TEBD = VEGTYPE_TO_PATCH(NVT_TEBD, IO%NPATCH)
IPATCH_TENE = VEGTYPE_TO_PATCH(NVT_TENE, IO%NPATCH)
IPATCH_BOBD = VEGTYPE_TO_PATCH(NVT_BOBD, IO%NPATCH)
IPATCH_BONE = VEGTYPE_TO_PATCH(NVT_BONE, IO%NPATCH)
IPATCH_BOND = VEGTYPE_TO_PATCH(NVT_BOND, IO%NPATCH)


ZWORK(:) = S%XVEGTYPE(:,NVT_TRBE) + S%XVEGTYPE(:,NVT_TRBD) + S%XVEGTYPE(:,NVT_TEBE) + &
           S%XVEGTYPE(:,NVT_TEBD) + S%XVEGTYPE(:,NVT_TENE) + S%XVEGTYPE(:,NVT_BOBD) + &
           S%XVEGTYPE(:,NVT_BONE) + S%XVEGTYPE(:,NVT_BOND)

ZH_TREE(:) = 0.
ZLAI(:) = 0.
!
DO JP = 1,IO%NPATCH
  !
  IF (JP==IPATCH_TRBE .OR. JP==IPATCH_TRBD .OR. JP==IPATCH_TEBE .OR. JP==IPATCH_TEBD .OR. &
      JP==IPATCH_TENE .OR. JP==IPATCH_BOBD .OR. JP==IPATCH_BONE .OR. JP==IPATCH_BOND) THEN
    !
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    !
    DO JJ=1,PK%NSIZE_P
      !
      IMASK = PK%NR_P(JJ)
      !
      IF (S%XVEGTYPE(IMASK,JP)/=0) THEN
        !
        ZH_TREE(IMASK) = ZH_TREE(IMASK) + PK%XH_TREE(JJ) * PK%XPATCH(JJ)
        !
        ZLAI(IMASK)  = ZLAI(IMASK) + PEK%XLAI(JJ) * PK%XPATCH(JJ)
        !
      ENDIF
      !
    ENDDO
    !
  ENDIF
  !
ENDDO  
!
WHERE(ZWORK(:)/=0.) 
  ZH_TREE(:) = ZH_TREE(:)/ZWORK(:)
  ZLAI(:) = ZLAI(:)/ZWORK(:)
END WHERE
!
DO JJ = 1,U%NSIZE_NATURE
  ZH_TREE_FULL(U%NR_NATURE(JJ)) = ZH_TREE(JJ)
  ZLAI_FULL   (U%NR_NATURE(JJ)) = ZLAI(JJ)
END DO
!
ZLAI_FULL(:) = U%XNATURE(:) * ZLAI_FULL(:)
!
!*       2. Envoi les variables vers mesonH 
!             ------------------------------

IF ( SIZE(PVH) /= SIZE(ZH_TREE_FULL) ) THEN
  WRITE(ILUOUT,*) 'try to get VH field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PVH) :', SIZE(PVH)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XVH) :', SIZE(ZH_TREE_FULL)
  CALL ABOR1_SFX('GET_VHN: VH SIZE NOT CORRECT')
ELSE
  PVH = ZH_TREE_FULL
END IF
!
!==============================================================================
!
!-------------------------------------------------------------------------------
!
IF ( SIZE(PLAI) /= SIZE(ZLAI_FULL) ) THEN
  WRITE(ILUOUT,*) 'try to get LAI field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PLAI) :', SIZE(PLAI)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XLAI) :', SIZE(ZLAI_FULL)
  CALL ABOR1_SFX('GET_LAIN: LAI SIZE NOT CORRECT')
ELSE
  PLAI = ZLAI_FULL
END IF
!
!==============================================================================
!
!-------------------------------------------------------------------------------
!
!==============================================================================
!
END SUBROUTINE GET_VEG_n
