!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
!     ##########################
      SUBROUTINE DG_DFTO3L (IO, NP, PDG)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!      from  AVERAGE_DIAG_MISC_ISBA_n
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    REFERENCE
!!    ---------
!!     
!!    AUTHOR
!!    ------
!!
!!       ELYAZIDI/HEYMES/RISTOR * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original  02/2011 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_NP_t, ISBA_P_t
!
USE MODD_SURF_PAR,  ONLY : XUNDEF, NUNDEF
USE YOMHOOK   ,     ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,     ONLY : JPRB
!
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
!
 REAL, DIMENSION(:,:), INTENT(OUT) :: PDG
!      
!*      0.2    declarations of local variables
TYPE(ISBA_P_t), POINTER :: PK
 INTEGER                         :: JI, JL ! loop indexes
 INTEGER                         :: IDEPTH 
 INTEGER                         :: IMASK, JP
 REAL                            :: ZWORK 
 !
 REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DG_DFTO3L',0,ZHOOK_HANDLE)
!
PDG(:,:)=0.0
!
DO JP=1,IO%NPATCH
  !
  PK => NP%AL(JP)
  !
  IF (PK%NSIZE_P == 0 ) CYCLE
  !
  DO JL = 1,IO%NGROUND_LAYER
    !
    DO JI=1,PK%NSIZE_P
      !
      IMASK = PK%NR_P(JI)
      !
      IDEPTH=PK%NWG_LAYER(JI)
      !
      IF(JL<=IDEPTH.AND.IDEPTH/=NUNDEF.AND.PK%XPATCH(JI)/=XUNDEF)THEN
        !
        PDG(IMASK,1) = PDG(IMASK,1) + PK%XDG(JI,1) * PK%XPATCH(JI) 
        ! ISBA-FR-DG2 comparable soil wetness index, liquid water and ice contents
        ZWORK = MIN(PK%XDZG(JI,JL),MAX(0.0,PK%XDG2(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))
        PDG(IMASK,2) = PDG(IMASK,2) + ZWORK * PK%XPATCH(JI) 
        !
        ! ISBA-FR-DG3 comparable soil wetness index, liquid water and ice contents
        ZWORK=MIN(PK%XDZG(JI,JL),MAX(0.0,PK%XDG(JI,JL)-PK%XDG2(JI)))
        PDG(IMASK,3) = PDG(IMASK,3) + ZWORK * PK%XPATCH(JI) 
        !
      ENDIF
    ENDDO
  ENDDO
  !
ENDDO
! 
PDG (:,3) =  PDG (:,2) + PDG (:,3)
WHERE (PDG(:,:)==0.0)
  PDG(:,:)=XUNDEF
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('DG_DFTO3L',1,ZHOOK_HANDLE)

END SUBROUTINE DG_DFTO3L


