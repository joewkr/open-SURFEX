!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
!     ##########################
      SUBROUTINE AVG_PATCH_WG (IO, NP, NPE, PWG, PWGI, PDG)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!      from  AVERAGE_DIAG_MISC_ISBA_n
!!     ONLY for 3L cases!!
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
USE MODD_ISBA_n, ONLY : ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
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
TYPE(ISBA_OPTIONS_t), INTENT(IN) :: IO
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
 REAL, DIMENSION(:,:), INTENT(OUT) :: PWG
 REAL, DIMENSION(:,:), INTENT(OUT) :: PWGI
 REAL, DIMENSION(:,:), INTENT(OUT) :: PDG
!      
!*      0.2    declarations of local variables
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
 INTEGER                         :: JI, JP ! loop indexes
 INTEGER                         :: IMASK
 REAL                            :: ZWORK 
REAL, DIMENSION(SIZE(PWG,1)) :: ZSUMPATCH
 !
 REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('AVG_PATCH_WG',0,ZHOOK_HANDLE)
!
ZSUMPATCH(:) = 0.0
DO JP=1,IO%NPATCH
  DO JI=1,NP%AL(JP)%NSIZE_P
    IMASK = NP%AL(JP)%NR_P(JI)
    ZSUMPATCH(IMASK) = ZSUMPATCH(IMASK) + NP%AL(JP)%XPATCH(JI)
  END DO
END DO

PWG (:,:) =0.0
PWGI(:,:)=0.0
PDG (:,:) =0.0
!
! 
IF (IO%NPATCH/=1)THEN
  DO JP=1,IO%NPATCH
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI) 
      IF(ZSUMPATCH(IMASK) > 0.)THEN
        !
        ZWORK=MAX(0.0,PK%XDG(JI,3)-PK%XDG(JI,2))
        PWG(IMASK,1)  = PWG(IMASK,1)  + PK%XPATCH(JI) * PEK%XWG(JI,1)  * PK%XDG (JI,1) 
        PWG(IMASK,2)  = PWG(IMASK,2)  + PK%XPATCH(JI) * PEK%XWG(JI,2)  * PK%XDG (JI,2) 
        PWG(IMASK,3)  = PWG(IMASK,3)  + PK%XPATCH(JI) * PEK%XWG(JI,3)  * ZWORK
        PWGI(IMASK,1) = PWGI(IMASK,1) + PK%XPATCH(JI) * PEK%XWGI(JI,1) * PK%XDG (JI,1) 
        PWGI(IMASK,2) = PWGI(IMASK,2) + PK%XPATCH(JI) * PEK%XWGI(JI,2) * PK%XDG (JI,2) 
        PWGI(IMASK,3) = PWGI(IMASK,3) + PK%XPATCH(JI) * PEK%XWGI(JI,3) * ZWORK
        ! 
        PDG(IMASK,1) = PDG(IMASK,1) + PK%XPATCH(JI) * PK%XDG(JI,1)
        PDG(IMASK,2) = PDG(IMASK,2) + PK%XPATCH(JI) * PK%XDG(JI,2)
        PDG(IMASK,3) = PDG(IMASK,3) + PK%XPATCH(JI) * PK%XDG(JI,3)
        !          
      ENDIF
    ENDDO
  ENDDO     
  !     
  WHERE (PDG(:,1)>0.0)
    PWG(:,1)  = PWG(:,1)  / PDG(:,1)
    PWGI(:,1) = PWGI(:,1) / PDG(:,1)
  ENDWHERE
  WHERE (PDG(:,2)>0.0)
    PWG(:,2)  = PWG(:,2)  / PDG(:,2)
    PWGI(:,2) = PWGI(:,2) / PDG(:,2)
  ENDWHERE
  WHERE (PDG(:,3)-PDG(:,2)>0.0)
    PWG(:,3)  = PWG(:,3)  / (PDG(:,3)-PDG(:,2))
    PWGI(:,3) = PWGI(:,3) / (PDG(:,3)-PDG(:,2))
  ENDWHERE
ELSE

  DO JP=1,IO%NPATCH
    DO JI=1,NP%AL(JP)%NSIZE_P
       IMASK = NP%AL(JP)%NR_P(JI)         
       PWG (IMASK,:) = NPE%AL(1)%XWG (JI,:)
       PWGI(IMASK,:) = NPE%AL(1)%XWGI(JI,:)
       PDG (IMASK,:) = NP%AL (1)%XDG (JI,:)
     ENDDO
  ENDDO

ENDIF 
!

IF (LHOOK) CALL DR_HOOK('AVG_PATCH_WG',1,ZHOOK_HANDLE)

END SUBROUTINE AVG_PATCH_WG


