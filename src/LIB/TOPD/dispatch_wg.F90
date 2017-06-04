!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
!     ##########################
      SUBROUTINE DISPATCH_WG (S, NP, NPE, PWG, PWGI, PDG)
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
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_NP_t, ISBA_NPE_t
!
USE MODD_SURF_PAR,  ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR,      ONLY : XWGMIN
USE MODD_COUPLING_TOPD, ONLY :  XATOP
!
USE YOMHOOK   ,     ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,     ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of argumentsXPATCH
!
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
 REAL, DIMENSION(:,:), INTENT(IN) :: PWG
 REAL, DIMENSION(:,:), INTENT(IN) :: PWGI
 REAL, DIMENSION(:,:), INTENT(IN) :: PDG
!      
!*      0.2    declarations of local variables
 INTEGER                         :: JJ, JLAYER, JPATCH ! loop indexes
 INTEGER                         :: IDEPTH 
 INTEGER                         :: INI, INP
 REAL                            :: ZWORK,ZTMP, ZWORK2
 REAL, DIMENSION(SIZE(S%XPATCH,1)) :: ZSUMPATCH
 REAL, DIMENSION(SIZE(S%XPATCH,1),SIZE(S%XPATCH,2)) :: ZFRAC_PATCH2
 REAL, DIMENSION(SIZE(S%XPATCH,1),SIZE(S%XPATCH,2)) :: ZFRAC_PATCH3
 REAL, DIMENSION(SIZE(PWG,1),SIZE(PWG,2)) :: ZWG_CTL
 !
 INTEGER :: IMASK, JP
 REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DISPATCH_WG',0,ZHOOK_HANDLE)
!
INI=SIZE(S%XPATCH,1)
INP=SIZE(S%XPATCH,2)
!
 !DO JPATCH=1,INP
 !
 ! write(*,*) 'In dispatch XPATCH (1)',JPATCH,XPATCH(1,JPATCH),XWG(1,2,JPATCH)
 !ENDDO
!write(*,*) 'In dispatch wg ,KI,INI,INP ',KI,INI,INP
IF (INP/=1)THEN
 DO JP=1,INP
   DO JJ=1,NP%AL(JP)%NSIZE_P
     IMASK = NP%AL(JP)%NR_P(JJ)
     IF ((S%XPATCH(IMASK,JP)/=XUNDEF).AND.(S%XPATCH(IMASK,JP)/=0.).AND.(XATOP(IMASK)==1.)) THEN
       WHERE (NPE%AL(JP)%XWG(JJ,:)/=XUNDEF)
         NPE%AL(JP)%XWG(JJ,:) = PWG(IMASK,:) 
         NPE%AL(JP)%XWGI(JJ,:)= PWGI(IMASK,:) 
         NP%AL(JP)%XDG(JJ,:)  = PDG (IMASK,:)
       ENDWHERE
     ENDIF
   ENDDO
 ENDDO

ELSE
 DO JP=1,INP
   DO JJ=1,NP%AL(JP)%NSIZE_P
     IMASK = NP%AL(JP)%NR_P(JJ)        
     NPE%AL(1)%XWG (JJ,:) = PWG(IMASK,:) 
     NPE%AL(1)%XWGI(JJ,:) = PWGI(IMASK,:) 
     NP%AL(1)%XDG(JJ,:)   = PDG (IMASK,:)
   ENDDO
 ENDDO
ENDIF
!
DO JP = 1,INP
  WHERE (NPE%AL(JP)%XWG(:,:)<XWGMIN) 
    NPE%AL(JP)%XWG(:,:)=XWGMIN
  ENDWHERE
ENDDO
 !     
IF (LHOOK) CALL DR_HOOK('DISPATCH_WG',1,ZHOOK_HANDLE)

END SUBROUTINE DISPATCH_WG


