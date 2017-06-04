!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_CPL_ESM_ISBA (IO, S, NK, NP, PTSTEP, PCPL_DRAIN, PCPL_RUNOFF, &
                                    PCPL_EFLOOD,PCPL_PFLOOD,PCPL_IFLOOD,PCPL_ICEFLUX  )  
!     #####################################################################
!
!!****  *DIAG_CPL_ESM_ISBA*  
!!
!!    PURPOSE
!!    -------
!         
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
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      B. Decharme    01/16 : Bug with flood budget and add cpl keys
!!      B. Decharme   10/2016  bug surface/groundwater coupling
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_NK_t, ISBA_NP_t
!
USE MODN_SFX_OASIS, ONLY : XTSTEP_CPL_LAND
USE MODD_SFX_OASIS,  ONLY : LCPL_FLOOD, LCPL_GW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
!
REAL, INTENT(IN)                   :: PTSTEP
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_DRAIN
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_RUNOFF
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_EFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_PFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_IFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_ICEFLUX
!
!*      0.2    declarations of local variables
!
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_P_t), POINTER :: PK
!
REAL, DIMENSION(SIZE(PCPL_DRAIN,1),SIZE(PCPL_DRAIN,2)) :: ZCPL_DRAIN
!
REAL, DIMENSION(SIZE(S%XPATCH,1)) :: ZSUMPATCH
REAL, DIMENSION(SIZE(S%XPATCH,1)) :: ZBUDGET
!
INTEGER :: INJ, JP, IMASK
INTEGER :: JI ! tile loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_ISBA',0,ZHOOK_HANDLE)
!
!* Initialization
!  --------------
!
INJ=SIZE(S%XPATCH,1)
!
ZSUMPATCH(:) = 0.0
DO JP=1,IO%NPATCH
  DO JI=1,INJ
     ZSUMPATCH(JI) = ZSUMPATCH(JI) + S%XPATCH(JI,JP)
  ENDDO
ENDDO
!
IF(IO%CISBA/='DIF')THEN
! prevent small negatives values with ISBA-FR
  ZCPL_DRAIN(:,:)=MAX(0.0,PCPL_DRAIN(:,:))
ELSE
  ZCPL_DRAIN(:,:)=PCPL_DRAIN(:,:)
ENDIF
!
!* update ISBA - RRM coupling variable (kg/m2)
!  -------------------------------------------
!
!kg/m²
DO JP=1,IO%NPATCH
  DO JI=1,INJ
!  
     IF(ZSUMPATCH(JI)>0.0)THEN
       S%XCPL_DRAIN (JI) = S%XCPL_DRAIN (JI) + PTSTEP * ZCPL_DRAIN (JI,JP) * S%XPATCH(JI,JP)/ZSUMPATCH(JI) 
       S%XCPL_RUNOFF(JI) = S%XCPL_RUNOFF(JI) + PTSTEP * PCPL_RUNOFF(JI,JP) * S%XPATCH(JI,JP)/ZSUMPATCH(JI) 
     ENDIF
!
     IF(IO%LGLACIER.AND.ZSUMPATCH(JI)>0.0)THEN
        S%XCPL_ICEFLUX(JI) = S%XCPL_ICEFLUX(JI) + PTSTEP * PCPL_ICEFLUX(JI,JP) * S%XPATCH(JI,JP)/ZSUMPATCH(JI)
     ENDIF
!   
     IF(LCPL_FLOOD.AND.IO%LFLOOD.AND.ZSUMPATCH(JI)>0.0)THEN
        S%XCPL_EFLOOD  (JI) = S%XCPL_EFLOOD  (JI) + PTSTEP * PCPL_EFLOOD  (JI,JP)*S%XPATCH(JI,JP)/ZSUMPATCH(JI)
        S%XCPL_PFLOOD  (JI) = S%XCPL_PFLOOD  (JI) + PTSTEP * PCPL_PFLOOD  (JI,JP)*S%XPATCH(JI,JP)/ZSUMPATCH(JI)
        S%XCPL_IFLOOD  (JI) = S%XCPL_IFLOOD  (JI) + PTSTEP * PCPL_IFLOOD  (JI,JP)*S%XPATCH(JI,JP)/ZSUMPATCH(JI)
     ENDIF
!    
  ENDDO
ENDDO
!
!* update ISBA Floodplains variable for mass conservation (kg/m2)
!  --------------------------------------------------------------
!
IF(LCPL_FLOOD.AND.IO%LFLOOD)THEN
  ZBUDGET(:) = 0.0
  DO JP = 1,IO%NPATCH
    KK => NK%AL(JP)
    PK => NP%AL(JP)

    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZBUDGET(IMASK) = ZBUDGET(IMASK) + (KK%XPIFLOOD(JI)*XTSTEP_CPL_LAND) + &
                       (S%XCPL_PFLOOD(IMASK)-S%XCPL_IFLOOD(IMASK)-S%XCPL_EFLOOD(IMASK))
    ENDDO
  ENDDO

  DO JP = 1,IO%NPATCH
    KK => NK%AL(JP)
    PK => NP%AL(JP)
  
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      IF (ZBUDGET(IMASK)<=0.) THEN
        KK%XPIFLOOD(JI) = 0.0
        KK%XFFLOOD (JI) = 0.0
      ENDIF
    ENDDO
  ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_ISBA',1,ZHOOK_HANDLE)
!
END SUBROUTINE DIAG_CPL_ESM_ISBA
