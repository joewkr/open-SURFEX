!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UNPACK_DIAG_PATCH_n(IO, DEK, PK, KMASK, KSIZE, KNPATCH, KPATCH, &
                               PCPL_DRAIN, PCPL_RUNOFF, PCPL_EFLOOD,       &
                               PCPL_PFLOOD, PCPL_IFLOOD, PCPL_ICEFLUX )  
!##############################################
!
!!****  *UNPACK_DIAG_PATCH_n* - unpacks ISBA diagnostics
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    10/2004 by P. Le Moigne: Halstead coefficient
!!      Modified    10/2005 by P. Le Moigne: Deallocation (EBA)
!!      Modified    05/2008 by B. Decharme : Flooding scheme
!!      Modified    01/2010 by B. Decharme : new diag
!!      Modified      04-09 by A.L. Gibelin : Add carbon diagnostics
!!      Modified      05-09 by A.L. Gibelin : Add carbon spinup
!!      Modified    08/2012 by B. Decharme : optimization
!!      Modified    06/2013 by B. Decharme : add lateral drainage flux diag for DIF
!!                                           add tiotale sublimation flux
!!      Modified    10/2014 by P. Samuelsson: MEB
!!
!!------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_P_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
!
INTEGER, INTENT(IN)                :: KSIZE, KPATCH, KNPATCH
INTEGER, DIMENSION(:), INTENT(IN)  :: KMASK
!
!Coupling variable
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_DRAIN
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_RUNOFF
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_EFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_PFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_IFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_ICEFLUX
!
INTEGER :: JI, IMASK, JSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UNPACK_DIAG_PATCH_N',0,ZHOOK_HANDLE)
!
IF (KNPATCH==1) THEN
  !
  IF (IO%LCPL_RRM) THEN
    PCPL_DRAIN (:,KPATCH) = DEK%XDRAIN (:)
    PCPL_RUNOFF(:,KPATCH) = DEK%XRUNOFF(:)
  END IF
  !
  IF (IO%LFLOOD) THEN
    PCPL_EFLOOD(:,KPATCH) = DEK%XLE_FLOOD(:) / PK%XLVTT(:) + DEK%XLEI_FLOOD(:) / PK%XLSTT(:)
    PCPL_PFLOOD(:,KPATCH) = DEK%XPFLOOD(:)
    PCPL_IFLOOD(:,KPATCH) = DEK%XIFLOOD(:)
  END IF    
  !
  IF(IO%LCPL_RRM.AND.IO%LGLACIER)THEN
    PCPL_ICEFLUX(:,KPATCH) = DEK%XICEFLUX(:)
  ENDIF
  !
ELSE
  !
  IF (IO%LCPL_RRM) THEN
    DO JI=1,KSIZE
      IMASK = KMASK(JI)
      PCPL_DRAIN (IMASK,KPATCH) = DEK%XDRAIN (JI)
      PCPL_RUNOFF(IMASK,KPATCH) = DEK%XRUNOFF (JI)
    END DO
  END IF
  !
  IF (IO%LFLOOD) THEN
    DO JI=1,KSIZE
      IMASK = KMASK(JI)
      PCPL_EFLOOD(IMASK,KPATCH) = DEK%XLE_FLOOD(JI) / PK%XLVTT(JI) + DEK%XLEI_FLOOD(JI) / PK%XLSTT(JI)
      PCPL_PFLOOD(IMASK,KPATCH) = DEK%XPFLOOD(JI)
      PCPL_IFLOOD(IMASK,KPATCH) = DEK%XIFLOOD(JI)
    END DO
  END IF
  !
  IF(IO%LCPL_RRM.AND.IO%LGLACIER)THEN
    DO JI=1,KSIZE
      IMASK  = KMASK(JI)
      PCPL_ICEFLUX(IMASK,KPATCH) = DEK%XICEFLUX(JI)
    END DO          
  ENDIF
  !
ENDIF
!
!------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UNPACK_DIAG_PATCH_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_DIAG_PATCH_n
