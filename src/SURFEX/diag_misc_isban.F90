!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_MISC_ISBA_n (DMK, KK, PK, PEK, AGK, IO, OSURF_MISC_BUDGET, &
                             OVOLUMETRIC_SNOWLIQ, PTSTEP, OAGRIP, PTIME, KSIZE  )  
!     ###############################################################################
!
!!****  *DIAG_MISC-ISBA_n * - additional diagnostics for ISBA
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
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2004
!!      Modified    10/2004 by P. Le Moigne: Halstead coefficient
!!      B. Decharme   2008    Do not limit the SWI to 1
!!                            Add total SWI
!!      S. Lafont    03/2009 : change unit of carbon output in kg/m2/s
!!      A.L. Gibelin 04/2009 : Add respiration diagnostics
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!        S. Lafont  01/2011 : accumulate carbon variable between 2 outputs
!!       B. Decharme 05/2012 : Carbon fluxes in diag_evap
!!       B. Decharme 05/2012 : Active and frozen layers thickness for dif
!!       B. Decharme 06/2013 : Snow temp for EBA scheme (XP_SNOWTEMP not allocated)
!!
!!------------------------------------------------------------------
!
!
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_AGRI_n, ONLY : AGRI_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODD_CSTS,       ONLY : XTT, XRHOLW
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!                                     
USE MODD_TYPE_SNOW
!
USE MODI_COMPUT_COLD_LAYERS_THICK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
TYPE(AGRI_t), INTENT(INOUT) :: AGK
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
!
LOGICAL, INTENT(IN) :: OSURF_MISC_BUDGET
LOGICAL, INTENT(IN) :: OVOLUMETRIC_SNOWLIQ
REAL,    INTENT(IN) :: PTSTEP        ! timestep for  accumulated values 
LOGICAL, INTENT(IN) :: OAGRIP
REAL,    INTENT(IN) :: PTIME   ! current time since midnight
INTEGER, INTENT(IN) :: KSIZE
!
!    
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PEK%XPSN))    :: ZSNOWTEMP
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZWORK
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZWORKTEMP
!
REAL, DIMENSION(KSIZE) :: ZALT, ZFLT
!
LOGICAL :: GMASK
INTEGER :: JL, JI, JK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
!
IF (OSURF_MISC_BUDGET) THEN
  !
  DMK%XSWI (:,:)=XUNDEF
  DMK%XTSWI(:,:)=XUNDEF  
  DO JL=1,SIZE(PEK%XWG,2)
    DO JI=1,SIZE(PEK%XWG,1)
      IF(PEK%XWG (JI,JL)/=XUNDEF)THEN    
        DMK%XSWI (JI,JL) = (PEK%XWG (JI,JL) - KK%XWWILT(JI,JL)) / (KK%XWFC(JI,JL) - KK%XWWILT(JI,JL))
        DMK%XTSWI(JI,JL) = (PEK%XWG (JI,JL) - KK%XWWILT(JI,JL)) / (KK%XWFC(JI,JL) - KK%XWWILT(JI,JL))
      ENDIF
      IF(PEK%XWGI (JI,JL)/=XUNDEF)THEN    
        DMK%XTSWI(JI,JL) = DMK%XTSWI(JI,JL) +  PEK%XWGI(JI,JL) / (KK%XWFC(JI,JL) - KK%XWWILT(JI,JL))
      ENDIF
    ENDDO
  ENDDO
  !
  DO JL = 1,SIZE(PEK%TSNOW%WSNOW,2)
    DO JI = 1,SIZE(PEK%TSNOW%WSNOW,1)
      ZWORK(JI,JL)  = PEK%TSNOW%WSNOW(JI,JL) / PEK%TSNOW%RHO(JI,JL)
    ENDDO
  ENDDO
  !
  DMK%XTWSNOW=0.
  DMK%XTDSNOW=0.
  ZSNOWTEMP=0.  
  !
  IF (PEK%TSNOW%SCHEME/='EBA')THEN
     ZWORKTEMP(:,:) = DMK%XSNOWTEMP(:,:)
  ELSE
     ZWORKTEMP(:,1) = MIN(PEK%XTG(:,1),XTT)
  ENDIF
  !
  DO JL = 1,SIZE(PEK%TSNOW%WSNOW,2)
    DO JI = 1,SIZE(PEK%TSNOW%WSNOW,1)
      DMK%XTWSNOW(JI) = DMK%XTWSNOW(JI) + PEK%TSNOW%WSNOW(JI,JL)      
      DMK%XTDSNOW(JI) = DMK%XTDSNOW(JI) + ZWORK (JI,JL)
      ZSNOWTEMP  (JI) = ZSNOWTEMP(JI) + ZWORKTEMP(JI,JL) * ZWORK(JI,JL)
    ENDDO
  ENDDO
  !
  WHERE(DMK%XTDSNOW(:)>0.0)
        ZSNOWTEMP(:)=ZSNOWTEMP(:)/DMK%XTDSNOW(:)
  ELSEWHERE
        ZSNOWTEMP(:)=XUNDEF
  ENDWHERE
  !
  DMK%XPSNG  (:) = PEK%XPSNG(:)
  DMK%XPSNV  (:) = PEK%XPSNV(:)
  DMK%XPSN   (:) = PEK%XPSN (:)
  DMK%XFF    (:) = KK%XFF   (:)
  DMK%XFFG   (:) = KK%XFFG  (:)
  DMK%XFFV   (:) = KK%XFFV  (:)
  DMK%XFSAT  (:) = KK%XFSAT (:)
  DMK%XTTSNOW(:) = ZSNOWTEMP(:)
  !  
  IF ( (PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO') .AND. OVOLUMETRIC_SNOWLIQ ) THEN
    !
    WHERE (DMK%XSNOWLIQ(:,:)/=XUNDEF) &
                    DMK%XSNOWLIQ(:,:) = DMK%XSNOWLIQ(:,:) * XRHOLW / DMK%XSNOWDZ(:,:)
    !
  ENDIF
  !
  ! cosine of solar zenith angle 
  !
  IF (IO%CPHOTO/='NON'.AND.IO%LTR_ML) THEN
       !
       ! Mask where vegetation evolution is performed (just before solar midnight)
       GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
       IF (GMASK) THEN
         DO JI=1,KSIZE
           !
           IF (PEK%XMUS(JI).NE.0.) THEN
             DMK%XDFAPARC   (JI) = PEK%XFAPARC   (JI) / PEK%XMUS(JI) 
             DMK%XDFAPIRC   (JI) = PEK%XFAPIRC   (JI) / PEK%XMUS(JI)
             DMK%XDLAI_EFFC (JI) = PEK%XLAI_EFFC (JI) / PEK%XMUS(JI)
           ENDIF
           !
         ENDDO
         DO JI=1,KSIZE   
           PEK%XFAPARC(JI)   = 0.
           PEK%XFAPIRC(JI)   = 0.
           PEK%XLAI_EFFC(JI) = 0.
           PEK%XMUS(JI)      = 0.
         ENDDO
       ENDIF
       !
  ENDIF
  !
  IF(IO%CISBA=='DIF')THEN
    ZALT(:)=0.0
    ZFLT(:)=0.0
    CALL COMPUT_COLD_LAYERS_THICK(PK%XDG(:,:),PEK%XTG(:,:),ZALT,ZFLT)
    DO JI=1,KSIZE
      DMK%XALT(JI) =  ZALT(JI) 
      DMK%XFLT(JI) =  ZFLT(JI)  
    ENDDO
  ENDIF
  !
END IF
!
IF (OAGRIP) THEN
  !
  DO JI=1,KSIZE
    DMK%XSEUIL   (JI)  =  AGK%XTHRESHOLDSPT (JI)
  END DO
!
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_ISBA_n
