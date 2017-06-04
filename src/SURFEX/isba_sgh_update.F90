!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################################
      SUBROUTINE ISBA_SGH_UPDATE (PMESH_SIZE, IO, S, K, NK, NP, NPE, PRAIN )
!     #######################################################################
!
!!****  *SGH_UPDATE*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the evolution of the fraction, mu, of the grid cell
!     reached by the rain, the Topmodel saturated fraction and the diagnostic
!     wetland fraction.
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
!!      07/2011 (B. Decharme) : Add fsat diag for dt92
!!      (B. Decharme) 04/2013 : DIF lateral subsurface drainage
!!
!-------------------------------------------------------------------------------
!
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NK_t, ISBA_NP_t, ISBA_NPE_t, &
                        ISBA_P_t, ISBA_PE_t
!
USE MODD_SGH_PAR,     ONLY : NDIMTAB, XMTOKM, XSTOHR, X001,   &
                             XMUREGP, XMUREGA
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN) :: PMESH_SIZE
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE

!
REAL, DIMENSION(:), INTENT(IN)   :: PRAIN
!                                   PRAIN   = rain rate (kg/m2/s)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PRAIN))          :: ZDIST, ZBETA, ZFSAT    ! IO%CRAIN = SGH
!                                        ZDIST  = the cell scale (in km)
!                                        ZBETA  = cell scale dependency parameter
!
REAL, DIMENSION(SIZE(PRAIN))          :: ZD_TOP, ZW_TOP, ZQTOP  ! IO%CRUNOFF = SGH
!                                        ZW_TOP = ative TOPMODEL-soil moisture at 't' (m3 m-3)
!                                        ZD_TOP = Topmodel active layer
!                                        ZQTOP  = Topmodel lateral sub-surface flow (-)
!
INTEGER, DIMENSION(SIZE(PRAIN))       :: IUP,IDOWN  ! IO%CRUNOFF = SGH
!                                        change in xsat (or fsat) index
!
INTEGER, DIMENSION(SIZE(PRAIN))       :: NMASK      ! indices correspondance between arrays
!
REAL, DIMENSION(SIZE(PRAIN))          :: ZWSAT_AVG, ZWWILT_AVG
!                                        Average soil properties content
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_PE_t), POINTER :: PEK
!
REAL                                  :: ZW_UP, ZW_DOWN
REAL                                  :: ZF_UP, ZF_DOWN, ZSLOPEF
REAL                                  :: ZQ_UP, ZQ_DOWN, ZSLOPEQ
!
INTEGER                               :: INJ, JJ, JI, JP, JTAB, ICOUNT, &
                                         JL, IMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SGH_UPDATE',0,ZHOOK_HANDLE)
!
INJ=SIZE(PRAIN,1)
!
ZFSAT(:) = 0.0
!
!*   1.0 Spatial distribution of precipitation
!    ---------------------------------------------
!
IF(IO%CRAIN=='SGH')THEN
!
  WHERE(PRAIN(:)>0.0)
    K%XMUF (:) =1.0
  ELSEWHERE
    K%XMUF (:) =0.0
  ENDWHERE

!        
! calculate the cell scale (in km)
!
  ZDIST(:) = SQRT(PMESH_SIZE(:))/XMTOKM
!
  WHERE(ZDIST(:)>=15.0)
!
!       calculate beta for the mu calculation
!         
    ZBETA (:) = XMUREGA + XMUREGP * EXP(-X001*ZDIST(:))
!
!       calculate mu, precip is in mm/hr
!   
    K%XMUF (:) = 1.0 - EXP(-ZBETA(:)*(PRAIN(:)*XSTOHR))
!
  ENDWHERE
!
    DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      KK => NK%AL(JP)
      IF (PK%NSIZE_P>0 )THEN
        DO JI = 1,PK%NSIZE_P
          IMASK = PK%NR_P(JI)
          KK%XMUF(JI) = K%XMUF(IMASK)
        ENDDO
      ENDIF
    ENDDO

ENDIF
!
!*   2.0 Computation of the saturated fraction given by TOPMODEL 
!    -----------------------------------------------------------
!
IF(IO%CRUNOFF=='SGH')THEN
!
! Calculation of the ative TOPMODEL-soil moisture at 't' (m)
! ---------------------------------------------------------------
!
  ZD_TOP    (:) = 0.0
  ZWSAT_AVG (:) = 0.0
  ZWWILT_AVG(:) = 0.0
  ZW_TOP    (:) = 0.0
!
  IF(IO%CISBA=='DIF')THEN        
!
    DO JP = 1,IO%NPATCH
      KK => NK%AL(JP)
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)

      IF (PK%NSIZE_P>0 )THEN

        DO JL = 1,IO%NLAYER_DUN
          DO JI = 1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            !
            ZD_TOP    (IMASK) = ZD_TOP    (IMASK) + PK%XPATCH(JI)*PK%XSOILWGHT(JI,JL)
            ZWSAT_AVG (IMASK) = ZWSAT_AVG (IMASK) + PK%XPATCH(JI)*PK%XSOILWGHT(JI,JL)*K%XWSAT(IMASK,JL)
            ZWWILT_AVG(IMASK) = ZWWILT_AVG(IMASK) + PK%XPATCH(JI)*PK%XSOILWGHT(JI,JL)*K%XWD0 (IMASK,JL)
            ZW_TOP    (IMASK) = ZW_TOP    (IMASK) + PK%XPATCH(JI)*PK%XSOILWGHT(JI,JL)*PEK%XWG (JI,JL)
            !
          ENDDO
        ENDDO
      ENDIF
    ENDDO
!
    WHERE(ZD_TOP(:)>0.0)
         ZWSAT_AVG (:) = ZWSAT_AVG (:)/ZD_TOP(:)
         ZWWILT_AVG(:) = ZWWILT_AVG(:)/ZD_TOP(:)
         ZW_TOP    (:) = ZW_TOP    (:)/ZD_TOP(:)
    ENDWHERE
!
  ELSE
!    
    DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)

      IF (PK%NSIZE_P>0 )THEN
        DO JI = 1,PK%NSIZE_P
          IMASK = PK%NR_P(JI)
          !          
          ZD_TOP(IMASK) = ZD_TOP(IMASK)+PK%XRUNOFFD(JI)*PK%XPATCH(JI)
          ZW_TOP(IMASK) = ZW_TOP(IMASK)+PK%XRUNOFFD(JI)*PK%XPATCH(JI)*PEK%XWG(JI,2)
          !
        ENDDO
      ENDIF
    ENDDO
!  
    WHERE(ZD_TOP(:)>0.0)
      ZW_TOP(:) = ZW_TOP(:) / ZD_TOP(:)
    ENDWHERE
!      
    ZWSAT_AVG (:) = K%XWSAT(:,1)
    ZWWILT_AVG(:) = K%XWD0 (:,1)
!
  ENDIF
!
! Find the boundary
! -----------------
!
  NMASK(:)=0
  ICOUNT=0
  DO JI=1,INJ  
     IF((S%XTI_MEAN(JI)/=XUNDEF.AND.ZW_TOP(JI)<ZWSAT_AVG(JI).AND.ZW_TOP(JI)>ZWWILT_AVG(JI))) THEN
       ICOUNT=ICOUNT+1
       NMASK(ICOUNT)=JI       
     ENDIF
     IF(ZW_TOP(JI)>=ZWSAT_AVG(JI))THEN
        ZFSAT (JI) = 1.0
     ENDIF
  ENDDO
!     
! compare wt_array and WT
! -----------------------
!
  DO JTAB=1,NDIMTAB
     DO JJ=1,ICOUNT
        JI = NMASK(JJ)    
        IF(S%XTAB_WTOP(JI,JTAB)>ZW_TOP(JI))THEN
          IUP(JJ)=JTAB
          IDOWN(JJ)=JTAB+1
        ELSEIF(S%XTAB_WTOP(JI,JTAB)==ZW_TOP(JI))THEN
          IUP(JJ)=JTAB
          IDOWN(JJ)=JTAB
        ENDIF
     ENDDO    
  ENDDO 
!    
! calculate fsat
! --------------
!   
  ZQTOP     (:) = 0.0
!
  DO JJ=1,ICOUNT
!  
     JI = NMASK(JJ)
!     
!    new range
     ZF_UP   = S%XTAB_FSAT(JI,IUP  (JJ))
     ZF_DOWN = S%XTAB_FSAT(JI,IDOWN(JJ))
     ZQ_UP   = S%XTAB_QTOP(JI,IUP  (JJ))
     ZQ_DOWN = S%XTAB_QTOP(JI,IDOWN(JJ))     
     ZW_UP   = S%XTAB_WTOP(JI,IUP  (JJ))
     ZW_DOWN = S%XTAB_WTOP(JI,IDOWN(JJ))
!     
!    Calculate new FSAT
     ZSLOPEF = 0.0
     ZSLOPEQ = 0.0
     IF(IUP(JJ)/=IDOWN(JJ))THEN
       ZSLOPEF = (ZF_UP-ZF_DOWN)/(ZW_UP-ZW_DOWN)
       ZSLOPEQ = (ZQ_UP-ZQ_DOWN)/(ZW_UP-ZW_DOWN)
     ENDIF
!     
     ZFSAT(JI) = ZF_DOWN+(ZW_TOP(JI)-ZW_DOWN)*ZSLOPEF
     ZQTOP(JI) = ZQ_DOWN+(ZW_TOP(JI)-ZW_DOWN)*ZSLOPEQ
!     
  ENDDO
!
  DO JP=1,IO%NPATCH
    KK => NK%AL(JP)
    PK => NP%AL(JP)
    IF(PK%NSIZE_P>0)THEN
      DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        KK%XFSAT(JI) = ZFSAT(IMASK)
      ENDDO
    ENDIF
  ENDDO
!
! Subsurface flow by layer (m/s)
! ------------------------------
!
  IF(IO%CISBA=='DIF')THEN        
!
    DO JP=1,IO%NPATCH
      KK => NK%AL(JP)
      PK => NP%AL(JP)
      IF(PK%NSIZE_P>0)THEN
        DO JL=1,IO%NLAYER_DUN
          DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            PK%XTOPQS(JI,JL) = K%XKANISO(IMASK,JL) * PK%XCONDSAT(JI,1) * ZQTOP(IMASK) * &
                                       PK%XSOILWGHT(JI,JL) / PK%XRUNOFFD(JI)
           ENDDO
        ENDDO
      ENDIF
    ENDDO
!
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ISBA_SGH_UPDATE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------
!
END SUBROUTINE ISBA_SGH_UPDATE
