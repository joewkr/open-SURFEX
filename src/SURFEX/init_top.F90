!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      SUBROUTINE INIT_TOP (IO, S, K, NK, NP, KLUOUT, PM    )
!
!     #####################################################################
!
!!****  *INIT_TOP*  
!!
!!    PURPOSE
!!    =======
!
!     Calculates the new array of the Datin-Saulnier TOPMODEL framework fonction for xsat and compute each 
!     satured fraction for each xsat value of the grids cells but also the active TOPMODEL-layer array, the 
!     driest fraction array and the normalized mean deficit array.
!     For calculate new array, we use the incomplete gamma function. (see gamma_inc.f for more detail)
! 
!     Note that over land point where topographic index do not exist, a VIC
!     distribution is used with Bcoef at least equal to 0.1. This value can be
!     change in namelist
!
!!
!!    AUTHOR
!!    ------
!!     B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!       B. decharme 04/2013 : DIF lateral drainage
!                             CTI linear regression done in PGD (HTOPREG deleted)
!
!-------------------------------------------------------------------------------
!
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY: ISBA_S_t, ISBA_NP_t, ISBA_NK_t, ISBA_P_t, ISBA_K_t
!
USE MODD_SURF_PAR,ONLY : XUNDEF
!
USE MODD_SGH_PAR, ONLY : X2, X4, XREGP, XREGA
!
USE MODI_DGAM
USE MODI_GAMMAS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
!*       0.     DECLARATIONS
!        ===================
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
!
INTEGER, INTENT(IN)                  :: KLUOUT
!
REAL,    DIMENSION(:), INTENT(INOUT) :: PM
!                                       PM = exponential decay factor of the local deficit
!
!*      0.2    declarations of local variables
!
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_P_t), POINTER :: PK
!
REAL, DIMENSION(SIZE(PM)) :: ZD_TOP, ZWSAT_AVG, ZWD0_AVG
!                            ZD_TOP  = Topmodel active layer
!
REAL                  :: ZXI, ZPHI, ZNU, ZTI_MEAN, ZTI_MIN, ZTI_MAX, ZTI_STD, ZTI_SKEW
!                        ZTI_MEAN= ti mean after regression
!                        ZXI     = ti pdf parameter
!                        ZPHI    = ti pdf parameter
!                        ZNU     = ti pdf parameter
!
REAL                  :: ZFTOT, ZYMAX, ZYMIN
REAL                  :: ZGYMAX, ZGYMIN
!                        ZFTOT   = total fraction of a grid cell
!                        ZYMAX   = yi maximum variable
!                        ZYMIN   = yi minimum variable
!                        ZGYMAX  = incomplete gamma function for ymax (GAMSTAR result)
!                        ZGYMIN  = incomplete gamma function for ymin (GAMSTAR result)
!
REAL                  :: ZXSAT_IND, ZYSAT, ZY0, ZDMOY, ZXMOY, ZFMED, ZF0
!                        ZXSAT_IND = Satured index for all index 
!                        ZYSAT     = changing variable of satured index
!                        ZY0       = changing variable of dry index
!                        ZDMOY     = grid cell average deficit (= Dbar/M)
!                        ZXMOY     = ti mean value on wet (1-fsat-f0) fraction
!                        ZF0       = dry fraction
!                        ZFMED     = wet (1-fsat-f0) fraction
!
REAL                  :: ZG, ZGYSAT, ZGY0
!                        ZG     = GAM result 
!                        ZGYSAT = the incomplete gamma function for ysat (GAMSTAR result)
!                        ZGY0   = the incomplete gamma function for y0 (GAMSTAR result)
!
REAL                  :: ZD0, ZPAS, ZCOR
!                        ZD0  = Normalized TOPMODEL maximum deficit D0/M coefficient
!                        ZPAS = pas for calculate the new xsat values array
!
INTEGER               :: IFLG, IFLGST
!                        IFLG   = incomplete gamma function error flag (GAM result)
!                        IFLGST = incomplete gamma function error flag (GAMSTAR result)
!                                 (see gamma_inc.f for more detail)
!
REAL                  :: ZNO, ZAR, ZTOT
!
REAL                  :: ZFUP, ZFDOWN, ZQUP, ZQDOWN, ZSLOPEQ, ZWUP, ZWDOWN, ZSLOPEW
!
INTEGER, DIMENSION (1):: ID
!
INTEGER               :: INI, JI, IND, JSI_MIN, JSI_MAX, IPAS, &
                         JL, INL, JP, IMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       1 TOPMODEL SURFACE RUNOFF SCHEME
!       ================================
!
!  1.1     initialisation of local variable
!  ----------------------------------------
!
! Grid cells number
!
IF (LHOOK) CALL DR_HOOK('INIT_TOP',0,ZHOOK_HANDLE)
!
INI = SIZE(PM(:))
INL = IO%NGROUND_LAYER
IPAS = SIZE(S%XTAB_FSAT,2)
!
! GAM result (not use here !)
!
ZG  = 0.0
!
ZD_TOP    (:) = 0.0
ZWSAT_AVG (:) = 0.0
ZWD0_AVG(:) = 0.0
!
! soil properties for runoff (m)
!
IF (IO%CISBA == 'DIF') THEN                                   
!
  DO JP=1,IO%NPATCH

    PK => NP%AL(JP)
    KK => NK%AL(JP)

    IF (PK%NSIZE_P == 0 ) CYCLE

    DO JL=1,INL
      DO JI=1,PK%NSIZE_P
        ! 
        IMASK = PK%NR_P(JI)
        !
        ZD_TOP   (IMASK) = ZD_TOP   (IMASK) + PK%XPATCH(JI) * PK%XSOILWGHT(JI,JL)
        ZWSAT_AVG(IMASK) = ZWSAT_AVG(IMASK) + PK%XPATCH(JI) * PK%XSOILWGHT(JI,JL) * K%XWSAT(IMASK,JL)
        ZWD0_AVG (IMASK) = ZWD0_AVG (IMASK) + PK%XPATCH(JI) * PK%XSOILWGHT(JI,JL) * K%XWD0 (IMASK,JL)
        !
      ENDDO
    ENDDO
  ENDDO
!
  WHERE(ZD_TOP(:)>0.0)
    ZWSAT_AVG(:)=ZWSAT_AVG(:)/ZD_TOP(:)
    ZWD0_AVG (:)=ZWD0_AVG (:)/ZD_TOP(:)
  ENDWHERE
!
ELSE
!     
  DO JP=1,IO%NPATCH
    !
    PK => NP%AL(JP)
    KK => NK%AL(JP)    
    !
     IF (PK%NSIZE_P == 0 ) CYCLE
     !
     DO JI=1,PK%NSIZE_P
       !
       IMASK = PK%NR_P(JI)
       !
       ZD_TOP(IMASK)=ZD_TOP(IMASK) + PK%XRUNOFFD(JI) * PK%XPATCH(JI)
       !
     ENDDO
     !
  ENDDO
  !
  ZWSAT_AVG(:) = K%XWSAT(:,1)
  ZWD0_AVG (:) = K%XWD0 (:,1)
  !      
ENDIF
!
!
!  1.2     Algorithme
!  ------------------
!
!grid cells loops 
!
ZAR = 0.0
ZTOT= 0.0
ZNO = 0.0
!
DO JI=1,INI
!   
   IF(S%XTI_MEAN(JI)==XUNDEF)THEN
!
!    *Case where the Topographics index are not defined.
!    --------------------------------------------------------         
     ZNO=ZNO+1.0
     S%XTAB_FSAT(JI,:)=0.0     
     S%XTAB_WTOP(JI,:)=XUNDEF
     S%XTAB_QTOP(JI,:)=0.0
!     
     PM(JI) =XUNDEF
!
   ELSE 
!
     ZTOT = ZTOT + 1.0
!
!    1.2.0 first initialisation
!    --------------------------
!
     ZXI       = 0.0        
     ZPHI      = 0.0
     ZNU       = 0.0
!      
!    New version : Regressions directly in the pgd
!    1000 meter DEM to 2m DEM (PAN AND KING 2012)
!             
     ZTI_MEAN=S%XTI_MEAN(JI)
     ZTI_MIN =S%XTI_MIN (JI)
     ZTI_MAX =S%XTI_MAX (JI)
     ZTI_STD =S%XTI_STD (JI)
     ZTI_SKEW=S%XTI_SKEW(JI)
!
!    Calculate topographic index pdf parameters 
!
!    Numerical problem especialy over Greenland
     IF(ZTI_SKEW<=0.2)THEN
!     
       ZTI_SKEW=0.2
!       
       WRITE(KLUOUT,*)'TI_SKEW is too low or negatif (=',ZTI_SKEW,'),' 
       WRITE(KLUOUT,*)'then PHI is too big for the grid-cell',JI,'So,GAMMA(PHI) -> +inf.'
       WRITE(KLUOUT,*)'The applied solution is to put TI_SKEW = 0.2'
       IF(ZTI_STD<1.0)THEN
         WRITE(KLUOUT,*)'In addition TI_STD is too low (=',ZTI_STD,'),' 
         WRITE(KLUOUT,*)'The applied solution is to put TI_STD = 1.0'
         ZTI_STD=1.0
       ENDIF               
!
       ZAR  = ZAR +1.0
!     
       ZXI  = ZTI_SKEW*ZTI_STD/X2 
       ZPHI = (ZTI_STD/ZXI)**X2
!
     ELSE
!
       ZXI  = ZTI_SKEW*ZTI_STD/X2 
       ZPHI = (ZTI_STD/ZXI)**X2
!
     ENDIF
!
     ZNU  = ZTI_MEAN-ZPHI*ZXI
!
!    Exponential decay factor of the local deficit
!
     PM(JI) =(ZWSAT_AVG(JI)-ZWD0_AVG(JI))*ZD_TOP(JI)/X4 
!
!    1.2.1 Calculate grid cell pdf total density FTOT = F(ymin --> ymax)
!    -------------------------------------------------------------------
!
!    Normalized TOPMODEL maximum deficit D0/M coefficient
!
     ZD0 = (ZWSAT_AVG(JI)-ZWD0_AVG(JI))*ZD_TOP(JI)/PM(JI)
!
!    Initialise
!
     ZGYMAX = 0.0
     ZGYMIN = 0.0
     ZFTOT  = 0.0
     ZYMIN  = 0.0
     ZYMAX  = 0.0
!
!    variable changing yi ---> (ti-nu)/xi
!
     ZYMIN = (ZTI_MIN-ZNU)/ZXI
     ZYMAX = (ZTI_MAX-ZNU)/ZXI
!  
!    Supress numerical artifact
!
     ZCOR  = ABS(MIN(0.0,ZYMIN))
!
     ZYMIN = MAX(0.0,ZYMIN+ZCOR)
     ZYMAX = ZYMAX+ZCOR
!
!    Errors flags indicating a number of error condition in G and GSTAR
!    (see gamma_inc.f for more detail)
!
     IFLG        =0
     IFLGST      =0
!
!    Computation of F(0 --> ymin)
!
     CALL DGAM(ZPHI,ZYMIN,10.,ZG,ZGYMIN,IFLG,IFLGST)
!      
!    if the incomplete gamma function don't work, print why
!
     IF (IFLGST/=0)THEN
        WRITE(KLUOUT,*)'GRID-CELL =',JI,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YMIN= ',ZYMIN 
        CALL ABOR1_SFX('INIT_TOP: (1) PROBLEM WITH DGAM FUNCTION')
     ENDIF      
!
!    Computation of F(0 --> ymax)
!
     CALL DGAM(ZPHI,ZYMAX,10.,ZG,ZGYMAX,IFLG,IFLGST)
!      
!    if the incomplete gamma function don't work, print why
!
     IF (IFLGST/=0)THEN
        WRITE(KLUOUT,*)'GRID-CELL =',JI,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YMAX= ',ZYMAX
        CALL ABOR1_SFX('INIT_TOP: (2) PROBLEM WITH DGAM FUNCTION')
     ENDIF      
!
!    FTOT = F(0 --> ymax) - F(0 --> ymin)
!
     ZFTOT=ZGYMAX-ZGYMIN
!
!    initialization water content and fraction
!
     S%XTAB_WTOP(JI,1) = ZWSAT_AVG(JI)
     S%XTAB_FSAT(JI,1) = 1.0
     S%XTAB_QTOP(JI,1) = 0.0
!     
     S%XTAB_WTOP(JI,IPAS) = ZWD0_AVG(JI)
     S%XTAB_FSAT(JI,IPAS) = 0.0
     S%XTAB_QTOP(JI,IPAS) = 0.0
!
!    Define the new limits for the satured index loop
!
     JSI_MIN = 2
     JSI_MAX = IPAS-1
     ZPAS    = (ZTI_MAX-ZTI_MIN)/REAL(IPAS-1)
!
!    1.2.2 Calculate all topmodel arrays
!    -----------------------------------
!
!    Satured index loop
!
     DO IND=JSI_MIN,JSI_MAX
!
!       initialize of loops variables
!
        ZXSAT_IND = 0.0 
        ZYSAT     = 0.0
        ZY0       = 0.0
        ZDMOY     = 0.0
        ZXMOY     = 0.0
        ZFMED     = 0.0
!
!       Initialize of incomplete gamma function flags and variables
!
        IFLG   = 0
        IFLGST = 0
        ZGYSAT = 0.0
        ZGY0   = 0.0
!
!       calculate xsat for all new index
!
        ZXSAT_IND=ZTI_MIN+REAL(IND-1)*ZPAS
!
!       Changing variable to compute incomplete gamma function 
!
        ZYSAT=(ZXSAT_IND-ZNU)/ZXI 
        ZY0  =((ZXSAT_IND-ZD0)-ZNU)/ZXI
!      
!       Calculate Y0 and ysat and assume ymin < y0 < ymax !

        ZYSAT=MAX(ZYMIN,MIN(ZYSAT+ZCOR,ZYMAX))
        ZY0  =MAX(ZYMIN,MIN(ZY0  +ZCOR,ZYMAX))
!
!       call incomplete gamma function for xsat
!
        CALL DGAM(ZPHI,ZYSAT,10.,ZG,ZGYSAT,IFLG,IFLGST)  
!
!       if the incomplete gamma function don't works, print why
!
        IF (IFLGST/=0)THEN
           WRITE(KLUOUT,*)'GRID-CELL= ',JI,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YSAT= ',ZYSAT
           CALL ABOR1_SFX('INIT_TOP: (3) PROBLEM WITH DGAM FUNCTION')
        ENDIF 
!
!       call incomplete gamma function for xsat
!
        CALL DGAM(ZPHI,ZY0,10.,ZG,ZGY0,IFLG,IFLGST)
!
!       if the incomplete gamma function don't works, print why
!
        IF (IFLGST/=0)THEN
           WRITE(KLUOUT,*)'GRID-CELL= ',JI,'FLGST= ',IFLGST,'PHI= ',ZPHI,'Y0= ',ZY0
           CALL ABOR1_SFX('INIT_TOP: (4) PROBLEM WITH DGAM FUNCTION')
        ENDIF 
!
!       compute satured fraction as FSAT = F(0 --> ymax) - F(0 --> ysat)
!       
        S%XTAB_FSAT(JI,IND)=MAX(0.0,(ZGYMAX-ZGYSAT)/ZFTOT)
!
!       Compute driest fraction
!
        ZF0=MAX(0.0,(ZGY0-ZGYMIN)/ZFTOT)
!
!       Calculate FMED
!        
        ZFMED=(1.0-S%XTAB_FSAT(JI,IND)-ZF0)
!
        IF (ZFMED/=0.0) THEN
!
!          Compute the new x mean, xmoy', over the wet fraction Fwet
!
           ZXMOY = ZNU+ZXI*(ZPHI-ZCOR+(EXP(-ZY0)*(ZY0**(ZPHI/X2))*(ZY0**(ZPHI/X2))     &
                 - EXP(-ZYSAT)*(ZYSAT**(ZPHI/X2))*(ZYSAT**(ZPHI/X2)))/(ZFMED*GAMMAS(ZPHI)))
!
!          supress numerical artifacs
!
           ZXMOY =MAX((ZXSAT_IND-ZD0),MIN(ZXSAT_IND,ZXMOY))
!
!          Calculate the mean normalysed deficit as Dbar/M = (1-fsat-f0)*(xsat-xmoy')+f0*D0/M
!
           ZDMOY = ZFMED*(ZXSAT_IND-ZXMOY)+ZF0*ZD0
!
        ENDIF
!
!       supress numerical artifacs
!
        ZDMOY = MAX(0.0,MIN(ZDMOY,ZD0))
!
!       Solves Dbar = (Wsat-WT)*d_top with Dbar/M (=ZDMOY) = (Wsat-WT)*d_top/M
!
        S%XTAB_WTOP(JI,IND) = ZWSAT_AVG(JI)-(PM(JI)*ZDMOY/ZD_TOP(JI))

!       Solves Qs = FMED * M * Ks * exp(-Xsat) / Ks (dimentionless)
!
        S%XTAB_QTOP(JI,IND) = ZFMED*PM(JI)*EXP(-ZXSAT_IND)
!        
      ENDDO
!
    ENDIF
!
ENDDO
!
! supress numerical artifacs for boundaries conditions
!
DO JI=1,INI
!
!  Upper boundary
!
   IF(S%XTAB_WTOP(JI,2)==ZWSAT_AVG(JI))THEN
!
     ZFUP=S%XTAB_FSAT(JI,1)
     ZWUP=S%XTAB_WTOP(JI,1)
     ZQUP=S%XTAB_QTOP(JI,1)
!   
     ID(:)=MAXLOC(S%XTAB_WTOP(JI,:),S%XTAB_WTOP(JI,:)<ZWSAT_AVG(JI))
!   
     ZFDOWN=S%XTAB_FSAT(JI,ID(1))
     ZWDOWN=S%XTAB_WTOP(JI,ID(1))
     ZQDOWN=S%XTAB_QTOP(JI,ID(1))
!     
     ZSLOPEW=(ZWUP-ZWDOWN)/(ZFUP-ZFDOWN)   
     ZSLOPEQ=(ZQUP-ZQDOWN)/(ZFUP-ZFDOWN)   
!
     DO IND=2,ID(1)-1
        S%XTAB_WTOP(JI,IND)=ZWDOWN+(S%XTAB_FSAT(JI,IND)-ZFDOWN)*ZSLOPEW
        S%XTAB_QTOP(JI,IND)=ZQDOWN+(S%XTAB_FSAT(JI,IND)-ZFDOWN)*ZSLOPEQ
     ENDDO
!   
   ENDIF
!
!  Lower boundary
!
   WHERE(S%XTAB_FSAT(JI,:)<=0.0      )
         S%XTAB_WTOP(JI,:)=ZWD0_AVG(JI)
         S%XTAB_QTOP(JI,:)=0.0
   ENDWHERE
   WHERE(S%XTAB_WTOP(JI,:)<=ZWD0_AVG(JI))
         S%XTAB_FSAT(JI,:)=0.0
         S%XTAB_QTOP(JI,:)=0.0
   ENDWHERE
!   
ENDDO
!
WRITE(KLUOUT,*)'-------------------TOPMODEL SUM-UP-------------------------'
WRITE(KLUOUT,*)'Number of grid-cells ',INI,'Number of Topmodel points',INT(ZTOT)
IF(INI/=0) THEN
  WRITE(KLUOUT,*)'Percentage of non TOPMODEL grid-cells',(100.*ZNO/FLOAT(INI))
ENDIF
IF(ZTOT>0.0)THEN
  WRITE(KLUOUT,*)'Percentage of arranged (TI-SKE=0.2) grid-cells',(100.*ZAR/ZTOT)
ENDIF
WRITE(KLUOUT,*)'-----------------------------------------------------------'
!
IF (LHOOK) CALL DR_HOOK('INIT_TOP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_TOP
