!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SOILGRID(PSOILGRID, PSOILDEPTH, PDG, KWG_LAYER  )

!     ##########################################################################
!
!!****  *SOILGRID*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the soil grid configuration using a reference grid
!     Also compute the root fraction
!         
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    Boone (2000)
!!    Boone et al. (2000)
!!    Habets et al. (2003)
!!    Decharme et al. (2011)
!!      
!!    AUTHOR
!!    ------
!!      A. Boone           * Meteo-France *
!!      new version :
!!      B. Decharme        * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     12/04/03
!!      new version :10/08/2011
!!      modif       :   09/2012 soildepth can reach 12m (permafrost)
!!                              bug coef algo
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR, ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,    DIMENSION(:),     INTENT(IN)    :: PSOILGRID   ! reference soil grid          (m)
REAL,    DIMENSION(:),   INTENT(IN)    :: PSOILDEPTH  ! total soil depth             (m)   
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PDG         ! depth of base of soil layers (m)
INTEGER, DIMENSION(:),   INTENT(OUT)   :: KWG_LAYER   ! last layers for soil moisture
!
!*      0.2    declarations of local variables
!
REAL,DIMENSION(SIZE(PDG,1)) :: ZREF
!
REAL               :: ZWORK
!
INTEGER            :: INI,INL
INTEGER            :: JJ,JL
!
LOGICAL            :: LOPTIMGRID
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!        0.     Initialization
!               --------------
!
IF (LHOOK) CALL DR_HOOK('SOILGRID',0,ZHOOK_HANDLE)
!
INI    = SIZE(PDG,1)
INL    = SIZE(PDG,2)
!
KWG_LAYER (:) = 0
ZREF      (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       1.     Grid configuration
!               ------------------
!
!*       1.1    Check consistency
!               -----------------
!
LOPTIMGRID=.FALSE.
IF(INL==NOPTIMLAYER) THEN
  IF( ALL(PSOILGRID(1:INL)==XOPTIMGRID(1:NOPTIMLAYER)) ) LOPTIMGRID=.TRUE.
ENDIF
!
!*       1.2    Assign soil layer depths if ECOCLIMAP
!               -------------------------------------
!
IF(LOPTIMGRID)THEN
  !
  !Optimized ECOCLIMAP soil grid
  CALL OPTIMSOILGRID
  !
ELSE
  !
  WHERE(PSOILDEPTH(:)/=XUNDEF)
    PDG(:,1)=MIN(0.01,PSOILGRID(1))
  ELSEWHERE
    PDG(:,1)=XUNDEF
  ENDWHERE
  !
    DO JJ=1,INI 
      !
      IF( PSOILDEPTH(JJ)==XUNDEF )THEN
        !
        PDG      (JJ,:) = XUNDEF              
        KWG_LAYER(JJ) = NUNDEF
        !
      ELSE
        !
        DO JL=2,INL
          !
          PDG      (JJ,JL) = PSOILGRID(JL)
          !
          IF ( PSOILGRID(JL)-PSOILGRID(JL-1)<=0.3 ) THEN       
            ZWORK = ABS(PSOILGRID(JL)-PSOILDEPTH(JJ))
            IF(ZWORK<=ZREF(JJ))THEN
              KWG_LAYER(JJ) = JL
              ZREF     (JJ) = ZWORK
            ENDIF
          ELSEIF(PSOILDEPTH(JJ)>=(PSOILGRID(JL)*0.3+PSOILGRID(JL-1)*0.7))THEN
            KWG_LAYER(JJ) = JL
          ENDIF
        ENDDO
        !
      ENDIF
    ENDDO
  !
ENDIF
!
IF(ANY(KWG_LAYER(:)==0))THEN
  CALL ABOR1_SFX('SOILGRID: WITH CISBA=DIF NWG_LAYER MUST BE DEFINED FOR EACH POINT')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SOILGRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE OPTIMSOILGRID
!
USE MODD_REPROD_OPER, ONLY : CDGDIF
!
IMPLICIT NONE
!
! declarations of local variables
!
INTEGER, PARAMETER                  :: NDLIM = 13
!
REAL, DIMENSION(NDLIM), PARAMETER   :: ZDLIM = &
      (/1.25,1.50,1.75,2.00,2.25,2.50,2.75,3.00,3.50,4.00,4.50,5.00,5.50/)
!
REAL,DIMENSION(SIZE(PDG,1)) :: ZDG_WATER
!
LOGICAL            :: LWORK
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! init
!
IF (LHOOK) CALL DR_HOOK('SOILGRID:OPTIMSOILGRID',0,ZHOOK_HANDLE)
!
ZDG_WATER (:) = XUNDEF
!
!-------------------------------------------------------------------------------
!a. Arranged depth
!
IF(CDGDIF=='ROOT')THEN
!
     DO JJ=1,INI
        IF(PSOILDEPTH(JJ)<=1.1)THEN
          ZDG_WATER(JJ)=MIN(1.0,PSOILDEPTH(JJ))
        ELSEIF(PSOILDEPTH(JJ)>1.1.AND.PSOILDEPTH(JJ)<=1.25)THEN
          ZDG_WATER(JJ)=1.25          
        ELSEIF(PSOILDEPTH(JJ)>5.50.AND.PSOILDEPTH(JJ)<=8.00)THEN
              ZDG_WATER(JJ)=8.00
        ELSEIF(PSOILDEPTH(JJ)>8.00.AND.PSOILDEPTH(JJ)<XUNDEF)THEN
              ZDG_WATER(JJ)=12.00 ! Permafrost case
        ELSE
          DO JL=1,NDLIM-1         
             IF(PSOILDEPTH(JJ)>ZDLIM(JL).AND.PSOILDEPTH(JJ)<=ZDLIM(JL+1))THEN
               ZDG_WATER(JJ)=MERGE(ZDLIM(JL),ZDLIM(JL+1),PSOILDEPTH(JJ)<(0.8*ZDLIM(JL)+0.2*ZDLIM(JL+1)))
             ENDIF
          ENDDO
        ENDIF
     ENDDO
!
ELSE
!
     DO JJ=1,INI
        IF(PSOILDEPTH(JJ)<1.25)THEN
          ZDG_WATER(JJ)=MIN(1.0,PSOILDEPTH(JJ))
        ELSEIF(PSOILDEPTH(JJ)>=5.50.AND.PSOILDEPTH(JJ)<6.50)THEN
              ZDG_WATER(JJ)=5.50
        ELSEIF(PSOILDEPTH(JJ)>=6.50.AND.PSOILDEPTH(JJ)<10.50)THEN
              ZDG_WATER(JJ)=8.00
        ELSEIF(PSOILDEPTH(JJ)>=10.50.AND.PSOILDEPTH(JJ)<XUNDEF)THEN
              ZDG_WATER(JJ)=12.00 ! Permafrost case
        ELSE
          DO JL=1,NDLIM-1         
             IF(PSOILDEPTH(JJ)>=ZDLIM(JL).AND.PSOILDEPTH(JJ)<ZDLIM(JL+1))THEN
               ZDG_WATER(JJ)=MERGE(ZDLIM(JL),ZDLIM(JL+1),PSOILDEPTH(JJ)<(0.4*ZDLIM(JL)+0.6*ZDLIM(JL+1)))
             ENDIF
          ENDDO
        ENDIF
     ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!b. General cases
!
  DO JJ=1,INI
    !
    IF(PSOILDEPTH(JJ)==XUNDEF)THEN
      !
      PDG      (JJ,:) = XUNDEF      
      KWG_LAYER(JJ) = NUNDEF    
      !
    ELSE
      !
      PDG(JJ,:) = PSOILGRID(:)
      !      
      LWORK=(ZDG_WATER(JJ)<=1.0.OR.&
             ZDG_WATER(JJ)==1.5.OR.&
             ZDG_WATER(JJ)==2.0.OR.&
             ZDG_WATER(JJ)==3.0.OR.&
             ZDG_WATER(JJ)==5.0.OR.&
             ZDG_WATER(JJ)==8.0.OR.&
             ZDG_WATER(JJ)==12.0   )
      ! 
      IF (LWORK) THEN    
        DO JL=2,INL      
          ZWORK = ABS(PSOILGRID(JL)-ZDG_WATER(JJ))                 
          IF(ZWORK<=ZREF(JJ))THEN
            KWG_LAYER(JJ)=JL
            ZREF(JJ)=ZWORK
          ENDIF
        ENDDO           
      ENDIF
      !
    ENDIF
    !
  ENDDO
!
!-------------------------------------------------------------------------------
!c. Particular cases
!
WHERE (ZDG_WATER(:)==1.25)
  KWG_LAYER(:) = 9
  PDG(:,9) = ZDG_WATER(:)
ELSEWHERE (ZDG_WATER(:)==1.75 .OR. ZDG_WATER(:)==2.25)
  KWG_LAYER(:) = 10
  PDG(:,10) = ZDG_WATER(:)
  WHERE (ZDG_WATER(:)==1.75) PDG(:,9) = 1.25
ELSEWHERE (ZDG_WATER(:)==2.50 .OR. ZDG_WATER(:)==2.75 .OR. ZDG_WATER(:)==3.50)
  KWG_LAYER(:) = 11
  PDG(:,11) = ZDG_WATER(:)
ELSEWHERE (ZDG_WATER(:)==4.00 .OR. ZDG_WATER(:)==4.50 .OR. ZDG_WATER(:)==5.50)
  KWG_LAYER(:) = 12
  PDG(:,12) = ZDG_WATER(:)
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('SOILGRID:OPTIMSOILGRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE OPTIMSOILGRID
!
!-------------------------------------------------------------------------------
END SUBROUTINE SOILGRID
