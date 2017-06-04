!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ZSFILTER(PZS,PMASK,KZSFILTER)
!     #############################################
!
!!**** *ZSFILTER* add a Laplacian to filter orographic signal
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    An iterative method is used, adding each time the discretized Laplacian
!!    to the point value.
!!    Note that only points where land is present are modified, taking into
!!    account only such points in the filtering.
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
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    15/03/96
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL,             DIMENSION(:,:), INTENT(INOUT) :: PZS      ! orography
REAL,             DIMENSION(:,:), INTENT(IN)    :: PMASK    ! where filter is applied
INTEGER,                          INTENT(IN)    :: KZSFILTER! iteration number
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL,DIMENSION(0:SIZE(PZS,1)+1,0:SIZE(PZS,2)+1) :: ZZS   ! modified 
                                                         ! orography
REAL,DIMENSION(0:SIZE(PZS,1)+1,0:SIZE(PZS,2)+1) :: ZMASK ! modified 
                                                         ! orography
INTEGER :: JI,JJ,JITER,IIU,IJU
REAL    :: ZK                 ! filter efficiency coefficient (0.=< ZK =<1.)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*       1.     Initialisations
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('ZSFILTER',0,ZHOOK_HANDLE)
IIU=SIZE(PZS,1)
IJU=SIZE(PZS,2)
ZK=1.
ZZS(:,:)=0.
ZZS(1:IIU,1:IJU)=PZS(:,:)
ZZS(0,:)    =2.*ZZS(1,:)  -ZZS(2,:)
ZZS(IIU+1,:)=2.*ZZS(IIU,:)-ZZS(IIU-1,:)
ZZS(:,0)    =2.*ZZS(:,1)  -ZZS(:,2)
ZZS(:,IJU+1)=2.*ZZS(:,IJU)-ZZS(:,IJU-1)
ZMASK(:,:)=0.
ZMASK(1:IIU,1:IJU)=PMASK(:,:)
!
!*       2.     Iterative loop
!               --------------
!
DO JITER=1,KZSFILTER
  DO JI=1,IIU
    DO JJ=1,IJU
      PZS(JI,JJ)= ZZS(JI,JJ)                          &
           + ZK*0.125* ZMASK(JI,JJ)                     &
            * (     ZMASK(JI-1,JJ)   * ZZS(JI-1,JJ)     &
                +   ZMASK(JI+1,JJ)   * ZZS(JI+1,JJ)     &
                +   ZMASK(JI,JJ-1)   * ZZS(JI,JJ-1)     &
                +   ZMASK(JI,JJ+1)   * ZZS(JI,JJ+1)     &
                - ( ZMASK(JI-1,JJ)                      &
                   +ZMASK(JI+1,JJ)                      &
                   +ZMASK(JI,JJ-1)                      &
                   +ZMASK(JI,JJ+1) ) * ZZS(JI,JJ)       )  
    ENDDO
  ENDDO
  ZZS(1:IIU,1:IJU)=PZS(:,:)
ENDDO
IF (LHOOK) CALL DR_HOOK('ZSFILTER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ZSFILTER
