!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
     SUBROUTINE CH_DEP_WATER  (PRESA,PUSTAR,PTA,PTRAD,PSV, HSV, PDEP)
  !###########################################################
  !
  !!                   
  !!                       
  !!
  !!    PURPOSE
  !!    -------
  !!      
  !!    Compute dry deposition velocity for chemical species    
  !!
  !!    AUTHOR
  !!    ------
  !!      P.Tulet      * Laboratoire d'Aerologie*
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original      20/02/97 
  !!    Modification  18/07/03  (Tulet) surface externalization
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !              ------------
  !
  USE MODD_CSTS
  USE MODD_CH_SURF
  !
!
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
!
  IMPLICIT NONE
  !
  !*       0.1   Declarations of dummy arguments :
  !
       REAL, DIMENSION(:),     INTENT(IN)  :: PRESA        ! aerodynamic resistances
       REAL, DIMENSION(:),     INTENT(IN)  :: PUSTAR       ! frition velocities
       REAL, DIMENSION(:),     INTENT(IN)  :: PTA          ! air temperature forcing (K)
       REAL, DIMENSION(:),     INTENT(IN)  :: PTRAD        ! radiative temperature   (K)
       REAL, DIMENSION(:,:),   INTENT(IN)  :: PSV
       CHARACTER(LEN=6), DIMENSION(:),INTENT(IN) :: HSV
       REAL, DIMENSION(:,:),   INTENT(INOUT):: PDEP      ! deposition dry velocity (m/s)
  !
  !
  !
  !*       0.2   Declarations of local variables :
  !
  !
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZSCMDT 
  ! Schmidt number
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZDIFFMOLVAL
  ! Molecular diffusivity
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZWATRB  
  ! water quasi-laminar  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZWATRC 
  ! water surface  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZRESWAT  
  !  final water resistance
  REAL, DIMENSION(SIZE(PTRAD,1))      :: ZTCOR
  REAL,DIMENSION(SIZE(PUSTAR,1))      ::ZUSTAR
  INTEGER :: JSV
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !============================================================================
  !            Primilary
  !            ---------


  !Default values
  !--------------
  IF (LHOOK) CALL DR_HOOK('CH_DEP_WATER',0,ZHOOK_HANDLE)
  ZWATRC(:,:) = 9999.
  ZRESWAT(:,:)= 9999.0
  ZWATRB(:,:) = 9999.
  ZUSTAR(:) = MAX(PUSTAR(:), 1E-9)
  !
  !
  !       2.0  Quasi-laminar resistance (for WATER) (Hicks, 1987)
  !            ------------------------      
  !
  !  
  !         compute molecular diffusivity for each species (Langevin, 1905)
  !         ----------------------------------------------
  DO JSV=1,SIZE(HSV,1)
    ZDIFFMOLVAL(:,JSV) = 2.22E-05 + 1.46E-07 * (PTRAD(:) - 273.0) * &
                                 SQRT(18. / XSREALMASSMOLVAL(JSV))  
    ZSCMDT(:,JSV)=0.15E-4 / ZDIFFMOLVAL(:,JSV)
  ENDDO
  !
  !
  !         For water
  !         ---------
  DO JSV=1,SIZE(HSV,1)
    ZWATRB(:,JSV) =  ((ZSCMDT(:,JSV)/0.72)**(2./3.)) &
                                     / (XKARMAN*ZUSTAR(:))  
  ENDDO
  !
  !
  !       3.  Surface resistance
  !            ------------------
  ! 
  !       3.1  Surface  resistance on water
  !            ----------------------------
  !
  !         3.1.1  Compute surface  resistance on inland water
  !                --------------------------------------------
  !  
  DO JSV=1,SIZE(HSV,1)
    ZWATRC(:,JSV) = 2.54E4 / ( XSREALHENRYVAL(JSV,1) *&
                       EXP(XSREALHENRYVAL(JSV,2)* (1./298. - 1./PTA(:))) *&
                       PTRAD(:) * ZUSTAR(:))  
  ENDDO
  ! 
  !          3.1.2 Surface temperature correction 
  !                ------------------------------
  ZTCOR(:) = 0.
  WHERE( PTRAD(:) < 271. )
    ZTCOR(:) = 1000. * EXP(-PTRAD(:) + 269.)
    ZTCOR(:) = MIN (2.5E3, ZTCOR(:))
  END WHERE
  DO JSV=1,SIZE(HSV,1)
    ZWATRC(:,JSV) = ZWATRC(:,JSV)+ZTCOR(:)   
  ENDDO
  !
  !
  !       5.0  Compute  water resistance (in land water and sea)
  !            -------------------------------------------------
  !
  DO JSV=1,SIZE(HSV,1) 
    ZRESWAT(:,JSV)  = PRESA(:) + ZWATRB(:,JSV) + ZWATRC(:,JSV)
  ENDDO
  !
  !        7.0  Compute dry deposition velocity for inland water 
  !             ------------------------------------------------
  !
  PDEP(:,:) = 1. / ZRESWAT(:,:)
IF (LHOOK) CALL DR_HOOK('CH_DEP_WATER',1,ZHOOK_HANDLE)
  !
END SUBROUTINE CH_DEP_WATER
