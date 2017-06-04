!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
     SUBROUTINE CH_DEP_TOWN ( PRESA_TOWN,  PUSTAR_TOWN,PTA, PTRAD, PWALL_O_HOR, &
                                PSV, HSV,  PDEP)  
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
  !!      Modification  21/07/00  (Guenais/Tulet) add deposition on town
  !!      Modification  18/07/03  (Tulet) surface externalization
  !!
  !-------------------------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !              ------------
  !
  USE MODD_CH_SURF
  USE MODD_CSTS
  !
!
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
!
  IMPLICIT NONE
  !
  !*       0.1   Declarations of dummy arguments :
  !
       REAL, DIMENSION(:),     INTENT(IN)  :: PRESA_TOWN   ! aerodynamic resistances
       REAL, DIMENSION(:),     INTENT(IN)  :: PUSTAR_TOWN  ! frition velocities
       REAL, DIMENSION(:),     INTENT(IN)  :: PTA          ! air temperature forcing (K)
       REAL, DIMENSION(:),     INTENT(IN)  :: PTRAD        ! radiative temperature  (K)
       REAL, DIMENSION(:),     INTENT(IN)  :: PWALL_O_HOR  ! normalized wall surface
       REAL, DIMENSION(:,:),   INTENT(OUT)    :: PDEP      ! deposition dry velocity (m/s)
       REAL, DIMENSION(:,:),   INTENT(IN)     :: PSV
       CHARACTER(LEN=6), DIMENSION(:),INTENT(IN) :: HSV
       !
  !
  !
  !*       0.2   Declarations of local variables :
  !
  !
  REAL    :: ZSTOWNRC_SO2
  ! donnees pour Rcsoil au SO2 Wesely (89)
  ! Hemisphere nord latitudes temperees
  REAL    :: ZSTOWNRC_O3
  ! donnees pour Rcsoil au O3 Wesely (89)
  ! Hemisphere nord latitudes temperees
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZSCMDT 
  ! Sc(:)hmidt number
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZDIFFMOLVAL
  ! Molecular diffusivity
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZTOWNRB 
  ! snow quasi-laminar  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZTOWNRC 
  ! towm surface  resistance

  REAL             , DIMENSION(SIZE(PTRAD,1),SIZE(HSV,1)) :: ZRESTOWN 
  !  final town  resistance
  REAL, DIMENSION(SIZE(PTRAD,1))      :: ZTOWN_MAX
  REAL, DIMENSION(SIZE(PTRAD,1))      :: ZTCOR
  REAL,DIMENSION(SIZE(PUSTAR_TOWN,1))      ::ZUSTAR_TOWN
  INTEGER :: JSV
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !============================================================================
  !            Primilary
  !            ---------

  !Default values
  !--------------
  IF (LHOOK) CALL DR_HOOK('CH_DEP_TOWN',0,ZHOOK_HANDLE)
  ZTOWNRC(:,:) = 9999.
  ZRESTOWN(:,:)= 9999.0
  ZTOWNRB(:,:) = 9999.
  !
  ! INDEX OF VEGTYPE
  ! ****************
  ZSTOWNRC_SO2 = 400.  ! Town
  ZSTOWNRC_O3  = 300.   ! Town

  ZUSTAR_TOWN(:) = MAX(PUSTAR_TOWN(:), 1E-9)
  !
  !        1.0  Aerodynamic resistance for the differents COVER TYPE
  !             ----------------------------------------------------
  !   PRESA_TOWN(:)     ! Aerodynamic resistance for TOWN 
  !
  !       2.0  Quasi-laminar resistance (Hicks, 1987)
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
        !         For town
        !         --------
        DO JSV=1,SIZE(HSV,1)
           ZTOWNRB(:,JSV) =  ((ZSCMDT(:,JSV)/0.72)**(2./3.)) &
                                          / (XKARMAN*ZUSTAR_TOWN(:))  
        ENDDO
  !
  !       3.  Surface resistance
  !            ------------------
  !          3.3.4 Surface temperature correction
  !                ------------------------------
           ZTCOR(:) = 0.
           WHERE(PTRAD(:) < 271.)
              ZTCOR(:) = 1000 * EXP(-PTRAD(:) + 269.)
              ZTCOR(:) = MIN(2.5E3, ZTCOR(:))
           END WHERE

           
           DO JSV=1,SIZE(HSV,1)
           ZTOWNRC(:,JSV) = ZTCOR(:) + (1.E5*ZSTOWNRC_SO2)/ (XSREALHENRYVAL(JSV,1) * &
                              EXP(XSREALHENRYVAL(JSV,2)* (1./298. - 1./PTA(:))))   
           ENDDO

  ! 
        !        6.0  Compute town resistance
        !             -----------------------
        !
        DO JSV=1,SIZE(HSV,1) 
           ZRESTOWN(:,JSV) = PRESA_TOWN(:) + &
                  ZTOWNRB(:,JSV) + ZTOWNRC(:,JSV)  
        ENDDO
           !
           ! Town fraction increased to consider the vertical surfaces of buildings
           ZTOWN_MAX(:) = 1. +  PWALL_O_HOR(:) 


        !        7.0  Compute final dry deposition velocity on urban area
        !             ---------------------------------------------------
        !
        DO JSV=1,SIZE(HSV,1) 
          PDEP(:,JSV) = ZTOWN_MAX(:) / ZRESTOWN(:,JSV)
        ENDDO
IF (LHOOK) CALL DR_HOOK('CH_DEP_TOWN',1,ZHOOK_HANDLE)

   
  !
END SUBROUTINE CH_DEP_TOWN
