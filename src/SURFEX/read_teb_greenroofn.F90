!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_TEB_GREENROOF_n (DTCO, U, IO, P, PEK, HPROGRAM,HPATCH)
!     ##################################
!
!!****  *READ_TEB_GREENROOF_n* - routine to initialise ISBA variables
!!                         
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!    based on read_teb_greenroofn
!!
!!    AUTHOR
!!    ------
!!      C. de Munck & A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_PE_t, ISBA_P_t
!
USE MODD_CO2V_PAR,          ONLY : XANFMINIT, XCONDCTMIN
!                                
USE MODD_SURF_PAR,          ONLY : XUNDEF
USE MODD_SNOW_PAR,          ONLY : XZ0SN
!
USE MODI_READ_SURF
!
USE MODI_READ_GR_SNOW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_P_t), INTENT(INOUT) :: P
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=3),  INTENT(IN)  :: HPATCH   ! current TEB patch identificator
!
!*       0.2   Declarations of local variables
!              -------------------------------
INTEGER           :: ILU                        ! 1D physical dimension
INTEGER           :: IRESP                           ! Error code after redding
INTEGER           :: IWORK                           ! Work integer
INTEGER           :: JLAYER, JNBIOMASS               ! loop counter on layers
 CHARACTER(LEN=30) :: YRECFM                          ! Name of the article to be read
 CHARACTER(LEN=4)  :: YLVL
REAL, DIMENSION(:),ALLOCATABLE  :: ZWORK             ! 2D array to write data in file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, 'TOWN  ',ILU)
!
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU))
!
!* soil temperatures
!
IWORK = IO%NGROUND_LAYER
!
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  PEK%XTG(:,JLAYER) = ZWORK
END DO
!
!
!* soil liquid water content
!
DO JLAYER=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  PEK%XWG(:,JLAYER) = ZWORK
END DO
!
!* soil ice water content
!
DO JLAYER=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  PEK%XWGI(:,JLAYER) = ZWORK
END DO
!
!* water intercepted on leaves
!
YRECFM=HPATCH//'GR_WR'
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,PEK%XWR(:),IRESP)
!
!* Leaf Area Index
!
IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
  YRECFM = HPATCH//'GR_LAI'
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,PEK%XLAI(:),IRESP)
END IF
!
!* snow mantel
!
 CALL READ_GR_SNOW(HPROGRAM,'GR',HPATCH,ILU,ILU,P%NR_P,0,PEK%TSNOW  )! IOO:GreenROOf 
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
!* aerodynamical resistance
!
YRECFM = HPATCH//'GR_RESA'
YRECFM=ADJUSTL(YRECFM)
PEK%XRESA(:) = 100.
 CALL READ_SURF(HPROGRAM,YRECFM,PEK%XRESA(:),IRESP)
!
PEK%XLE(:) = XUNDEF
!
!* ISBA-AGS variables
!
IF (IO%CPHOTO/='NON') THEN
  PEK%XAN(:)    = 0.
  PEK%XANDAY(:) = 0.
  PEK%XANFM(:)  = XANFMINIT
  PEK%XLE(:)    = 0.
END IF
!
IF (IO%CPHOTO=='AST') THEN
  PEK%XBIOMASS(:,:)      = 0.
  PEK%XRESP_BIOMASS(:,:) = 0.
ELSEIF (IO%CPHOTO=='NIT') THEN
  PEK%XBIOMASS(:,:) = 0.
  DO JNBIOMASS=1,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GR_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(HPROGRAM,YRECFM,PEK%XBIOMASS(:,JNBIOMASS),IRESP)
  END DO

  PEK%XRESP_BIOMASS(:,:) = 0.
  DO JNBIOMASS=2,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GR_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(HPROGRAM,YRECFM,PEK%XRESP_BIOMASS(:,JNBIOMASS),IRESP)
  END DO
ENDIF
!
!
DEALLOCATE(ZWORK)
IF (LHOOK) CALL DR_HOOK('READ_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_GREENROOF_n
