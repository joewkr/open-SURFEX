!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_TEB_GARDEN_n (DTCO, U, IO, P, PEK, HPROGRAM,HPATCH)
!     ##################################
!
!!****  *READ_TEB_GARDEN_n* - routine to initialise ISBA variables
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
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!!
!!      READ_SURF for general reading : 08/2003 (S.Malardel)
!!      B. Decharme  2008    : Floodplains
!!      B. Decharme  01/2009 : Optional Arpege deep soil temperature read
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n, ONLY : DIAG_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_PE_t, ISBA_P_t
!
USE MODD_CO2V_PAR,       ONLY : XANFMINIT, XCONDCTMIN
!                                
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SNOW_PAR,       ONLY : XZ0SN
!
USE MODI_READ_SURF
!
USE MODI_INIT_IO_SURF_n
USE MODI_SET_SURFEX_FILEIN
USE MODI_END_IO_SURF_n
USE MODI_TOWN_PRESENCE
USE MODI_ALLOCATE_GR_SNOW
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
!
LOGICAL           :: GTOWN          ! town variables written in the file
INTEGER           :: IVERSION, IBUGFIX
INTEGER           :: ILU            ! 1D physical dimension
INTEGER           :: IRESP          ! Error code after redding
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=4)  :: YLVL
REAL, DIMENSION(:),ALLOCATABLE  :: ZWORK      ! 2D array to write data in file
!
INTEGER :: IWORK   ! Work integer
!
INTEGER :: JL, JNBIOMASS  ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_GARDEN_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, 'TOWN  ',ILU)
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU))
!* soil temperatures
!
IWORK=IO%NGROUND_LAYER
!
ALLOCATE(PEK%XTG(ILU,IWORK))
DO JL=1,IWORK
  WRITE(YLVL,'(I2)') JL
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF
  YRECFM=ADJUSTL(YRECFM)  
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  PEK%XTG(:,JL)=ZWORK
END DO
!
!
!* soil liquid water content
!
ALLOCATE(PEK%XWG(ILU,IWORK))
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF  
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  PEK%XWG(:,JL)=ZWORK
END DO
!
!* soil ice water content
!
ALLOCATE(PEK%XWGI(ILU,IWORK))
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
! ajouter ici un test pour lire les anciens fichiers
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF  
  YRECFM=ADJUSTL(YRECFM)  
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  PEK%XWGI(:,JL)=ZWORK
END DO
!
!* water intercepted on leaves
!
ALLOCATE(PEK%XWR(ILU))
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  YRECFM=HPATCH//'GD_WR'
ELSE
  YRECFM='TWN_WR'
ENDIF
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,PEK%XWR(:),IRESP)
!
!* Leaf Area Index (if prognostic)
!
IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_LAI'
  ELSE
    YRECFM='TWN_LAI'
  ENDIF        
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,PEK%XLAI(:),IRESP)        
END IF
!
!* snow mantel
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ')
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ')
!
 CALL TOWN_PRESENCE(HPROGRAM,GTOWN)
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP')
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
!
IF (.NOT. GTOWN) THEN
  PEK%TSNOW%SCHEME='1-L'
  CALL ALLOCATE_GR_SNOW(PEK%TSNOW,ILU)
ELSE
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    CALL READ_GR_SNOW(HPROGRAM,'GD',HPATCH,ILU,ILU,P%NR_P,0,PEK%TSNOW  )
  ELSE
    CALL READ_GR_SNOW(HPROGRAM,'GARD',HPATCH,ILU,ILU,P%NR_P,0,PEK%TSNOW  )
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
!* aerodynamical resistance
!
ALLOCATE(PEK%XRESA(ILU))
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  YRECFM=HPATCH//'GD_RES'
ELSE
  YRECFM='TWN_RESA'
ENDIF
YRECFM=ADJUSTL(YRECFM)
PEK%XRESA(:) = 100.
 CALL READ_SURF(HPROGRAM,YRECFM,PEK%XRESA(:),IRESP)
!
ALLOCATE(PEK%XLE(ILU))
PEK%XLE(:) = XUNDEF
!
!* ISBA-AGS variables
!
IF (IO%CPHOTO/='NON') THEN
  ALLOCATE(PEK%XAN   (ILU)) 
  ALLOCATE(PEK%XANDAY(ILU)) 
  ALLOCATE(PEK%XANFM (ILU))
  PEK%XAN(:)    = 0.
  PEK%XANDAY(:) = 0.
  PEK%XANFM(:)  = XANFMINIT
  PEK%XLE(:)    = 0.
ELSE
  ALLOCATE(PEK%XAN   (0)) 
  ALLOCATE(PEK%XANDAY(0)) 
  ALLOCATE(PEK%XANFM (0))
ENDIF
!
IF(IO%CPHOTO/='NON') THEN
  ALLOCATE(PEK%XBIOMASS         (ILU,IO%NNBIOMASS))
  ALLOCATE(PEK%XRESP_BIOMASS    (ILU,IO%NNBIOMASS))
ELSE
  ALLOCATE(PEK%XBIOMASS         (0,0))
  ALLOCATE(PEK%XRESP_BIOMASS    (0,0))
END IF
!
IF (IO%CPHOTO=='AST') THEN
  !
  PEK%XBIOMASS(:,:) = 0.
  PEK%XRESP_BIOMASS(:,:) = 0.
  !
ELSEIF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
  !
  PEK%XBIOMASS(:,:) = 0.
  DO JNBIOMASS=1,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
      YRECFM=HPATCH//'GD_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ELSE
      YRECFM='TWN_BIOMASS'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ENDIF
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(HPROGRAM,YRECFM,PEK%XBIOMASS(:,JNBIOMASS),IRESP)
  END DO

  PEK%XRESP_BIOMASS(:,:) = 0.
  DO JNBIOMASS=2,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
      YRECFM=HPATCH//'GD_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ELSE
      YRECFM='TWN_RESP_BIOM'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ENDIF    
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(HPROGRAM,YRECFM,PEK%XRESP_BIOMASS(:,JNBIOMASS),IRESP)
  END DO
  !
ENDIF
!
DEALLOCATE(ZWORK)
IF (LHOOK) CALL DR_HOOK('READ_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_GARDEN_n
