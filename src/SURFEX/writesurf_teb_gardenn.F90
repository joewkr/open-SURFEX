!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_TEB_GARDEN_n (HSELECT, OSNOWDIMNC, IO, S, PEK, HPROGRAM,HPATCH)
!     #####################################
!
!!****  *WRITESURF_TEB_GARDEN_n* - writes ISBA prognostic fields
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
!!      P. LeMoigne 12/2004 : correct dimensionning if more than 10 layers in
!!                            the soil (diffusion version)
!!      B. Decharme  2008    : Floodplains
!!      B. Decharme  01/2009 : Optional Arpege deep soil temperature write
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_PE_t, ISBA_S_t
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
USE MODI_WRITE_SURF
USE MODI_WRITESURF_GR_SNOW
USE MODD_DST_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT 
LOGICAL, INTENT(IN) :: OSNOWDIMNC
!
TYPE(ISBA_OPTIONS_t), INTENT(IN) :: IO
TYPE(ISBA_S_t), INTENT(IN) :: S
TYPE(ISBA_PE_t), INTENT(IN) :: PEK
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=3),  INTENT(IN)  :: HPATCH   ! current teb patch
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(SIZE(PEK%XTG,1)) :: IMASK_P
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=14) :: YFORM          ! Writing format
 CHARACTER(LEN=4 ) :: YLVL
!
INTEGER :: JL, JI ! loop counter on soil layers
!
REAL, DIMENSION(:),ALLOCATABLE  :: ZWORK      ! 2D array to write data in file
!
INTEGER :: JNBIOMASS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*       2.     Prognostic fields:
!               -----------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_GARDEN_N',0,ZHOOK_HANDLE)
ALLOCATE(ZWORK(SIZE(PEK%XTG,1)))
!* soil temperatures
!
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//'GD_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A11,I1.1,A4)'
  IF (JL >= 10)  YFORM='(A11,I2.2,A4)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_GD_TG',JL,' (K)'
  ZWORK=PEK%XTG(:,JL)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!* soil liquid water content
!
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//'GD_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A11,I1.1,A8)'
  IF (JL >= 10)  YFORM='(A11,I2.2,A8)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_GD_WG',JL,' (m3/m3)'
  ZWORK=PEK%XWG(:,JL)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!* soil ice water content
!
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//'GD_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A11,I1.1,A8)'
  IF (JL >= 10)  YFORM='(A11,I2.2,A8)'
  WRITE(YCOMMENT,YFORM) 'X_Y_GD_WGI',JL,' (m3/m3)'
  ZWORK=PEK%XWGI(:,JL)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
DEALLOCATE(ZWORK)
!
!* water intercepted on leaves
!
YRECFM=HPATCH//'GD_WR'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='X_Y_GD_WR (kg/m2)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XWR(:),IRESP,HCOMMENT=YCOMMENT)
!
!* Leaf Area Index
!
IF (IO%CPHOTO/='NON' .AND. IO%CPHOTO/='AST') THEN
  YRECFM=HPATCH//'GD_LAI'
  YRECFM=ADJUSTL(YRECFM)
  YCOMMENT='X_Y_GD_LAI (m2/m2)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XLAI(:),IRESP,HCOMMENT=YCOMMENT)
END IF
!
IF (IO%CPHOTO=='NIT') THEN
  !
  DO JNBIOMASS=1,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GD_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YFORM='(A11,I1.1,A8)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_BIOMASS',JNBIOMASS,' (kg/m2)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XBIOMASS(:,JNBIOMASS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !
  DO JNBIOMASS=2,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GD_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YFORM='(A16,I1.1,A10)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_RESP_BIOMASS',JNBIOMASS,' (kg/m2/s)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XRESP_BIOMASS(:,JNBIOMASS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
END IF
!
!* aerodynamical resistance
!
YRECFM=HPATCH//'GD_RES'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='X_Y_GD_RESA (s/m)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XRESA(:),IRESP,HCOMMENT=YCOMMENT)
!
!* snow mantel
!
YRECFM='GD'
DO JI = 1,SIZE(IMASK_P)
  IMASK_P(JI) = JI
ENDDO
 CALL WRITESURF_GR_SNOW(OSNOWDIMNC, HSELECT, HPROGRAM, YRECFM, HPATCH,&
                        SIZE(PEK%XTG,1), IMASK_P, 0, PEK%TSNOW, S%XWSN_WR, &
                        S%XRHO_WR, S%XHEA_WR, S%XAGE_WR, S%XSG1_WR, S%XSG2_WR, &
                        S%XHIS_WR, S%XALB_WR)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_GARDEN_n
