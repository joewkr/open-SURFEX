!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_WRITESURF_TEB_GREENROOF_n
CONTAINS
      SUBROUTINE WRITESURF_TEB_GREENROOF_n (HSELECT, OSNOWDIMNC, IO, S, PEK, HPROGRAM,HPATCH)
!     #####################################
!
!!****  *WRITESURF_TEB_GREENROOF_n* - writes ISBA prognostic fields
!!
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    Based on "writesurf_teb_gardenn"
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
!!      A. Lemonsu & C. de Munck
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_PE_t, ISBA_S_t
!
USE MODI_WRITE_SURF
USE MODI_WRITESURF_GR_SNOW
USE MODD_DST_SURF
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
LOGICAL, INTENT(IN) :: OSNOWDIMNC
!
TYPE(ISBA_OPTIONS_t), INTENT(IN) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_PE_t), INTENT(IN) :: PEK
!
 CHARACTER(LEN=6),  INTENT(IN)     :: HPROGRAM ! program calling
 CHARACTER(LEN=3),  INTENT(IN)     :: HPATCH   ! current teb patch
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(SIZE(PEK%XTG,1)) :: IMASK_P
INTEGER                           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=30)                 :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100)                :: YCOMMENT       ! Comment string
 CHARACTER(LEN=14)                 :: YFORM          ! Writing format
 CHARACTER(LEN=4 )                 :: YLVL
!
INTEGER                           :: JL, JI        ! loop counter on soil layers
!
REAL, DIMENSION(:),  ALLOCATABLE  :: ZWORK          ! 2D array to write data in file
!
INTEGER                           :: IWORK          ! Work integer
!
INTEGER                           :: JSV, JNBIOMASS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*       2.     Prognostic fields:
!               -----------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
ALLOCATE(ZWORK(SIZE(PEK%XTG,1)))
!
!
!* soil temperatures
!
IWORK=IO%NGROUND_LAYER
!
DO JL=1,IWORK
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//'GR_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A13,I1.1,A4)'
  IF (JL >= 10)  YFORM='(A13,I2.2,A4)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_TWN_TG_GR',JL,' (K)'
  ZWORK=PEK%XTG(:,JL)
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!* soil liquid water content
!
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//'GR_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A13,I1.1,A8)'
  IF (JL >= 10)  YFORM='(A13,I2.2,A8)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_TWN_WG_GR',JL,' (m3/m3)'
  ZWORK=PEK%XWG(:,JL)
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!* soil ice water content
!
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//'GR_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A14,I1.1,A8)'
  IF (JL >= 10)  YFORM='(A14,I2.2,A8)'
  WRITE(YCOMMENT,YFORM) 'X_Y_GR_WGI',JL,' (m3/m3)'
  ZWORK=PEK%XWGI(:,JL)
  CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
DEALLOCATE(ZWORK)
!
!* water intercepted on leaves
!
YRECFM=HPATCH//'GR_WR'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='X_Y_TWN_WR_GR (kg/m2)'
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,PEK%XWR(:),IRESP,HCOMMENT=YCOMMENT)
!
!* Leaf Area Index
!
IF (IO%CPHOTO/='NON' .AND. IO%CPHOTO/='AST') THEN
  YRECFM=HPATCH//'GR_LAI'
  YRECFM=ADJUSTL(YRECFM)
  YCOMMENT='X_Y_GR_LAI (m2/m2)'
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,PEK%XLAI(:),IRESP,HCOMMENT=YCOMMENT)
END IF
!
!
!* biomass
!
IF (IO%CPHOTO=='NIT') THEN
  DO JNBIOMASS=1,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GR_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YFORM='(A11,I1.1,A8)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_BIOMASS',JNBIOMASS,' (kg/m2)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,PEK%XBIOMASS(:,JNBIOMASS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !
  DO JNBIOMASS=2,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GR_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YFORM='(A16,I1.1,A10)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_RESP_BIOMASS',JNBIOMASS,' (kg/m2/s)'
    CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,PEK%XRESP_BIOMASS(:,JNBIOMASS),IRESP,HCOMMENT=YCOMMENT)
  END DO
END IF
!
!* aerodynamical resistance
!
!
YRECFM=HPATCH//'GR_RESA'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='X_Y_GR_RESA (s/m)'
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,PEK%XRESA(:),IRESP,HCOMMENT=YCOMMENT)
!
!* snow mantel
!
DO JI = 1,SIZE(IMASK_P)
  IMASK_P(JI) = JI
ENDDO
YRECFM='GR'
 CALL WRITESURF_GR_SNOW(OSNOWDIMNC, HSELECT, HPROGRAM, YRECFM, HPATCH, &
                        SIZE(PEK%XTG,1), IMASK_P, 0, PEK%TSNOW, S%XWSN_WR, &
                        S%XRHO_WR, S%XHEA_WR, S%XAGE_WR, S%XSG1_WR, S%XSG2_WR, &
                        S%XHIS_WR, S%XALB_WR)

!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_GREENROOF_n
END MODULE MODI_WRITESURF_TEB_GREENROOF_n
