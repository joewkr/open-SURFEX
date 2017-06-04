!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_TEB_GARDEN_n (HSELECT, TOP, T, KTIME, IO, K, P, HPROGRAM)
!     ###############################################
!
!!****  *WRITE_PGD_TEB_GARDEN_n* - writes ISBA fields describing urban gardens
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
!!      A. Lemonsu & C. de Munck   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2011 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t
!
USE MODD_SURF_PAR,          ONLY : XUNDEF, NUNDEF
!
USE MODI_WRITE_SURF
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
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_t), INTENT(INOUT) :: T
!
INTEGER, INTENT(IN) :: KTIME
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_P_t), INTENT(INOUT) :: P
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=4 ) :: YLVL
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWORK
INTEGER :: JJ, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_GARDEN_N',0,ZHOOK_HANDLE)
!
!* soil scheme option
!
YRECFM='GD_ISBA'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,IO%CISBA,IRESP,HCOMMENT=YCOMMENT)
!
!* Reference grid for DIF
!
IF(IO%CISBA=='DIF') THEN
  DO JL=1,IO%NGROUND_LAYER
     WRITE(YLVL,'(I4)') JL     
     YRECFM='GD_SGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YCOMMENT='Depth of TEB Garden soilgrid layer '//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,IO%XSOILGRID(JL),IRESP,HCOMMENT=YCOMMENT)
  END DO 
ENDIF
!
!* number of soil layers
!
YRECFM='GD_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,IO%NGROUND_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of time data for vegetation characteristics (VEG, LAI, EMIS, Z0) 
!
YRECFM='GD_NTIME'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,KTIME,IRESP,HCOMMENT=YCOMMENT)
!
! * clay fraction
!
YRECFM='GD_CLAY'
YCOMMENT='X_Y_GD_CLAY'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XCLAY(:,1),IRESP,HCOMMENT=YCOMMENT)
!        
! * sand fraction
!
YRECFM='GD_SAND'
YCOMMENT='X_Y_GD_SAND'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XSAND(:,1),IRESP,HCOMMENT=YCOMMENT)
!        
! * orographic runoff coefficient
!
YRECFM='GD_RUNOFFB'
YCOMMENT='X_Y_GD_RUNOFFB'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XRUNOFFB,IRESP,HCOMMENT=YCOMMENT)
!        
! * subgrid drainage coefficient
!
YRECFM='GD_WDRAIN'
YCOMMENT='X_Y_GD_WDRAIN'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XWDRAIN,IRESP,HCOMMENT=YCOMMENT)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_GARDEN_n
