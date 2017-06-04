!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_TEB_GREENROOF_n (HSELECT, KTIME, IO, K, HPROGRAM)
!     ###############################################
!
!!****  *WRITESURF_PGD_TEB_GREENROOF_n* - writes ISBA fields describing urban greenroofs
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
!!     A. Lemonsu & C. de Munck   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t
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
INTEGER, INTENT(IN) :: KTIME
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: K
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
!
!* soil scheme option
!
YRECFM='GR_ISBA'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,IO%CISBA,IRESP,HCOMMENT=YCOMMENT)
!
!* thermal conductivity option
!
YRECFM='GR_SCOND'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,IO%CSCOND,IRESP,HCOMMENT=YCOMMENT)
!
!* number of soil layers
!
YRECFM='GR_LAYER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,IO%NGROUND_LAYER,IRESP,HCOMMENT=YCOMMENT)
!
!* number of time data for green roof chacteristics (VEG, LAI, EMIS, Z0) 
!
YRECFM='GR_NTIME'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,KTIME,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='GR_RUNOFFB'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,K%XRUNOFFB,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='GR_WDRAIN'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HSELECT, HPROGRAM,YRECFM,K%XWDRAIN,IRESP,HCOMMENT=YCOMMENT)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TEB_GREENROOF_n
