!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_TSZ0_PAR_n (HSELECT, DTZ, HPROGRAM)
!     ################################################
!
!!****  *WRITESURF_PGD_TSZ0_PAR_n* - writes TSZ0 physiographic fields
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
!!      P. Le Moigne 12/2004 : add type of photosynthesis 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
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
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TSZ0_PAR_N',0,ZHOOK_HANDLE)
!
DTZ%NTIME = SIZE(DTZ%XDATA_DTS)
YRECFM   = 'ND_TSZ0_TIME'
YCOMMENT = '(-)'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,DTZ%NTIME,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM   = 'D_DTS'
YCOMMENT = 'X_Y_DATA_DTS'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,DTZ%XDATA_DTS(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-',HNAM_DIM="Nforc_tsz0      ")
!
YRECFM   = 'D_DHUGRD'
YCOMMENT = 'X_Y_DATA_DHUGRD'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,DTZ%XDATA_DHUGRD(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-',HNAM_DIM="Nforc_tsz0      ")
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TSZ0_PAR_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TSZ0_PAR_n
