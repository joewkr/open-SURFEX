!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE OL_READ_ATM_ASCII (KFORC_STEP,                                 &
                              PTA,PQA,PWIND,PDIR_SW,PSCA_SW,PLW,PSNOW,    &
                              PRAIN,PPS,PCO2,PDIR                         )  
!**************************************************************************
!
!!    PURPOSE
!!    -------
!         Read in the ascii file the atmospheric forcing for the actual time
!         step KFORC_STEP, and for the next one.
!         The two time step are needed for the time interpolation of the
!         forcing.
!         If the end of the file  is reached, set the two step to the last
!         values.
!         Return undef value if the variable is not present
!!
!!**  METHOD
!!    ------
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
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     03/2008       
!
USE MODD_IO_SURF_OL, ONLY : XCOUNT
USE MODI_READ_SURF_ATM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
! global variables
REAL, DIMENSION(:,:),INTENT(OUT) :: PTA
REAL, DIMENSION(:,:),INTENT(OUT) :: PQA
REAL, DIMENSION(:,:),INTENT(OUT) :: PWIND
REAL, DIMENSION(:,:),INTENT(OUT) :: PDIR_SW
REAL, DIMENSION(:,:),INTENT(OUT) :: PSCA_SW
REAL, DIMENSION(:,:),INTENT(OUT) :: PLW
REAL, DIMENSION(:,:),INTENT(OUT) :: PSNOW
REAL, DIMENSION(:,:),INTENT(OUT) :: PRAIN
REAL, DIMENSION(:,:),INTENT(OUT) :: PPS
REAL, DIMENSION(:,:),INTENT(OUT) :: PCO2
REAL, DIMENSION(:,:),INTENT(OUT) :: PDIR
INTEGER,INTENT(IN)               :: KFORC_STEP
! local variables
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! read data
IF (LHOOK) CALL DR_HOOK('OL_READ_ATM_ASCII',0,ZHOOK_HANDLE)
 CALL READ_SURF_ATM('ASCII ',PTA    (:,1:XCOUNT),KFORC_STEP,XCOUNT,22)
 CALL READ_SURF_ATM('ASCII ',PQA    (:,1:XCOUNT),KFORC_STEP,XCOUNT,23)
 CALL READ_SURF_ATM('ASCII ',PWIND  (:,1:XCOUNT),KFORC_STEP,XCOUNT,24)
 CALL READ_SURF_ATM('ASCII ',PLW    (:,1:XCOUNT),KFORC_STEP,XCOUNT,25)
 CALL READ_SURF_ATM('ASCII ',PDIR_SW(:,1:XCOUNT),KFORC_STEP,XCOUNT,26)
 CALL READ_SURF_ATM('ASCII ',PSCA_SW(:,1:XCOUNT),KFORC_STEP,XCOUNT,27)
 CALL READ_SURF_ATM('ASCII ',PRAIN  (:,1:XCOUNT),KFORC_STEP,XCOUNT,28)
 CALL READ_SURF_ATM('ASCII ',PSNOW  (:,1:XCOUNT),KFORC_STEP,XCOUNT,29)
 CALL READ_SURF_ATM('ASCII ',PPS    (:,1:XCOUNT),KFORC_STEP,XCOUNT,30)
 CALL READ_SURF_ATM('ASCII ',PDIR   (:,1:XCOUNT),KFORC_STEP,XCOUNT,31)
 CALL READ_SURF_ATM('ASCII ',PCO2   (:,1:XCOUNT),KFORC_STEP,XCOUNT,32)
!
IF (LHOOK) CALL DR_HOOK('OL_READ_ATM_ASCII',1,ZHOOK_HANDLE)
!
END SUBROUTINE OL_READ_ATM_ASCII
