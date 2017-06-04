!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE LFIGET_LUOUT(HPROGRAM,KLUOUT)
!     #######################################################
!
!!****  *LFIGET_LUOUT* - get output listing logical unit
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
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling GROUND
INTEGER,           INTENT(OUT) :: KLUOUT   ! Logical unit of output listing
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
                                    ! at the open of the file in LFI  routines 
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LFIGET_LUOUT',0,ZHOOK_HANDLE)
KLUOUT=10  ! This value is not used by FMATTR and FMOPEN routines, for which
IF (LHOOK) CALL DR_HOOK('LFIGET_LUOUT',1,ZHOOK_HANDLE)
!          ! logical unit start at number 11
!-------------------------------------------------------------------------------
!
END SUBROUTINE LFIGET_LUOUT
