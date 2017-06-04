!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE FLAG_UPDATE (DIO, DUO, &
                              ONOWRITE_CANOPY,OPGD,OPROVAR_TO_DIAG,OSELECT)
!     ############################################################
!
!!****  *FLAG_UPDATE* - routine to modify selection of output fields
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
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2008 
!
!       B.Decharme  10/2009 flag to desactivate writing of pgd 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
!
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY
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
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DIO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
!
LOGICAL, INTENT(IN) :: ONOWRITE_CANOPY ! flag to (des)activate writing of canopy fields
LOGICAL, INTENT(IN) :: OPGD            ! flag to (des)activate writing of pgd field
LOGICAL, INTENT(IN) :: OPROVAR_TO_DIAG ! flag to (des)activate writing of diag of prognostic field
LOGICAL, INTENT(IN) :: OSELECT         ! flag to (des)activate control which fields are written
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FLAG_UPDATE',0,ZHOOK_HANDLE)
LNOWRITE_CANOPY = ONOWRITE_CANOPY
DIO%LPGD            = OPGD
DUO%LPROVAR_TO_DIAG = OPROVAR_TO_DIAG
DUO%LSELECT         = OSELECT
IF (LHOOK) CALL DR_HOOK('FLAG_UPDATE',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE FLAG_UPDATE
