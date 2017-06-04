!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOCATE_PHYSIO (IO, KK, PK, PEK, KVEGTYPE )
!   ##########################################################################
!
!!****  *ALLOCATE_PHYSIO* - 
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
!!
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    xx/xxxx
!!      Modified 10/2014 P. Samuelsson  MEB
!
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_AGRI,        ONLY : LAGRIP
!
USE MODD_TREEDRAG,       ONLY : LTREEDRAG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
INTEGER, INTENT(IN) :: KVEGTYPE
!
INTEGER :: ISIZE
INTEGER               :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_PHYSIO',0,ZHOOK_HANDLE)
!
ISIZE = PK%NSIZE_P
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
!
ALLOCATE(PK%XDG                     (ISIZE,IO%NGROUND_LAYER)) 
ALLOCATE(PK%XD_ICE                  (ISIZE              )) 
!
ALLOCATE(PEK%XLAI                    (ISIZE              )) 
ALLOCATE(PEK%XVEG                    (ISIZE              )) 
ALLOCATE(PEK%XZ0                     (ISIZE              )) 
ALLOCATE(PEK%XEMIS                   (ISIZE              )) 
!
ALLOCATE(PEK%XRSMIN                  (ISIZE              )) 
ALLOCATE(PEK%XGAMMA                  (ISIZE              )) 
ALLOCATE(PEK%XWRMAX_CF               (ISIZE              )) 
ALLOCATE(PEK%XRGL                    (ISIZE              )) 
ALLOCATE(PEK%XCV                     (ISIZE              )) 
ALLOCATE(PEK%XALBNIR_VEG             (ISIZE              )) 
ALLOCATE(PEK%XALBVIS_VEG             (ISIZE              )) 
ALLOCATE(PEK%XALBUV_VEG              (ISIZE              )) 
!
ALLOCATE(PK%XZ0_O_Z0H               (ISIZE              )) 
!
IF (ISIZE_LMEB_PATCH>0 .OR. IO%CPHOTO/='NON') THEN
  ALLOCATE(PEK%XBSLAI                  (ISIZE              )) 
ELSE
  ALLOCATE(PEK%XBSLAI     (0))  
ENDIF
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT' options)
!
IF (IO%CPHOTO/='NON'.OR.LTREEDRAG) THEN
  ALLOCATE(PK%XH_TREE                 (ISIZE              ))
ELSE
  ALLOCATE(PK%XH_TREE                 (0                 ))
ENDIF
!
IF (IO%CPHOTO/='NON') THEN
  ALLOCATE(PK%XRE25                   (ISIZE              )) 
  ALLOCATE(PK%XDMAX                   (ISIZE              ))  
  ALLOCATE(PEK%XLAIMIN                (ISIZE              )) 
  ALLOCATE(PEK%XSEFOLD                (ISIZE              )) 
  ALLOCATE(PEK%XGMES                  (ISIZE              )) 
  ALLOCATE(PEK%XGC                    (ISIZE              )) 
  ALLOCATE(PEK%XF2I                   (ISIZE              ))
  ALLOCATE(PEK%LSTRESS                (ISIZE              )) 
  IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
    ALLOCATE(PEK%XCE_NITRO               (ISIZE              )) 
    ALLOCATE(PEK%XCF_NITRO               (ISIZE              )) 
    ALLOCATE(PEK%XCNA_NITRO              (ISIZE              ))  
  ELSE
    ALLOCATE(PEK%XCE_NITRO    (0))
    ALLOCATE(PEK%XCF_NITRO    (0))
    ALLOCATE(PEK%XCNA_NITRO   (0))
  ENDIF
ELSE
  ALLOCATE(PK%XRE25      (0))
  ALLOCATE(PK%XDMAX      (0))  
  ALLOCATE(PEK%XLAIMIN   (0))
  ALLOCATE(PEK%XSEFOLD   (0))  
  ALLOCATE(PEK%XGMES     (0))
  ALLOCATE(PEK%XGC       (0))
  ALLOCATE(PEK%XF2I      (0))
  ALLOCATE(PEK%LSTRESS   (0))
  ALLOCATE(PEK%XCE_NITRO (0))
  ALLOCATE(PEK%XCF_NITRO (0))
  ALLOCATE(PEK%XCNA_NITRO(0))
ENDIF  
!
! - Irrigation, seeding and reaping
!
IF (LAGRIP .AND. (IO%CPHOTO == 'NIT' .OR. IO%CPHOTO == 'NCB'))  THEN
  ALLOCATE(PEK%TSEED                  (ISIZE              )) 
  ALLOCATE(PEK%TREAP                  (ISIZE              )) 
  ALLOCATE(PEK%XWATSUP                (ISIZE              )) 
  ALLOCATE(PEK%XIRRIG                 (ISIZE              ))
ELSE
  ALLOCATE(PEK%TSEED     (0))
  ALLOCATE(PEK%TREAP     (0))
  ALLOCATE(PEK%XWATSUP   (0))
  ALLOCATE(PEK%XIRRIG    (0))        
ENDIF
!
! - ISBA-DF scheme
!
IF(IO%CISBA=='DIF')THEN
  ALLOCATE(PK%XROOTFRAC  (ISIZE,IO%NGROUND_LAYER))
  ALLOCATE(PK%NWG_LAYER  (ISIZE))
  ALLOCATE(PK%XDROOT     (ISIZE))
  ALLOCATE(PK%XDG2       (ISIZE))
ELSE  
  ALLOCATE(PK%XROOTFRAC  (0,0))
  ALLOCATE(PK%NWG_LAYER  (0)  )
  ALLOCATE(PK%XDROOT     (0)  )        
  ALLOCATE(PK%XDG2       (0)  )        
ENDIF
!
ALLOCATE(PEK%XGNDLITTER (ISIZE))
ALLOCATE(PEK%XZ0LITTER  (ISIZE))
ALLOCATE(PEK%XH_VEG     (ISIZE))
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_PHYSIO',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_PHYSIO
