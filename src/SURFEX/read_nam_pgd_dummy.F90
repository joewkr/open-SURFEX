!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_DUMMY(HPROGRAM, KDUMMY_NBR, HDUMMY_NAME, HDUMMY_AREA, &
                                      HDUMMY_ATYPE, HDUMMY_FILE, HDUMMY_FILETYPE      )  
!     ##############################################################
!
!!**** *READ_NAM_PGD_DUMMY* reads namelist NAM_DUMMY_PGD
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2005
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),                   INTENT(IN)   :: HPROGRAM     ! Type of program
INTEGER,                            INTENT(OUT)  :: KDUMMY_NBR
!                          ! number of dummy pgd fields chosen by user
 CHARACTER(LEN=20), DIMENSION(1000), INTENT(OUT)  :: HDUMMY_NAME
!                          ! name of the dummy pgd fields (for information)
 CHARACTER(LEN=3),  DIMENSION(1000), INTENT(OUT)  :: HDUMMY_AREA
!                          ! areas where dummy pgd fields are defined
!                          ! 'ALL' : everywhere
!                          ! 'SEA' : where sea exists
!                          ! 'LAN' : where land exists
!                          ! 'WAT' : where inland water exists
!                          ! 'NAT' : where natural or agricultural areas exist
!                          ! 'TWN' : where town areas exist
!                          ! 'STR' : where streets are present
!                          ! 'BLD' : where buildings are present
 CHARACTER(LEN=3),  DIMENSION(1000), INTENT(OUT)  :: HDUMMY_ATYPE    ! avg type for dummy pgd fields
!                                                                   ! 'ARI' , 'INV'
 CHARACTER(LEN=28), DIMENSION(1000), INTENT(OUT)  :: HDUMMY_FILE     ! data files
 CHARACTER(LEN=6),  DIMENSION(1000), INTENT(OUT)  :: HDUMMY_FILETYPE ! type of these files
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                             :: NDUMMY_NBR
!                          ! number of dummy pgd fields chosen by user
 CHARACTER(LEN=20), DIMENSION(1000)  :: CDUMMY_NAME
!                          ! name of the dummy pgd fields (for information)
 CHARACTER(LEN=3),  DIMENSION(1000)  :: CDUMMY_AREA
!                          ! areas where dummy pgd fields are defined
!                          ! 'ALL' : everywhere
!                          ! 'SEA' : where sea exists
!                          ! 'LAN' : where land exists
!                          ! 'WAT' : where inland water exists
!                          ! 'NAT' : where natural or agricultural areas exist
!                          ! 'TWN' : where town areas exist
!                          ! 'STR' : where streets are present
!                          ! 'BLD' : where buildings are present
 CHARACTER(LEN=3),  DIMENSION(1000)  :: CDUMMY_ATYPE    ! avg type for dummy pgd fields
!                                                      ! 'ARI' , 'INV'
 CHARACTER(LEN=28), DIMENSION(1000)  :: CDUMMY_FILE     ! data files
 CHARACTER(LEN=6),  DIMENSION(1000)  :: CDUMMY_FILETYPE ! type of these files
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DUMMY_PGD/ NDUMMY_NBR, CDUMMY_NAME, CDUMMY_AREA,       &
                          CDUMMY_ATYPE, CDUMMY_FILE, CDUMMY_FILETYPE  
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_DUMMY',0,ZHOOK_HANDLE)
NDUMMY_NBR = 0
!
CDUMMY_NAME     = "                    "
CDUMMY_FILE     = "                            "
CDUMMY_FILETYPE = "      "
CDUMMY_AREA     = "ALL"
CDUMMY_ATYPE    = "ARI"
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DUMMY_PGD',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DUMMY_PGD)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*    3.      Fills output arguments
!             ----------------------
!
KDUMMY_NBR         = NDUMMY_NBR
HDUMMY_NAME(:)     = CDUMMY_NAME(:)
HDUMMY_AREA(:)     = CDUMMY_AREA(:)
HDUMMY_ATYPE(:)    = CDUMMY_ATYPE(:)
HDUMMY_FILE(:)     = CDUMMY_FILE(:)
HDUMMY_FILETYPE(:) = CDUMMY_FILETYPE(:)
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_DUMMY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_DUMMY
