!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_LONLAT_n (DTCO, U, UG, HSELECT, HPROGRAM)
!     ####################################
!
!!****  *GET_LONLAT_n* - routine to get some surface fields
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
!!      Original    08/2008
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODI_GET_LUOUT
USE MODI_GET_COORD_n
USE MODI_GET_SURF_SIZE_n
USE MODI_WRITE_SURF
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_IO_BUFF_CLEAN
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
!
INTEGER            :: IRET      
 CHARACTER(LEN=100) :: YCOMMENT
!
INTEGER            :: INI      
REAL, DIMENSION(:), ALLOCATABLE :: ZLON, ZLAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_LONLAT_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
 CALL GET_SURF_SIZE_n(DTCO, U, 'FULL', INI)
!
ALLOCATE(ZLON(INI))
ALLOCATE(ZLAT(INI))
!
 CALL GET_COORD_n(UG, HPROGRAM,INI,ZLON,ZLAT)      
!
 CALL IO_BUFF_CLEAN
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','WRITE')
!
YCOMMENT='XLON'
 CALL WRITE_SURF(HSELECT,HPROGRAM,'XLON',ZLON(:),IRET,HCOMMENT=YCOMMENT,HDIR='A')
!
YCOMMENT='XLAT'
 CALL WRITE_SURF(HSELECT, HPROGRAM,'XLAT',ZLAT(:),IRET,HCOMMENT=YCOMMENT,HDIR='A')
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('GET_LONLAT_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_LONLAT_n
