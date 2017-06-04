!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      SUBROUTINE READ_TOPD_HEADER_CONNEX(HPROGRAM,HFILE,HFORM,KNMC)
!     #######################
!
!!****  *READ_TOPD_HEADER*  
!!
!!    PURPOSE
!!    -------
!     This routine aims at reading topographic files
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    
!!    
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Vincendon    * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   11/2006
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODI_GET_LUOUT
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN, ONLY : NPMAX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*),  INTENT(IN)  :: HPROGRAM    !
 CHARACTER(LEN=*),  INTENT(IN)  :: HFILE       ! File to be read
 CHARACTER(LEN=*),  INTENT(IN)  :: HFORM       ! Format of the file to be read
INTEGER,           INTENT(OUT) :: KNMC     ! number of pixels in a catchment 
!
!*      0.2    declarations of local variables
!
!
INTEGER                   :: JJ ! loop control 
INTEGER                   :: ILUOUT      ! Unit of the files
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_TOPD_HEADER_CONNEX',0,ZHOOK_HANDLE)
!
!*       0.2    preparing file openning
!               ----------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
WRITE(ILUOUT,*) 'Open ',HFILE,'header'
!
 CALL OPEN_FILE(HPROGRAM,NUNIT,HFILE,HFORM,HACTION='READ')
!
READ(NUNIT,*)
READ(NUNIT,*) KNMC
!
DO JJ=1,5
  READ(NUNIT,*) 
ENDDO
!
 CALL CLOSE_FILE(HPROGRAM,NUNIT)
!
IF (LHOOK) CALL DR_HOOK('READ_TOPD_HEADER_CONNEX',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_TOPD_HEADER_CONNEX






 
