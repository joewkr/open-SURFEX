!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      SUBROUTINE READ_TOPD_HEADER_DTM(HPROGRAM,HFILE,HFORM,PX0,PY0,KNXC,KNYC,PNUL,PDXT)
!     #######################
!
!!****  *READ_TOPD_HEADER*  
!!
!!    PURPOSE
!!    -------
!     This routine aims at reading topographic files header 
!     for a given file (then for a cathment)
!
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
 CHARACTER(LEN=*),  INTENT(IN)  :: HPROGRAM   !
 CHARACTER(LEN=*),  INTENT(IN)  :: HFILE      ! File to be read
 CHARACTER(LEN=*),  INTENT(IN)  :: HFORM      ! Format of the file to be read
REAL,              INTENT(OUT) :: PX0        ! abcissa of bottom-left pixel   
REAL,              INTENT(OUT) :: PY0        ! ordinate of bottom-left pixel   
INTEGER,           INTENT(OUT) :: KNXC       ! number of topographixc grid points along abcissa axis   
INTEGER,           INTENT(OUT) :: KNYC       ! number of topographixc grid points along ordinate axis   
REAL,              INTENT(OUT) :: PNUL       ! undifined value in topographic files
REAL,              INTENT(OUT) :: PDXT       ! catchment rid mesh size   
!
!*      0.2    declarations of local variables
!
INTEGER                   :: JJ          ! loop control 
INTEGER                   :: ILUOUT      ! Unit of the files
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_TOPD_HEADER_DTM',0,ZHOOK_HANDLE)
!
!*       0.2    preparing file openning
!               ----------------------
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
WRITE(ILUOUT,*) 'Open ',HFILE,'header'
!
 CALL OPEN_FILE(HPROGRAM,NUNIT,HFILE,HFORM,HACTION='READ')
!
DO JJ=1,5
  READ(NUNIT,*) 
ENDDO
!
READ(NUNIT,*) PX0
READ(NUNIT,*) PY0
READ(NUNIT,*) KNXC
READ(NUNIT,*) KNYC
READ(NUNIT,*) PNUL
READ(NUNIT,*) PDXT
READ(NUNIT,*)
READ(NUNIT,*)
!
 CALL CLOSE_FILE(HPROGRAM,NUNIT)
!
IF (LHOOK) CALL DR_HOOK('READ_TOPD_HEADER_DTM',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_TOPD_HEADER_DTM






 
