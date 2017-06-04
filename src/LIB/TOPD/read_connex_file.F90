!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################     
      SUBROUTINE READ_CONNEX_FILE(HPROGRAM,HFILE,HFORM,KNMC,PCONN,KLINE)
!     #######################
!
!!****  *READ_CONNEX_FILE*  
!!
!!    PURPOSE
!!    -------
!     This routine aims at reading connexion file
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
!!                 03/2014 (B. Vincendon) format correction
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN, ONLY : NPMAX
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
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
INTEGER,           INTENT(IN)  :: KNMC       ! Number of pixels in the catchment
REAL, DIMENSION(:,:),INTENT(OUT)   :: PCONN    ! pixels topographic slope/length flow
INTEGER, DIMENSION(:),INTENT(OUT)  :: KLINE    ! second index of the pixel in the array PCONN
!
!*      0.2    declarations of local variables
!
!
INTEGER                   :: JJ          ! loop control 
INTEGER                   :: ILUOUT      ! Unit of the files
INTEGER                   :: IINDEX      ! index of the pixel in the topo domain
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_CONNEX_FILE',0,ZHOOK_HANDLE)
!
!*       0.2    preparing file openning
!               ----------------------
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_FILE(HPROGRAM,NUNIT,HFILE,HFORM,HACTION='READ')
!
WRITE(ILUOUT,*) 'Open ',HFILE,'debut'
!
DO JJ=1,7
  READ(NUNIT,*) 
ENDDO
!
DO JJ=1,KNMC
  !
  READ(NUNIT,*,END=120) PCONN(JJ,:)
  IINDEX = INT(PCONN(JJ,1))
  KLINE(IINDEX) = JJ
  !
ENDDO
!   
120   CALL CLOSE_FILE(HPROGRAM,NUNIT)
!
IF (LHOOK) CALL DR_HOOK('READ_CONNEX_FILE',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_CONNEX_FILE







