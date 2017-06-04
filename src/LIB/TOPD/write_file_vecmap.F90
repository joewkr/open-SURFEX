!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!------------------------------------------------------------
!     ##########################
      SUBROUTINE WRITE_FILE_VECMAP(PVAR,HVAR,KCAT)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!        
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
!!    REFERENCE
!!    ---------
!!     
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault  * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   25/01/2005
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN
!
USE MODD_SURF_PAR, ONLY:XUNDEF
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
REAL, DIMENSION(:),INTENT(IN) :: PVAR   ! variable to write in the file
 CHARACTER(LEN=30), INTENT(IN) :: HVAR   ! end name of the file
INTEGER,           INTENT(IN) :: KCAT   ! catchment number
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=50)          :: CNAME
 CHARACTER(LEN=40)          :: CFMT
INTEGER                    :: JJ,JI,JK
INTEGER                    :: IINDEX ! reference number of the pixel
REAL                       :: ZOUT ! pixel not included in the catchment
REAL                       :: ZMIN,ZMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_VECMAP',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
ZOUT = XUNDEF
ZMIN = MINVAL(PVAR)
ZMAX = MAXVAL(PVAR)
!
CNAME = TRIM(CCAT(KCAT))//TRIM(HVAR)
!
 CALL OPEN_FILE('ASCII ',NUNIT,HFILE=CNAME,HFORM='FORMATTED')
!
DO JI=1,5
  WRITE(NUNIT,*)
ENDDO
!
WRITE(NUNIT,*) XX0(KCAT)
WRITE(NUNIT,*) XY0(KCAT)
WRITE(NUNIT,*) NNXC(KCAT) 
WRITE(NUNIT,*) NNYC(KCAT)
WRITE(NUNIT,*) ZOUT
WRITE(NUNIT,*) XDXT(KCAT)
WRITE(NUNIT,*) ZMIN
WRITE(NUNIT,*) ZMAX
!
DO JI=1,NNYC(KCAT)
  DO JK=1,NNXC(KCAT)
    IINDEX = (JI - 1) * NNXC(KCAT) + JK
    IF (XTOPD(KCAT,IINDEX).EQ.XNUL(KCAT)) THEN
      WRITE(NUNIT,*) ZOUT
    ELSE
      WRITE(NUNIT,*) PVAR(NLINE(KCAT,IINDEX))
    ENDIF
  ENDDO
ENDDO
! 
 CALL CLOSE_FILE('ASCII ',NUNIT)
!
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_VECMAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_FILE_VECMAP

