!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE WRITE_FILE_MAP(PVAR,HVAR)
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
USE MODD_TOPODYN, ONLY : CCAT, NNCAT, NNYC, NNXC, XX0, XY0, XDXT, NLINE, &
                         XTOPD, XNUL
!
USE MODD_SURF_PAR, ONLY : XUNDEF
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
REAL, DIMENSION(:,:), INTENT(IN) :: PVAR   ! variable to write in the file
 CHARACTER(LEN=30),    INTENT(IN) :: HVAR   ! end name of the file
!
!*      0.2    declarations of local variables
 CHARACTER(LEN=50),DIMENSION(NNCAT) :: CNAME
 CHARACTER(LEN=40)                  :: CFMT
 CHARACTER(*),PARAMETER     :: YPFMT1="('(',I4,'(F10.3,')"
INTEGER                    :: JWRK1,JJ,JI,JCAT
INTEGER                    :: IINDEX ! reference number of the pixel
INTEGER                    :: ILUOUT
REAL                       :: ZOUT ! pixel not included in the catchment
REAL                       :: ZMIN,ZMAX
REAL                       :: ZX1, ZY1, ZX2, ZY2 ! left top and right bottom pixels coordinates
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_MAP',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
 CALL GET_LUOUT('OFFLIN',ILUOUT)

ZOUT = XUNDEF
!
DO JCAT=1,NNCAT
  !
  CNAME(JCAT) = TRIM(CCAT(JCAT))//TRIM(HVAR)
  !
  WRITE(ILUOUT,*) CNAME(JCAT)
  !
  CALL OPEN_FILE('ASCII ',NUNIT,HFILE=CNAME(JCAT),HFORM='FORMATTED')
  !
  !*       1.0    writing header map file
  !               --------------------------------------
  !
  IINDEX = (NNYC(JCAT)-1) * NNXC(JCAT) + 1
  !
  ZX1 = XX0(JCAT)
  ZY1 = XY0(JCAT) + ( (NNYC(JCAT)-1) * XDXT(JCAT) )
  !
  ZMIN = MINVAL(PVAR(JCAT,:))
  ZMAX = MAXVAL(PVAR(JCAT,:),MASK=PVAR(JCAT,:)/=XUNDEF)
  !
  DO JJ=1,5
    WRITE(NUNIT,*)
  ENDDO
  !
  WRITE(NUNIT,*) XX0(JCAT)
  WRITE(NUNIT,*) XY0(JCAT)
  WRITE(NUNIT,*) NNXC(JCAT) 
  WRITE(NUNIT,*) NNYC(JCAT)
  WRITE(NUNIT,*) ZOUT
  WRITE(NUNIT,*) XDXT(JCAT)
  WRITE(NUNIT,*) ZMIN
  WRITE(NUNIT,*) ZMAX
  !
  DO JJ=1,NNYC(JCAT)
    !
    DO JI=1,NNXC(JCAT)
      !
      IINDEX = (JJ - 1) * NNXC(JCAT) + JI
      ZX1 = XX0(JCAT) + ((JI-1) * XDXT(JCAT))
      ZY1 = XY0(JCAT) + ((JJ-1) * XDXT(JCAT))
      !
      IF ( XTOPD(JCAT,IINDEX).EQ.XNUL(JCAT) ) THEN
        !
        WRITE(NUNIT,*) ZOUT
        !
      ELSEIF (NLINE(JCAT,IINDEX)/=0) THEN
        !
        WRITE(NUNIT,*) PVAR(JCAT,NLINE(JCAT,IINDEX))
        !
      ELSE
        !
        WRITE(NUNIT,*) ZOUT
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',NUNIT)
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_MAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_FILE_MAP
