!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################     
      SUBROUTINE READ_SLOPE_FILE(HPROGRAM,HFILE,HFORM,KNMC,PTANB,PSLOP,PDAREA,PLAMBDA)
!     #######################
!
!!****  *READ_SLOPE_FILE*  
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
!!                 03/2014 (B. Vincendon) add the possibility of reading topographic
!!                         files produced by a new chain (based of java+GRASS)
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
USE MODD_SURF_PAR,  ONLY : XUNDEF
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
INTEGER,           INTENT(IN)  :: KNMC        ! Number of pixels in the catchment
REAL, DIMENSION(:),  INTENT(OUT)   :: PTANB    ! pixels topographic slope(tan(beta)
REAL, DIMENSION(:),  INTENT(OUT)   :: PSLOP   ! pixels topographic slope/length flow
REAL, DIMENSION(:),  INTENT(OUT)   :: PDAREA  ! drainage area (aire drainee)
REAL, DIMENSION(:),  INTENT(OUT)   :: PLAMBDA ! pure topographic index
!
!*      0.2    declarations of local variables
!
!
INTEGER                   :: JJ ! loop control 
INTEGER                   :: IWRK        ! work variable
INTEGER                   :: ILUOUT      ! Unit of the files
!
REAL                      :: ZWRK        ! work variable
REAL, DIMENSION(KNMC)     :: ZDAREA      ! drainage area (aire drainee)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
CHARACTER(LEN=100)    :: YHEADER    ! Header File to be read
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_SLOPE_FILE',0,ZHOOK_HANDLE)
!
!*       0.2    preparing file openning
!               ----------------------
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_FILE(HPROGRAM,NUNIT,HFILE,HFORM,HACTION='READ')
!
READ(NUNIT,*) YHEADER
!
IF (INDEX(YHEADER,'pixel_REF')/=0) THEN !Slope file from new java+GRASS chain
 write(ILUOUT,*) 'Slope file from new java + GRASS chain'
 DO JJ=1,KNMC
  READ(NUNIT,*,END=110) IWRK, PTANB(JJ),PLAMBDA(JJ) 
 ENDDO
 PSLOP(:)=PLAMBDA(:) !not used
 PDAREA(:)=PLAMBDA(:) !not used
ELSE !Slope file from old f77  chain
 write(*,*) 'Slope file from old f77 chain'
 DO JJ=1,KNMC
   READ(NUNIT,*,END=110) IWRK, PTANB(JJ), PSLOP(JJ), ZWRK, PDAREA(JJ)
  PLAMBDA(JJ) = LOG(PDAREA(JJ)/PSLOP(JJ))
 ENDDO
ENDIF
110 CALL CLOSE_FILE(HPROGRAM,NUNIT)
!
IF (LHOOK) CALL DR_HOOK('READ_SLOPE_FILE',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_SLOPE_FILE







