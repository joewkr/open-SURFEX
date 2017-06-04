!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INI_DATA_SOIL(HISBA,PDG_OUT,PSURF,PSURF2,PROOTDEPTH, &
                               PSOILDEPTH,PSOILGRID,KWG_LAYER   )
!     #########################
!
!!**** *INI_DATA_SOIL* initializes soil depth and root fraction for a given
!!                     number of soil layers
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
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
!!    Original    01/04/2003
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_SOILGRID
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=*), INTENT(IN) :: HISBA   ! type of soil (Force-Restore OR Diffusion)
REAL, DIMENSION(:,:), INTENT(OUT) :: PDG_OUT
!
REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: PSURF
REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: PSURF2
REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: PROOTDEPTH
REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: PSOILDEPTH
REAL, DIMENSION(:),   OPTIONAL, INTENT(IN) :: PSOILGRID   ! reference soil grid          (m)
!
INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: KWG_LAYER   ! last layers for soil moisture
!
!*    0.2    Declaration of local variables
!      ------------------------------
!
LOGICAL,DIMENSION(SIZE(PDG_OUT,1)) :: LSURF
INTEGER            :: JLOOP    ! class loop counter
INTEGER            :: JLAYER   ! soil layer loop counter
INTEGER            :: JVEG     ! vegetation types loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    1.     Allocations
!            -----------
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_SOIL',0,ZHOOK_HANDLE)
!
PDG_OUT(:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2.     loop on cover types
!            -------------------
!
LSURF(:) = .FALSE. 
!
IF (PRESENT(PSURF2) .AND. PRESENT(PSURF)) THEN
  LSURF(:) = (PSURF(:)==0. .AND. PSURF2(:)==0.)
ELSEIF (PRESENT(PSURF)) THEN
  LSURF(:) = (PSURF(:)==0.)
ENDIF
!
!*    3.     soil depth
!            ----------
!
!*    3.1    force-restore case (2 layers)
!            ------------------
IF (HISBA=='2-L') THEN

  IF (.NOT.PRESENT(PROOTDEPTH)) CALL ABOR1_SFX("INI_DATA_SOIL: FOR HISBA==2-L, PROOTDEPTH IS NEEDED")
   
  DO JLOOP = 1,SIZE(LSURF)
    IF (LSURF(JLOOP)) CYCLE
    IF(PROOTDEPTH(JLOOP) /= XUNDEF) THEN
      PDG_OUT(JLOOP,1) = 0.01
      PDG_OUT(JLOOP,2) = PROOTDEPTH(JLOOP)
    ENDIF
  ENDDO
!
!
!*    3.2    force-restore case (3 layers)
!            ------------------
!
ELSE
          
  IF (.NOT.PRESENT(PSOILDEPTH)) CALL ABOR1_SFX("INI_DATA_SOIL: FOR HISBA/=2-L, PSOILDEPTH IS NEEDED")

  IF (HISBA=='3-L') THEN

    IF (.NOT.PRESENT(PROOTDEPTH)) CALL ABOR1_SFX("INI_DATA_SOIL: FOR HISBA==3-L, PROOTDEPTH IS NEEDED")

    DO JLOOP = 1,SIZE(LSURF)
      IF (LSURF(JLOOP)) CYCLE
      IF(PSOILDEPTH(JLOOP) /= XUNDEF) THEN
        PDG_OUT(JLOOP,1) = 0.01
        PDG_OUT(JLOOP,2) = PROOTDEPTH(JLOOP)
        PDG_OUT(JLOOP,3) = PSOILDEPTH(JLOOP)
      ENDIF
    ENDDO
!
!
!*    3.3    Diffusion case (at least 4 soil layers)
!            --------------
!
  ELSE

    IF (.NOT.PRESENT(PSOILGRID)) CALL ABOR1_SFX("INI_DATA_SOIL: FOR HISBA==DIF, PSOILGRID IS NEEDED")
    IF (.NOT.PRESENT(KWG_LAYER)) CALL ABOR1_SFX("INI_DATA_SOIL: FOR HISBA==DIF, KWG_LAYER IS NEEDED")

    CALL SOILGRID(PSOILGRID,PSOILDEPTH,PDG_OUT,KWG_LAYER)

  ENDIF

ENDIF
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_SOIL',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_DATA_SOIL
