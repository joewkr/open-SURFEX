!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      SUBROUTINE MAKE_MASK_ISBA_TO_TOPD(KI)
!     #######################
!
!!****  *MAKE_MASK_ISBA_TO_TOPD*  
!!
!!    PURPOSE
!!    -------
!
!     Create a mask for each Surfex mesh and each catchment. 
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
     
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault  * CNRM * 
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   16/03/2005
!!                 11/2011 : Loops simplified (Vincendon)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPODYN,       ONLY : NNCAT, NNMC
USE MODD_COUPLING_TOPD, ONLY : NMASKT, NMASKI, NNPIX
USE MODD_SURF_PAR,        ONLY : NUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN) :: KI    ! Grid dimensions
!
!*      0.2    declarations of local variables
!
INTEGER, DIMENSION(KI)  :: INBPIX_IN_MESH ! number of pixel in each ISBA mesh
INTEGER                 :: JCAT, JPIX, INUMPIX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_ISBA_TO_TOPD',0,ZHOOK_HANDLE)
!
INUMPIX=MAXVAL(NNPIX)
!
ALLOCATE(NMASKI(KI,NNCAT,INUMPIX))
NMASKI(:,:,:) = NUNDEF
!
INBPIX_IN_MESH(:) = 0
!
DO JCAT = 1,NNCAT
  !
  DO JPIX = 1,NNMC(JCAT)
    !si le point du bassin versant est dans une maille isba
    IF ((NMASKT(JCAT,JPIX)/=0).AND.(NMASKT(JCAT,JPIX)/=NUNDEF)) THEN
      !indice du point du bassin versant dans la maille isba
      INBPIX_IN_MESH(NMASKT(JCAT,JPIX)) = INBPIX_IN_MESH(NMASKT(JCAT,JPIX)) + 1
      ! nmaski associe à la maille isba, au bassin versant et au numéro du point 
      ! du bassin versant dans la maille isba, l'indice du point dans le bassin
      ! versant
      NMASKI(NMASKT(JCAT,JPIX),JCAT,INBPIX_IN_MESH(NMASKT(JCAT,JPIX))) = JPIX
    ENDIF
    !
  ENDDO
  !
ENDDO
! write(*,*) 'NMASKT min et max',MINVAL(NMASKT(1,:)),MAXVAL(NMASKT(1,:))
! write(*,*) 'NMASKT min et max',MINVAL(NMASKT(2,:)),MAXVAL(NMASKT(2,:))
! write(*,*) 'NMASKT min et max',MINVAL(NMASKT(3,:)),MAXVAL(NMASKT(3,:))
! write(*,*) 'NMASKT min et max',MINVAL(NMASKT(4,:)),MAXVAL(NMASKT(4,:))
! write(*,*) 'NMASKI 3132 min et max',MINVAL(NMASKI(MINVAL(NMASKT(1,:)),1,:)),MAXVAL(NMASKI(MINVAL(NMASKT(1,:)),1,:))
! write(*,*) 'NMASKI 6662 min et max',MINVAL(NMASKI(MAXVAL(NMASKT(1,:)),1,:)),MAXVAL(NMASKI(MAXVAL(NMASKT(1,:)),1,:))
! stop
!
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_ISBA_TO_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_MASK_ISBA_TO_TOPD







