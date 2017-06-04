!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###################
      SUBROUTINE FLOWDOWN(KNMC,PVAR,PCONN,KLINE)
!     ###################
!
!!****  *FLOWDOWN*
!
!!    PURPOSE
!!    -------
! to propagate data between pixels of a catchment in function of its topography
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
!!      K. Chancibault  * CNRM / Meteo-France *
!!      G-M Saulnier    * LTHE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   14/01/2005
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1     declarations of arguments
!
INTEGER, INTENT(IN) :: KNMC  ! catchment grid points number
REAL, DIMENSION(:), INTENT(INOUT)   :: PVAR  ! variable to propagate
REAL, DIMENSION(:,:), INTENT(IN)    :: PCONN ! catchment grid points connections
INTEGER, DIMENSION(:), INTENT(IN)   :: KLINE ! 
!
!*      0.2    declarations of local variables
!
INTEGER                  :: JJ, JI ! work variables
INTEGER                  :: JNUP  ! number of upslope pixels
INTEGER                  :: JCOL  ! third index of the pixel in the array XCONN
INTEGER                  :: JREF  ! index of the upslope pixel in the topo domain
REAL                     :: ZFAC  ! propagation factor between this pixel and the
                                  ! upslope one
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FLOWDOWN',0,ZHOOK_HANDLE)
!
DO JJ=1,KNMC
   JNUP = INT(PCONN(JJ,4))
   DO JI=1,JNUP
      JCOL = ((JI-1)*2) + 5
      JREF = INT(PCONN(JJ,JCOL))
      ZFAC = PCONN(JJ,JCOL+1)
      PVAR(JJ) = PVAR(JJ) + PVAR(KLINE(JREF)) * ZFAC
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('FLOWDOWN',1,ZHOOK_HANDLE)
!
END SUBROUTINE FLOWDOWN
