!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SET_AXIS(HNAME, PVALUE ,CDPOSITIVE, KSIZE, CDUNITS, PBOUNDS)
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Declare a Surfex axis to XIOS 
!!
!!
!!     IMPLICIT ARGUMENTS :
!!     -------------------- 
!!
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
!!
!!
!!     REFERENCE
!!     ---------
!!
!!     XIOS Reference guide - Yann Meurdesoif - 10/10/2014 - 
!!     svn co -r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> ; cd <dir>/doc ; ....
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    08/2015
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ABOR1_SFX
!
USE MODD_XIOS, ONLY : LXIOS
#ifdef WXIOS
USE XIOS,      ONLY : XIOS_GET_HANDLE, XIOS_ADD_CHILD, XIOS_SET_AXIS_ATTR, XIOS_AXISGROUP, XIOS_AXIS
#endif
!
!
IMPLICIT NONE
!
!   Arguments
!
CHARACTER(LEN=*), INTENT(IN)  :: HNAME    ! axis name
REAL,DIMENSION(:),INTENT(IN), OPTIONAL  :: PVALUE   ! axis coordinate values array
CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: CDPOSITIVE  ! 'up' or 'down', if axis is vertical
INTEGER         , INTENT(IN), OPTIONAL  :: KSIZE       ! size of the axis (when values are not provided)
CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: CDUNITS     ! Units for the values
REAL,DIMENSION(:,:),INTENT(IN),OPTIONAL :: PBOUNDS  ! coordinate bounds array (should be (2,:))
!
!  Local variables
!
#ifdef WXIOS
TYPE(xios_axisgroup) :: axisgroup_hdl
TYPE(xios_axis)      :: axis_hdl
#endif
!
INTEGER              :: I
REAL(KIND=JPRB), DIMENSION(:),ALLOCATABLE :: ZAXIS
REAL(KIND=JPRB)      :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SET_AXIS',0,ZHOOK_HANDLE)
!
IF (LXIOS) THEN 
#ifdef WXIOS
!
!$OMP SINGLE
   
   CALL XIOS_GET_HANDLE("axis_definition",axisgroup_hdl)
   CALL XIOS_ADD_CHILD(axisgroup_hdl,axis_hdl,HNAME)
   IF (PRESENT(PVALUE)) THEN
      CALL XIOS_SET_AXIS_ATTR(HNAME, VALUE=PVALUE, N_GLO=SIZE(PVALUE))
      IF (PRESENT(PBOUNDS)) THEN
         CALL XIOS_SET_AXIS_ATTR(HNAME, BOUNDS=PBOUNDS)
      ENDIF
      IF (PRESENT(CDUNITS)) THEN
         CALL XIOS_SET_AXIS_ATTR(HNAME, UNIT=CDUNITS)
      ENDIF
   ELSE
      IF (PRESENT(KSIZE)) THEN 
         ALLOCATE(ZAXIS(KSIZE))
         ZAXIS=(/(I, I=1,KSIZE)/)
         CALL XIOS_SET_AXIS_ATTR(HNAME, VALUE=ZAXIS, N_GLO=KSIZE)
         DEALLOCATE(ZAXIS)
      ELSE
         CALL ABOR1_SFX('SET_AXIS : MUST PROVIDE PVALUE OR KSIZE FOR'//TRIM(HNAME))
      ENDIF
   ENDIF
   IF (PRESENT(CDPOSITIVE)) THEN    
      CALL XIOS_SET_AXIS_ATTR(HNAME, POSITIVE=CDPOSITIVE)
   ENDIF

!$OMP END SINGLE
!
#endif
ENDIF
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SET_AXIS',1,ZHOOK_HANDLE)
!
END SUBROUTINE SET_AXIS
