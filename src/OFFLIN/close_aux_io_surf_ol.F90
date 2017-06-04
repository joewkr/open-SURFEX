!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
subroutine close_aux_io_surf_ol
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF_OL',0,ZHOOK_HANDLE)
 CALL ABOR1_SFX('CLOSE_AUX_IO_SURF_OL: NOT YET CODED!')
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF_OL',1,ZHOOK_HANDLE)
end subroutine close_aux_io_surf_ol
