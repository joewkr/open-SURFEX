!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE GET_VEGTYPE_2_PATCH_MASK(  &
        KLUOUT,                            &! output listing logical unit
       KSIZE_VEG,                         &!I Size of a vegetation vector within a patch vector
       KSIZE_PATCH,                       &!I Size of a patch within a nature vector
       KMASK_PATCH_NATURE,                &!I Mask to transform from patch vector to nature vector
       PVEGTYPE_PATCH,                    &!I Fraction of a nature point #i with vegetation #j which is packed to patch #k
       KMASK,                             &!O Mask from vegtype vector to patch vector
       KVEGTYPE                          &!I Index of vegtype in question
       )      
!
!
!!    PURPOSE
!!    -------
!     Create a patch-->vegtype mask
!     So that later, a patch can be packed into vegtype vectors
!!
!!    AUTHOR
!!    ------
!!     Alf Grini <alf.grini@cnrm.meteo.fr>
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2005
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE  ! Number of possible vegtypes
!!------------------------------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)                        :: KLUOUT               !Output listing logical unit
INTEGER, INTENT(IN)                        :: KSIZE_VEG            !Size of vegetation vector in question
INTEGER, INTENT(IN)                        :: KSIZE_PATCH          !Size of patch vector in question
INTEGER, INTENT(IN),DIMENSION(:)           :: KMASK_PATCH_NATURE   !PATCH -->NATURE mask
!
INTEGER, INTENT(IN)                        :: KVEGTYPE !Vegtype in quesition

REAL, DIMENSION(:,:), INTENT(IN)  :: PVEGTYPE_PATCH  !Fraction of nature point in npatch with nveg vegetation
!
!OUTPUT
INTEGER, DIMENSION(KSIZE_VEG), INTENT(OUT)   :: KMASK     !vegetation type to patch

!
!LOCAL
!
INTEGER                                  :: KK       ! Counter for points in vegetation vector
INTEGER                                  :: JJ       ! Counter for points in patch vector
INTEGER                                  :: II       ! Point in nature vector corresponding to JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_VEGTYPE_2_PATCH_MASK',0,ZHOOK_HANDLE)
KMASK(:)              = 0

KK=1  !First point of vegetation-vector
DO JJ=1,KSIZE_PATCH                  !Number of points in the patch in question
   II=KMASK_PATCH_NATURE(JJ)         !Nature-index corresponding to the point in question
   IF(PVEGTYPE_PATCH(II,KVEGTYPE)>0.)THEN
      KMASK(KK)=JJ
      KK=KK+1
   ENDIF
ENDDO  !Loop on points in patch vector

IF(KK-1.ne.KSIZE_VEG) THEN
  WRITE(KLUOUT,*) "ERROR in routine GET_VEGTYPE_2_PATCH_MASK"
  WRITE(KLUOUT,*) "problem in number of vegetation types"
  WRITE(KLUOUT,*) "KK-1     =", KK-1
  WRITE(KLUOUT,*) "KSIZE_VEG=", KSIZE_VEG
  CALL ABOR1_SFX('GET_VEGTYPE_2_PATCH_MASK: WRONG NUMBER OF VEGETATION TYPES')
END IF
IF (LHOOK) CALL DR_HOOK('GET_VEGTYPE_2_PATCH_MASK',1,ZHOOK_HANDLE)

END SUBROUTINE GET_VEGTYPE_2_PATCH_MASK
