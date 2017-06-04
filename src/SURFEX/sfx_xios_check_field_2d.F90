!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
 SUBROUTINE SFX_XIOS_CHECK_FIELD_2D(U, HREC, HCOMMENT, OWRITE, PFIELD2, HAXIS)
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Ensure that a 2-dim field is already declared to Xios.  
!!
!!
!!     IMPLICIT ARGUMENTS :
!!     -------------------- 
!!
!!     YXIOS_DOMAIN
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
!!     svn co -r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> 
!!       cd <dir>/doc ; ....
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
USE YOMHOOK    , ONLY         : LHOOK,   DR_HOOK
USE PARKIND1   , ONLY         : JPRB
!
USE MODD_SURF_ATM_n, ONLY     : SURF_ATM_t
!
#ifdef WXIOS
USE XIOS, ONLY                :  XIOS_SET_FIELD_ATTR, XIOS_IS_DEFINED_FIELD_ATTR, &
                                 XIOS_IS_VALID_AXIS, XIOS_GET_AXIS_ATTR, XIOS_IS_DEFINED_AXIS_ATTR
USE MODD_XIOS , ONLY          :  YPATCH_DIM_NAME
#endif
!
USE MODI_SET_AXIS
USE MODI_SFX_XIOS_CHECK_FIELD
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!   Arguments
!
TYPE(SURF_ATM_t)      ,INTENT(INOUT):: U
CHARACTER(LEN=*)      ,INTENT(IN)   :: HREC     ! name of the field to check
CHARACTER(LEN=100)    ,INTENT(IN)   :: HCOMMENT ! Comment string
LOGICAL               ,INTENT(INOUT):: OWRITE   ! set to .FALSE. if any issue re. Xios for this field
REAL,DIMENSION(:,:)   ,INTENT(IN)   :: PFIELD2  ! 2D field value
CHARACTER(LEN=*)      ,INTENT(IN) , OPTIONAL :: HAXIS    ! name of the additional axis
!
!  Local variables
!
LOGICAL            :: LISDEF, LLWRITE, LDEFUNITS
LOGICAL            :: LVALID_AXIS, LVERTICAL_AXIS
CHARACTER(LEN=100) :: YAXIS
CHARACTER(LEN=100) :: YUNITS
REAL(KIND=JPRB), DIMENSION(:),ALLOCATABLE :: ZAXIS
INTEGER            :: IDIM        ! for additonal dim
CHARACTER(LEN=3)   :: YIDIM       ! for additonal dim  
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_CHECK_FIELD_2D',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
!$OMP SINGLE
!
! Assume that every axis attribute is well defined as soon as the axis is defined
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,axis_ref=LISDEF) 
!
IF ( .NOT. LISDEF ) THEN 
   IF (.NOT. PRESENT(HAXIS) .OR. (TRIM(HAXIS)=='')) THEN
      YAXIS='you_should_define_a_dim_for_'//HREC//'_in_xml_file_or_call_set_axis_in_code'
   ELSE
      YAXIS=TRIM(HAXIS)
   ENDIF
   LVALID_AXIS=XIOS_IS_VALID_AXIS(YAXIS)
   !
   IF (TRIM(HAXIS)/=TRIM(YPATCH_DIM_NAME)) THEN
      IF (.NOT. LVALID_AXIS) THEN 
         ALLOCATE(ZAXIS(SIZE(PFIELD2,2)))
         ZAXIS=(/(IDIM, IDIM=1,SIZE(PFIELD2,2))/)
         CALL SET_AXIS(YAXIS,ZAXIS)
         DEALLOCATE(ZAXIS)
      ELSE 
         ! xxx il faut encore vérifier si le set_axis_attr des valeurs a été fait pour ce champ !!
      ENDIF
      CALL XIOS_SET_FIELD_ATTR(HREC, axis_ref=YAXIS)
   ELSE 
      ! Account for loop on pacthes
      DO IDIM=1,SIZE(PFIELD2,2)
         IF (IDIM < 10) THEN 
            WRITE(YIDIM,'(I1)') IDIM
         ELSE
            WRITE(YIDIM,'(I2)') IDIM
         ENDIF
         CALL SFX_XIOS_CHECK_FIELD(U, TRIM(HREC)//'_'//TRIM(YIDIM), HCOMMENT, LLWRITE, PFIELD1=PFIELD2(:,IDIM))
         OWRITE= OWRITE .AND. LLWRITE
      END DO
      ! Disable the writing as a mutli-dim array
      CALL XIOS_SET_FIELD_ATTR(HREC, enabled=.FALSE.)
   ENDIF
ENDIF
!
!$OMP END SINGLE
#endif
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_CHECK_FIELD_2D',1,ZHOOK_HANDLE)
! ----------------------------------------------------------------------
!
END SUBROUTINE SFX_XIOS_CHECK_FIELD_2D
