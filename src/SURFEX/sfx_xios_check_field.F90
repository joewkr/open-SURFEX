!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_CHECK_FIELD(U, HREC, HCOMMENT, OWRITE, PFIELD1, PFIELD2, PFIELD3, HAXIS)
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Ensure that a field is already declared to Xios.  
!!     If not, declare it using HREC, and declare it in a default 
!!        output file if this file is enabled ; 
!!     If 'units' or 'name' attribute is not defined using XIOS config 
!!        files , use HCOMMENT to declare it
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
USE MODD_XIOS,       ONLY     : YXIOS_DOMAIN, LXIOS_DEF_CLOSED, COUTPUT_DEFAULT
USE MODD_SURF_PAR,   ONLY     : XUNDEF
USE MODD_SURF_ATM_n, ONLY     : SURF_ATM_t
USE YOMHOOK    , ONLY         : LHOOK,   DR_HOOK
USE PARKIND1   , ONLY         : JPRB
!
#ifdef WXIOS
USE XIOS, ONLY                : XIOS_FIELD, XIOS_FIELDGROUP, XIOS_FILE, XIOS_GET_HANDLE,&
     XIOS_ADD_CHILD, XIOS_SET_ATTR, XIOS_IS_DEFINED_FIELD_ATTR, XIOS_SET_FIELD_ATTR,    &
     XIOS_GET_FILE_ATTR, XIOS_IS_VALID_FILE, XIOS_IS_VALID_FIELD, XIOS_IS_VALID_AXIS,   &
     XIOS_IS_DEFINED_FILE_ATTR
#endif
!
USE MODI_SFX_XIOS_CHECK_FIELD_2D
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!   Arguments
!
TYPE(SURF_ATM_t)  , INTENT(INOUT) :: U
CHARACTER(LEN=*)      ,INTENT(IN) :: HREC     ! name of the field to check
CHARACTER(LEN=100)    ,INTENT(IN) :: HCOMMENT ! Comment string
LOGICAL               ,INTENT(OUT):: OWRITE   ! TRUE if no issue re. Xios for this field
REAL,DIMENSION(:)     ,INTENT(IN) , OPTIONAL :: PFIELD1  ! value
REAL,DIMENSION(:,:)   ,INTENT(IN) , OPTIONAL :: PFIELD2  ! value
REAL,DIMENSION(:,:,:) ,INTENT(IN) , OPTIONAL :: PFIELD3  ! value
CHARACTER(LEN=*)      ,INTENT(IN) , OPTIONAL :: HAXIS    ! name of the additional axis
!
!  Local variables
!
LOGICAL            :: LISDEF, LLWRITE
INTEGER            :: IPO,IPF  
INTEGER            :: KSIZE  
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
#ifdef WXIOS
TYPE(xios_field) :: field_hdl, other_field_hdl
TYPE(xios_fieldgroup) :: fieldgroup_hdl
TYPE(xios_file) :: file_hdl
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_CHECK_FIELD',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
!
OWRITE=.TRUE.
!
! ----------------------------------------------------------------------
! If field is 3D , just give up 
! ----------------------------------------------------------------------
!
IF (PRESENT(PFIELD3)) THEN 
   CALL ABOR1_SFX('THIS XIOS INTERFACE CANNOT YET HANDLE 2 AXES IN ADDITION TO HORIZONTAL SPACE AXES')
ENDIF
!
! ----------------------------------------------------------------------
!  Check consistency between field size and current domain size
! ----------------------------------------------------------------------
!
IF ((PRESENT(PFIELD1)) .OR. (PRESENT(PFIELD2)) .OR. (PRESENT(PFIELD3)) ) THEN 
   IF (PRESENT(PFIELD1)) KSIZE=SIZE(PFIELD1)
   IF (PRESENT(PFIELD2)) KSIZE=SIZE(PFIELD2,1)
   IF (PRESENT(PFIELD3)) KSIZE=SIZE(PFIELD3,1)
   IF ((YXIOS_DOMAIN=='FULL'  ) .AND. (KSIZE /= U%NSIZE_FULL  )) OWRITE=.FALSE.
   IF ((YXIOS_DOMAIN=='NATURE') .AND. (KSIZE /= U%NSIZE_NATURE)) OWRITE=.FALSE.
   IF ((YXIOS_DOMAIN=='SEA   ') .AND. (KSIZE /= U%NSIZE_SEA   )) OWRITE=.FALSE.
   IF ((YXIOS_DOMAIN=='WATER ') .AND. (KSIZE /= U%NSIZE_WATER )) OWRITE=.FALSE.
   IF ((YXIOS_DOMAIN=='TOWN  ') .AND. (KSIZE /= U%NSIZE_TOWN  )) OWRITE=.FALSE.
   IF (.NOT. OWRITE) THEN 
      IF (.NOT. LXIOS_DEF_CLOSED) THEN 
         CALL ABOR1_SFX('SFX_XIOS_CHECK_FIELD : Inconsistent size for field '//HREC//' on domain '//YXIOS_DOMAIN)
      ENDIF
      IF (LHOOK) CALL DR_HOOK('SFX_XIOS_CHECK_FIELD',1,ZHOOK_HANDLE)
      RETURN
   ENDIF
ENDIF
!
! ----------------------------------------------------------------------
!   If XIOS init phase is over, just check if field is known to XIOS 
!   and returns
! ----------------------------------------------------------------------
!
IF (LXIOS_DEF_CLOSED) THEN
!$OMP SINGLE
   OWRITE=XIOS_IS_VALID_FIELD(HREC)
!$OMP END SINGLE
   IF (LHOOK) CALL DR_HOOK('SFX_XIOS_CHECK_FIELD',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
!$OMP SINGLE
!
! ----------------------------------------------------------------------
!  We are still in the XIOS init phase =>  Define field if necessary 
! ----------------------------------------------------------------------
!
OWRITE=.FALSE.
IF (.NOT. XIOS_IS_VALID_FIELD(HREC))  THEN
   CALL XIOS_GET_HANDLE("field_definition",fieldgroup_hdl)
   CALL XIOS_ADD_CHILD(fieldgroup_hdl,field_hdl,HREC)
   ! Inherit default values from 'default_field' 
   IF (.NOT. XIOS_IS_VALID_FIELD("default_field")) &
        CALL ABOR1_SFX('sfx_xios_check_field:cannot output field '//HREC//' : no default_field is defined')
   ! With XIOS2, next call creates an issue
   ! CALL XIOS_SET_ATTR(field_hdl,field_ref="default_field",name=HREC)
   CALL XIOS_SET_ATTR(field_hdl,name=HREC)
   !
ELSE 
   CALL XIOS_GET_HANDLE(HREC,field_hdl)
ENDIF
!
! ----------------------------------------------------------------------
! If field enabling is not defined, set it to TRUE
! ----------------------------------------------------------------------
!
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,enabled=LISDEF) 
IF ( .NOT. LISDEF ) CALL XIOS_SET_FIELD_ATTR(HREC, enabled=.TRUE.)
!
! ----------------------------------------------------------------------
! If field attribute 'domain' is not defined, set it
! ----------------------------------------------------------------------
!
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,domain_ref=LISDEF) 
IF ( .NOT. LISDEF ) THEN 
   CALL XIOS_SET_FIELD_ATTR(HREC, domain_ref=YXIOS_DOMAIN)
ENDIF
!
! ----------------------------------------------------------------------
!  2d fields are a special case, and may lead to (implicit) recursion
! ----------------------------------------------------------------------
!
IF (PRESENT(PFIELD2)) THEN 
   IF (PRESENT(HAXIS)) THEN 
      CALL SFX_XIOS_CHECK_FIELD_2D(U, HREC, HCOMMENT, OWRITE, PFIELD2, HAXIS)
   ELSE
      CALL SFX_XIOS_CHECK_FIELD_2D(U, HREC, HCOMMENT, OWRITE, PFIELD2)
   ENDIF
ENDIF
!
! ----------------------------------------------------------------------
! If NetCDF variable name is not defined , set it
! ----------------------------------------------------------------------
!
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,name=LISDEF) 
IF ( .NOT. LISDEF ) THEN 
   CALL XIOS_SET_FIELD_ATTR(HREC, name=HREC)
ENDIF
!
! ----------------------------------------------------------------------
! If field attribute 'long_name' is not defined or empty, set it 
! ----------------------------------------------------------------------
!
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,long_name=LISDEF) 
IF ( .NOT. LISDEF .AND. (TRIM(HCOMMENT) /= '') ) THEN 
   CALL XIOS_SET_FIELD_ATTR(HREC,long_name=TRIM(HCOMMENT))
ENDIF
!
!
! ------------------------------------------------------------------------
! If field attribute 'units' is not defined or empty, try to guess a value 
! from HCOMMENT (using rightmost string between parenthesis)
! ------------------------------------------------------------------------
!
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,unit=LISDEF)  
IF ( .NOT. LISDEF ) THEN 
   IPO=INDEX(HCOMMENT,"(",.TRUE.)
   IPF=INDEX(HCOMMENT,")",.TRUE.)
   IF ( (IPO > 0) .AND. (IPF>IPO+1) ) THEN
      CALL XIOS_SET_FIELD_ATTR(HREC,unit=HCOMMENT(IPO+1:IPF-1))
   ENDIF
ENDIF
!

! ----------------------------------------------------------------------
! Set default value to Surfex's one
! ----------------------------------------------------------------------
!
CALL XIOS_SET_FIELD_ATTR(HREC,default_value=XUNDEF)
!
! ----------------------------------------------------------------------
! If file 'default_ouput is enabled, add field to it
! ----------------------------------------------------------------------
!
IF ( XIOS_IS_VALID_FILE(COUTPUT_DEFAULT)) THEN 
   CALL XIOS_GET_HANDLE(COUTPUT_DEFAULT,file_hdl)
   CALL XIOS_IS_DEFINED_FILE_ATTR(COUTPUT_DEFAULT,enabled=LISDEF) 
   IF (LISDEF ) CALL XIOS_GET_FILE_ATTR(COUTPUT_DEFAULT,enabled=LISDEF)
   IF (LISDEF) THEN 
      CALL XIOS_ADD_CHILD(file_hdl,field_hdl)
      CALL XIOS_SET_ATTR(field_hdl,field_ref=HREC)
   ENDIF
ELSE
   CALL ABOR1_SFX('sfx_xios_check_field : cannot output field '//HREC//' : no default_output file is defined')
ENDIF
!
!
!$OMP END SINGLE
#else
OWRITE=.FALSE.
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_CHECK_FIELD',1,ZHOOK_HANDLE)
! ----------------------------------------------------------------------
!
END SUBROUTINE SFX_XIOS_CHECK_FIELD
