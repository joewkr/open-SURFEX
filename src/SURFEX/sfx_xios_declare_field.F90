!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_DECLARE_FIELD(HREC, HDOMAIN, HAXIS, KLEV, HAXIS2, KLEV2, HCOMMENT ,KFREQOP) 
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Declare field HREC and some attributes to XIOS if needed 

!!      If 'units' or 'long_name' attribute is not defined using XIOS
!!     config files , use HCOMMENT to declare it. Same for domain and
!!     other axis, either using relevant args or with default values
!!
!!     If haxis si provided and is the name of dimension 'patch' and
!!     haxis2 is not provided, rather proceed by a loop of 2D
!!     fields declarations
!!  
!!     IMPLICIT ARGUMENTS :
!!     -------------------- 
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
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
!!     Original    03/2016
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_XIOS,       ONLY     : LXIOS_DEF_CLOSED, YPATCH_DIM_NAME, NBASE_XIOS_FREQ
USE MODD_SURF_PAR,   ONLY     : XUNDEF

#ifdef WXIOS
USE XIOS
#endif
!
USE MODI_SET_AXIS
USE MODI_ABOR1_SFX
!
USE YOMHOOK, ONLY  : LHOOK,   DR_HOOK
USE PARKIND1, ONLY : JPRB, JPIM
!
IMPLICIT NONE

!
!   Arguments
!
 CHARACTER(LEN=*)   ,INTENT(IN)            :: HREC     ! field id
 CHARACTER(LEN=*)   ,INTENT(IN), OPTIONAL  :: HDOMAIN  ! name of the horiz domain
 CHARACTER(LEN=*)   ,INTENT(IN), OPTIONAL  :: HAXIS    ! name of the additional axis
INTEGER             ,INTENT(IN), OPTIONAL  :: KLEV     ! Axis size 
 CHARACTER(LEN=*)   ,INTENT(IN), OPTIONAL  :: HAXIS2   ! name of second additional axis
INTEGER            ,INTENT(IN), OPTIONAL  :: KLEV2    ! Second axis size
 CHARACTER(LEN=*)   ,INTENT(IN), OPTIONAL  :: HCOMMENT ! Comment string a la Surfex
INTEGER(KIND=JPIM)  ,INTENT(IN), OPTIONAL  :: KFREQOP  ! Sampling frequency, in minutes
!
 CHARACTER(1000)    :: YLDOMAIN
 CHARACTER(1000)    :: YLCOMMENT
 CHARACTER(1000)    :: YAXIS,YAXIS2
 CHARACTER(3)       :: YIDIM
!
INTEGER(KIND=JPIM) :: IFREQOP  ! Sampling frequency, in minutes
INTEGER(KIND=JPIM) :: IIDIM, ILEV, ILEV2
LOGICAL            :: GGRIDDEF
!
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
!
! ----------------------------------------------------------------------
!   If XIOS init phase is over, just returns
! ----------------------------------------------------------------------
!
IF (LXIOS_DEF_CLOSED) THEN
   IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD_INTERNAL',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
YLDOMAIN='FULL'
IF (PRESENT(HDOMAIN)) YLDOMAIN=TRIM(HDOMAIN)
YLCOMMENT=''
IF (PRESENT(HCOMMENT)) YLCOMMENT=TRIM(HCOMMENT)
IFREQOP=0
IF (PRESENT(KFREQOP)) IFREQOP=KFREQOP
ILEV=0          
IF (PRESENT(KLEV))     ILEV=KLEV
ILEV2=0         
IF (PRESENT(KLEV2))    ILEV2=KLEV2
YAXIS=''        
IF (PRESENT(HAXIS))    YAXIS=TRIM(HAXIS)
YAXIS2=''       
IF (PRESENT(HAXIS2))   YAXIS2=TRIM(HAXIS2)
!
IF (PRESENT(HAXIS) .AND. (YAXIS==TRIM(YPATCH_DIM_NAME)) .AND. .NOT. PRESENT(HAXIS2)) THEN
  ! For historical reason, in that case, a special treatment for
  ! avoiding that 'patch' dimension (provided as 1st dimension) is
  ! actually used : proceed by declaring a set of individual arrays
  IF ( ILEV == 0 ) CALL XIOS_GET_AXIS_ATTR(HAXIS, n_glo=ILEV)
  DO IIDIM=1,ILEV
    IF ( IIDIM < 10 ) THEN 
      WRITE(YIDIM,'(I1)') IIDIM
    ELSE
      IF ( IIDIM < 100 ) THEN 
        WRITE(YIDIM,'(I2)') IIDIM
      ELSE
        WRITE(YIDIM,'(I2)') IIDIM
      ENDIF
    ENDIF
    !write(0,*) '<field id="'//trim(HREC)//'_'//TRIM(YIDIM)//'", domain_ref="'//trim(CLDOMAIN)//'" />'
    CALL SFX_XIOS_DECLARE_FIELD_INTERNAL(TRIM(HREC)//'_'//TRIM(YIDIM), YLDOMAIN, YLCOMMENT, IFREQOP)
  END DO
  !
ELSE
  !
  ! Standard case
  ! 
  CALL SFX_XIOS_DECLARE_FIELD_INTERNAL(HREC, YLDOMAIN, YLCOMMENT, IFREQOP)
  IF (PRESENT(HAXIS))  CALL SFX_XIOS_DECLARE_AXIS_INTERNAL(HREC,YAXIS,ILEV)
  IF (PRESENT(HAXIS2)) CALL SFX_XIOS_DECLARE_AXIS_INTERNAL(HREC,YAXIS2,ILEV2,OSECOND=.TRUE.)
ENDIF
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD',1,ZHOOK_HANDLE)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


CONTAINS 

SUBROUTINE SFX_XIOS_DECLARE_FIELD_INTERNAL(HREC, HDOMAIN, HCOMMENT , KFREQOP) 
USE MODD_XIOS,       ONLY     : COUTPUT_DEFAULT
USE MODD_SURF_PAR,   ONLY     : XUNDEF
USE YOMHOOK    , ONLY         : LHOOK,   DR_HOOK
USE PARKIND1   , ONLY         : JPRB, JPIM
!
#ifdef WXIOS
USE XIOS
#endif
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!   Arguments
!
CHARACTER(LEN=*)   ,INTENT(IN)  :: HREC     ! field id
CHARACTER(LEN=*)   ,INTENT(IN)  :: HDOMAIN  ! name of the horiz domain
CHARACTER(LEN=*)   ,INTENT(IN)  :: HCOMMENT ! Comment string a la Surfex
INTEGER(KIND=JPIM) ,INTENT(IN)  :: KFREQOP  ! Sampling frequency, in minutes
!
!  Local variables
!
LOGICAL            :: GISDEF, GGRIDDEF
INTEGER            :: IPO,IPF
!
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
#ifdef WXIOS
TYPE(xios_field)      :: field_hdl, other_field_hdl
TYPE(xios_fieldgroup) :: fieldgroup_hdl
TYPE(xios_file)       :: file_hdl
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
!
!$OMP SINGLE
!
! ----------------------------------------------------------------------
!  We are still in the XIOS init phase =>  Define field if necessary 
! ----------------------------------------------------------------------
!
IF (.NOT. XIOS_IS_VALID_FIELD(HREC))  THEN
        
  CALL XIOS_GET_HANDLE("field_definition",fieldgroup_hdl)
  CALL XIOS_ADD_CHILD(fieldgroup_hdl,field_hdl,HREC)
  !IF (.NOT. XIOS_IS_VALID_FIELD("default_field")) &
  !    CALL ABOR1_SFX('sfx_xios_check_field:cannot output field '//HREC//' : no default_field is defined')
  CALL XIOS_SET_ATTR(field_hdl,name=HREC)
  !
  ! ----------------------------------------------------------------------
  ! If default_ouput file is defined, add this field to it
  ! ----------------------------------------------------------------------
  !
  IF ( XIOS_IS_VALID_FILE(COUTPUT_DEFAULT)) THEN 
    CALL XIOS_GET_HANDLE(COUTPUT_DEFAULT,file_hdl)
    CALL XIOS_ADD_CHILD(file_hdl,field_hdl)
    CALL XIOS_SET_ATTR(field_hdl,field_ref=HREC)
  ENDIF

ENDIF
!
! ----------------------------------------------------------------------
!  If field attribute 'domain' is not defined, set it
! ----------------------------------------------------------------------
!
 CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,grid_ref=GGRIDDEF) 
 CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,domain_ref=GISDEF) 
!
IF (  .NOT. GISDEF .AND. .NOT. GGRIDDEF ) THEN 
  IF (TRIM(HDOMAIN)=='') &
        CALL ABOR1_SFX('SFX_XIOS_DECLARE_FIELD_INTERNAL : MUST PROVIDE HDOMAIN '//HREC)
  !if (trim(hrec)=='PFRSO1') write(0,*) 'Setting domain for PFRSO1 !!!'
  CALL XIOS_SET_FIELD_ATTR(HREC, domain_ref=TRIM(HDOMAIN))
  !CALL XIOS_SET_FIELD_ATTR(HREC, grid_ref=TRIM(HDOMAIN))
ELSE
   !write(0,*) 'Field '//trim(hrec)//' already has a grid or domain:',GGRIDDEF,GISDEF
ENDIF
!
! ----------------------------------------------------------------------
! If prec  is not defined , set it to the provided value (def : timestep)
! ----------------------------------------------------------------------
!
! CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,name=GISDEF) 
!IF ( .NOT. GISDEF ) THEN 
!  CALL XIOS_SET_FIELD_ATTR(HREC, name=trim(HREC))
!ENDIF
!
! ------------------------------------------------------------------------
! If field attribute 'unit' is not defined or empty, try to guess a value 
! from HCOMMENT (using rightmost string between parenthesis)
! ------------------------------------------------------------------------
!
 CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,unit=GISDEF)
IF ( .NOT. GISDEF ) THEN 
   IPO=INDEX(HCOMMENT,"(",.TRUE.)
   IPF=INDEX(HCOMMENT,")",.TRUE.)
   IF ( (IPO > 0) .AND. (IPF>IPO+1) ) THEN
      CALL XIOS_SET_FIELD_ATTR(HREC,unit=HCOMMENT(IPO+1:IPF-1))
   ENDIF
ENDIF
!
! ----------------------------------------------------------------------
! If field attribute 'long_name' is not defined or empty, set it 
! ----------------------------------------------------------------------
!
 CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,long_name=GISDEF) 
IF ( .NOT. GISDEF .AND. (TRIM(HCOMMENT) /= '') ) THEN 
  IF (IPO > 1) THEN 
    CALL XIOS_SET_FIELD_ATTR(HREC,long_name=TRIM(HCOMMENT(1:IPO-1)))
  ELSE
    CALL XIOS_SET_FIELD_ATTR(HREC,long_name=TRIM(HCOMMENT(:)))
  ENDIF
ENDIF
!
! ----------------------------------------------------------------------
! Set default value to Surfex's one
! ----------------------------------------------------------------------
!
 CALL XIOS_SET_FIELD_ATTR(HREC,default_value=XUNDEF)
!
!$OMP END SINGLE
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD_INTERNAL',1,ZHOOK_HANDLE)
! ----------------------------------------------------------------------
!
END SUBROUTINE SFX_XIOS_DECLARE_FIELD_INTERNAL

SUBROUTINE SFX_XIOS_DECLARE_AXIS_INTERNAL(HREC, HAXIS, KLEV, OSECOND) 
!
USE YOMHOOK    , ONLY         : LHOOK,   DR_HOOK
USE PARKIND1   , ONLY         : JPRB
!
#ifdef WXIOS
USE XIOS
#endif
!
USE MODI_ABOR1_SFX
!
!
IMPLICIT NONE
!
!   Arguments
!
CHARACTER(LEN=*)   ,INTENT(IN) :: HREC     ! field id
CHARACTER(LEN=*)   ,INTENT(IN) :: HAXIS    ! axis name 
INTEGER            ,INTENT(IN) :: KLEV     ! axis size
LOGICAL            ,INTENT(IN),OPTIONAL :: OSECOND  ! Is it a second axis
!
!  Local variables
!
LOGICAL            :: GISDEF, GGRIDDEF, GVALID_AXIS
CHARACTER(1000)    :: YAXIS
INTEGER            :: INGLO
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_AXIS_INTERNAL',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
 CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,grid_ref=GGRIDDEF) 
IF (.NOT. GGRIDDEF ) THEN 
  ! If an axis is already declared, just do nothing, except
  ! if it is second call
  CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,axis_ref=GISDEF) 
  IF ( .NOT. GISDEF .OR. PRESENT(OSECOND)) THEN
    IF ( TRIM(HAXIS) == '')  THEN
      GVALID_AXIS=.FALSE.
      IF (PRESENT(OSECOND)) THEN
        YAXIS='dim2_for_'//TRIM(HREC)
      ELSE
        YAXIS='dim_for_'//TRIM(HREC)
      ENDIF
    ELSE
      GVALID_AXIS=XIOS_IS_VALID_AXIS(trim(HAXIS))
      YAXIS=TRIM(HAXIS)
    ENDIF
    IF (.NOT. GVALID_AXIS) THEN 
       IF ( KLEV /= 0) THEN 
          CALL SET_AXIS(TRIM(HAXIS),KSIZE=KLEV)
          !write(0,*) 'calling set_axis for '//trim(yaxis)//" "//HREC ; call flush(0)
       ELSE
          CALL ABOR1_SFX('SFX_XIOS_DECLARE_FIELD:SFX_XIOS_DECLARE_AXIS_INTERNAL'//&
               ': MUST PROVIDE KLEV OR AN ALREADY DECLARED HAXIS for '//HREC)
       ENDIF
    ENDIF
    CALL XIOS_SET_FIELD_ATTR(HREC, axis_ref=TRIM(YAXIS))
  ELSE
    !write(0,*) 'An axis is already defined for '//HREC ; call flush(0)
  ENDIF
ELSE
   !write(0,*) 'A grid is already defined for '//HREC ; call flush(0)
ENDIF
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_AXIS_INTERNAL',1,ZHOOK_HANDLE)
!
END SUBROUTINE SFX_XIOS_DECLARE_AXIS_INTERNAL

END SUBROUTINE SFX_XIOS_DECLARE_FIELD
