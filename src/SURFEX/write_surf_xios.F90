!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE WRITE_SURF_XIOS(HREC, HCOMMENT, KFIELD, PFIELD, OFIELD, HFIELD, PFIELD1, PFIELD2, PFIELD3, HAXIS, HAXIS2) 
!!
!!
!!     PURPOSE
!!     --------
!!
!!    Use XIOS API to provide usual behaviour for write_surf_xx routines, namely either :
!!          - 'write' a scalar , here by defining a NetCDF global attribute in 
!!            default Surfex diags file (be it a real, double, integer or string)
!!          - or 'write' a field, here using sfx_xios_send_block
!!
!!     Thanks to the routine called (sfx_xios_send_block) this routine can be called 
!!     both during XIOS context intialization phase (it will declare the field to XIOS 
!!     if needed) and afterwrads, for actual output
!!
!!     This routine will not work properly if more (or less) than one of the optional 
!!     field args is actually provided
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
#ifdef WXIOS
USE MODD_XIOS, ONLY           : LXIOS_DEF_CLOSED, COUTPUT_DEFAULT, YXIOS_DOMAIN, YPATCH_DIM_NAME
USE XIOS
!
USE MODI_SFX_XIOS_DECLARE_FIELD
USE MODI_SFX_XIOS_SEND_BLOCK
#endif
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX 
!
IMPLICIT NONE
!
!   Arguments
!
 CHARACTER(LEN=*),      INTENT(IN)            :: HREC     ! name of the field 
 CHARACTER(LEN=*),      INTENT(IN)            :: HCOMMENT !
!
INTEGER         ,      INTENT(IN), OPTIONAL :: KFIELD   ! value
REAL            ,      INTENT(IN), OPTIONAL :: PFIELD   ! value
REAL,DIMENSION(:),     INTENT(IN), OPTIONAL :: PFIELD1  ! value
REAL,DIMENSION(:,:)   ,INTENT(IN), OPTIONAL :: PFIELD2  ! value
REAL,DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PFIELD3  ! value
LOGICAL         ,      INTENT(IN), OPTIONAL :: OFIELD   ! value
 CHARACTER(LEN=*),      INTENT(IN), OPTIONAL :: HFIELD   ! value
 CHARACTER(LEN=*) ,     INTENT(IN), OPTIONAL :: HAXIS    ! Name of 2nd dimension. Not necessary even for a 2d field
 CHARACTER(LEN=*) ,     INTENT(IN) , OPTIONAL :: HAXIS2   ! Name of 3rd dimension
!
!  Local variables
!
 CHARACTER(LEN=1000) :: YAXIS, YAXIS2
 CHARACTER(LEN=1000) :: YNAME    
 CHARACTER(LEN=10)   :: YLVL
LOGICAL         :: GRET
INTEGER         :: JI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
#ifdef WXIOS
TYPE(xios_variable) :: var_hdl
TYPE(xios_file) :: file_hdl
#endif
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_XIOS',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
!$OMP SINGLE
!
IF (PRESENT(KFIELD) .OR. PRESENT(PFIELD) .OR. PRESENT(HFIELD) .OR. PRESENT(OFIELD)) THEN
  !
  ! Case of writing a scalar
  !
  IF (.NOT. LXIOS_DEF_CLOSED) THEN
    !
    ! ----------------------------------------------------------------------
    !  We are still in the XIOS init phase ->
    !  Create a NetCDF file attribute using an XIOS variable , in default  
    !  output file (if it is a valid file) , and set the value
    ! ----------------------------------------------------------------------
    !
!doesn't work for the moment
!    IF ( XIOS_IS_VALID_FILE(COUTPUT_DEFAULT)) THEN 
!
!      CALL XIOS_GET_HANDLE(COUTPUT_DEFAULT,file_hdl)
!      CALL XIOS_ADD_CHILD(file_hdl,var_hdl,HREC)
!
!      IF (PRESENT(KFIELD) ) THEN 
!        CALL XIOS_SET_ATTR(var_hdl,type="int")
!        GRET = XIOS_SETVAR(HREC,KFIELD)
!      ENDIF
!      IF (PRESENT(PFIELD)) THEN 
!        CALL XIOS_SET_ATTR(var_hdl,type="float")
!        GRET = XIOS_SETVAR(HREC,PFIELD)
!      ENDIF
!      IF (PRESENT(OFIELD)) THEN 
!        CALL XIOS_SET_ATTR(var_hdl,type="bool")
!        GRET = XIOS_SETVAR(HREC,OFIELD)
!      ENDIF
!      IF (PRESENT(HFIELD)) THEN 
!        CALL XIOS_SET_ATTR(var_hdl,type="string")
!        GRET = XIOS_SETVAR(HREC,HFIELD)
!      ENDIF
!
!    ENDIF

  ENDIF
  !
ELSE
  !
  ! The data is not a scalar, but a field. 
  !
  IF (PRESENT(PFIELD1)) THEN

    IF (.NOT. LXIOS_DEF_CLOSED ) THEN

      CALL SFX_XIOS_DECLARE_FIELD(HREC, YXIOS_DOMAIN, HCOMMENT=HCOMMENT)

    ELSE

!     IF (XIOS_IS_VALID_FIELD(HREC)) THEN 
!       IF (XIOS_FIELD_IS_ACTIVE(HREC)) THEN
      CALL SFX_XIOS_SEND_BLOCK(HREC,PFIELD=PFIELD1)
!       ENDIF
!     ENDIF

    ENDIF
    !
  ELSE IF (PRESENT(PFIELD2)) THEN 

    YAXIS=''
    IF (PRESENT(HAXIS)) YAXIS = TRIM(HAXIS)
    !
    ! Patch dimension is never used in output files
    !
    IF (TRIM(YAXIS) /= TRIM(YPATCH_DIM_NAME)) THEN 

      IF (.NOT. LXIOS_DEF_CLOSED ) THEN
        CALL SFX_XIOS_DECLARE_FIELD(HREC, YXIOS_DOMAIN, HAXIS=YAXIS, KLEV=SIZE(PFIELD2,2), HCOMMENT=HCOMMENT)
      ELSE
        CALL SFX_XIOS_SEND_BLOCK(HREC,PFIELD2=PFIELD2)
      ENDIF

    ELSE ! Per-patch fields are written through a loop, and with patch number suffix

      DO JI=1,SIZE(PFIELD2,2)

        IF (JI < 10) THEN 
          WRITE(YLVL,'(I1)') JI
        ELSE
          WRITE(YLVL,'(I2)') JI
        ENDIF
        YNAME=HREC//'_'//TRIM(YLVL)

        IF (.NOT. LXIOS_DEF_CLOSED ) THEN 
          CALL SFX_XIOS_DECLARE_FIELD(TRIM(YNAME), YXIOS_DOMAIN, HCOMMENT=HCOMMENT)
        ELSE
          CALL SFX_XIOS_SEND_BLOCK(TRIM(YNAME),PFIELD=PFIELD2(:,JI))
        ENDIF

      ENDDO

    ENDIF
    !
  ELSE IF (PRESENT(PFIELD3)) THEN 

    YAXIS=''
    IF (PRESENT(HAXIS)) YAXIS=TRIM(HAXIS)

    IF (TRIM(YAXIS) /= TRIM(YPATCH_DIM_NAME)) THEN 

      ! Assume that dimension 2 is patch number and iterate on it
      DO JI=1,SIZE(PFIELD3,2)
        IF (JI < 10) THEN 
          WRITE(YLVL,'(I1)') JI
        ELSE
          WRITE(YLVL,'(I2)') JI
        ENDIF
        YNAME=HREC//'_'//TRIM(YLVL)

        IF (.NOT. LXIOS_DEF_CLOSED ) THEN
          CALL SFX_XIOS_DECLARE_FIELD(YNAME, YXIOS_DOMAIN, HCOMMENT=HCOMMENT)
        ELSE
          CALL SFX_XIOS_SEND_BLOCK(YNAME,PFIELD2=PFIELD3(:,JI,:))
        ENDIF

      ENDDO

    ELSE

      IF (.NOT. LXIOS_DEF_CLOSED ) THEN 

        YAXIS2=''
        IF (PRESENT(HAXIS2)) YAXIS2=HAXIS2
        CALL SFX_XIOS_DECLARE_FIELD(HREC, YXIOS_DOMAIN, HAXIS=YAXIS, HAXIS2=YAXIS2, HCOMMENT=HCOMMENT,&
                                    KLEV=SIZE(PFIELD2,2), KLEV2=SIZE(PFIELD3,3))
      ELSE
        CALL SFX_XIOS_SEND_BLOCK(YNAME,PFIELD3=PFIELD3)
      ENDIF

    ENDIF

  ENDIF

ENDIF
!
!$OMP END SINGLE
#endif
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_XIOS',1,ZHOOK_HANDLE)
! ----------------------------------------------------------------------
!
END SUBROUTINE WRITE_SURF_XIOS
