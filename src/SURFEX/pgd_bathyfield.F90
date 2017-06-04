!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_BATHYFIELD (UG, U, USS, &
                                 HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,&
                                  HNCVARNAME,PUNIF,PFIELD)  
!     ##############################################################
!
!!**** *PGD_FIELD* monitor for averaging and interpolations of ISBA physiographic fields
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!    C. Lebeaupin Brossier        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2008
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_PGDWORK,        ONLY : XALL, NSIZE_ALL, NSIZE, XSUMVAL
!
USE MODI_GET_LUOUT
USE MODI_TREAT_BATHYFIELD
USE MODI_INTERPOL_FIELD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
 CHARACTER(LEN=*),  INTENT(IN) :: HFIELD    ! field name for prints
 CHARACTER(LEN=3),  INTENT(IN) :: HAREA     ! area where field is defined
!                                          ! 'ALL' : everywhere
!                                          ! 'NAT' : on nature
!                                          ! 'TWN' : on town
!                                          ! 'SEA' : on sea
!                                          ! 'WAT' : on inland waters
!                                          ! 'LAN' : on nature + on town
 CHARACTER(LEN=28), INTENT(IN) :: HFILE     ! data file name
 CHARACTER(LEN=6),  INTENT(IN) :: HFILETYPE ! data file type
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME! variable name to read
REAL,              INTENT(IN) :: PUNIF     ! prescribed uniform value for field
REAL, DIMENSION(:),INTENT(OUT):: PFIELD    ! physiographic field
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ILUOUT    ! output listing logical unit
!
 CHARACTER(LEN=20) :: YFIELD
INTEGER :: JLOOP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_BATHYFIELD',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*    2.      Output listing logical unit
!             ---------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    3.2     No data
!             -------
!
IF (LEN_TRIM(HFILE)/=0) THEN
!        
!-------------------------------------------------------------------------------
!
!*    3.      Averages the field
!             ------------------
!
  ALLOCATE(NSIZE_ALL (U%NDIM_FULL,1))
  ALLOCATE(XALL      (U%NDIM_FULL,1,1))
!
  NSIZE_ALL(:,:) = 0
  XALL   (:,:,:) = 0.
!
  YFIELD = '                    '
  YFIELD = HFIELD(1:MIN(LEN(HFIELD),20))
!
  PFIELD(:) = XUNDEF

  CALL TREAT_BATHYFIELD(UG, U, USS, &
                        HPROGRAM,'SURF  ',HFILETYPE,'A_MESH',HFILE, HNCVARNAME,&
                     YFIELD,PFIELD,HAREA                           )  
!
!-------------------------------------------------------------------------------
!
!*    4.      Mask for the interpolations
!             ---------------------------
!
  SELECT CASE (HAREA)
    CASE ('LAN')
      WHERE (U%XTOWN(:)+U%XNATURE(:)==0. .AND. NSIZE(:,1)==0 ) NSIZE(:,1) = -1
    CASE ('TWN')
      WHERE (U%XTOWN  (:)==0. .AND. NSIZE(:,1)==0 ) NSIZE(:,1) = -1
    CASE ('NAT')
      WHERE (U%XNATURE(:)==0. .AND. NSIZE(:,1)==0 ) NSIZE(:,1) = -1
    CASE ('SEA')
      WHERE (U%XSEA   (:)==0. .AND. NSIZE(:,1)==0 ) NSIZE(:,1) = -1
    CASE ('WAT')
      WHERE (U%XWATER (:)==0. .AND. NSIZE(:,1)==0 ) NSIZE(:,1) = -1

  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    5.      Interpolation if some points are not initialized (no data for these points)
!             ------------------------------------------------
!
  CALL INTERPOL_FIELD(UG, U, &
                      HPROGRAM,ILUOUT,NSIZE(:,1),PFIELD(:),HFIELD)
!
  DO JLOOP=1,SIZE(PFIELD)
   PFIELD(JLOOP)=MIN(PFIELD(JLOOP),-1.)
  ENDDO
  DEALLOCATE(NSIZE    )
  DEALLOCATE(XSUMVAL  )
!
!-------------------------------------------------------------------------------
!
!
!*    3.      Uniform field is prescribed
!             ---------------------------
!
!
ELSEIF (PUNIF/=XUNDEF) THEN
!
!*    3.1     Use of the presribed field
!             --------------------------
!
  PFIELD(:) = PUNIF
!
ELSE
!
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of field : ', HFIELD
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_BATHYFIELD: NO PRESCRIBED VALUE NOR INPUT FILE FOR '//HFIELD)
!
END IF
!-------------------------------------------------------------------------------
!
!*    6.      Mask for the field
!             ------------------
!
SELECT CASE (HAREA)
  CASE ('LAN')
    WHERE (U%XTOWN(:)+U%XNATURE(:)==0.) PFIELD(:) = XUNDEF
  CASE ('TWN')
    WHERE (U%XTOWN  (:)==0.) PFIELD(:) = XUNDEF
  CASE ('NAT')
    WHERE (U%XNATURE(:)==0.) PFIELD(:) = XUNDEF
  CASE ('SEA')
    WHERE (U%XSEA   (:)==0.) PFIELD(:) = XUNDEF
  CASE ('WAT')
    WHERE (U%XWATER (:)==0.) PFIELD(:) = XUNDEF

END SELECT
IF (LHOOK) CALL DR_HOOK('PGD_BATHYFIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_BATHYFIELD
