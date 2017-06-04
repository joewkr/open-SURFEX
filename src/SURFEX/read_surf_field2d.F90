!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SURF_FIELD2D( HPROGRAM,PFIELD2D,HFIELDNAME,HCOMMENT,HDIR,KRESP)
!     #####################################
!
!!****  *READ_SURF_FIELD2D* - reads surfex field in input file using READ_SURF,
!!                           patch by patch if needed in MESONH
!!                           with Z-parallel IO in MESO-NH, we force surfex to write 2D fields
!!                           because Z-parallel IO are not supported for 2D SURFEX fields.
!!                        
!!
!!    PURPOSE
!!    -------
!!      reads surfex field in output file using WRITE_SURF,
!!      patch by patch if needed in MESONH
!!      and NB_PROCIO_R > 1
!!      examples of HFIELDNAME : 'TG', 'soil depth from ecoclimap'
!!      with Z-parallel IO in MESO-NH, we force surfex to write 2D fields
!!      because Z-parallel IO are not supported for 2D SURFEX fields.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      M.Moge   *LA - UPS*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/01/2016
!!      J.Escobar : 20/04/2016 : Pb IOZ/NETCDF , add optional KRESP error code
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
USE MODI_READ_SURF
#ifdef SFX_MNH
USE MODI_GET_NB_PROCIO_READ_MNH
#endif
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),                 INTENT(IN) :: HPROGRAM     ! calling program
REAL, DIMENSION(:,:),          INTENT(INOUT) :: PFIELD2D     ! 2D field to be read
CHARACTER(LEN=12),                INTENT(IN) :: HFIELDNAME   ! name of the field PFIELD2D. Example : 'X_Y_TG'
CHARACTER(LEN=*), OPTIONAL,      INTENT(OUT) :: HCOMMENT   !comment string
CHARACTER(LEN=1),OPTIONAL,        INTENT(IN) :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
INTEGER,OPTIONAL,               INTENT(OUT)  :: KRESP  
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(PFIELD2D,1)) :: ZWORK
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
INTEGER           :: IPATCH         ! number of patches in PFIELD2D
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=4 ) :: YPATCH         ! current patch
INTEGER           :: INB_PROCIO     ! number of processes used for Z-parallel IO with MESO-NH
!
CHARACTER(LEN=1)   :: YDIR
INTEGER :: JPATCH  ! loop counter on patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (PRESENT(KRESP)) KRESP = -1
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_FIELD2D',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
IPATCH = SIZE( PFIELD2D, 2 )
!
INB_PROCIO = 1
#ifdef SFX_MNH
IF (HPROGRAM=='MESONH') THEN
  CALL GET_NB_PROCIO_READ_MNH( INB_PROCIO, IRESP )
ENDIF
#endif
!
IF ( INB_PROCIO > 1 ) THEN
!
  DO JPATCH=1,IPATCH
    YRECFM=ADJUSTL(HFIELDNAME(:LEN_TRIM(HFIELDNAME)))
    WRITE(YPATCH,'(I4.4)') JPATCH
    IF ( IPATCH > 1 ) THEN
      YRECFM=ADJUSTL(YRECFM(:LEN_TRIM(YRECFM)))//YPATCH
    ENDIF
    CALL READ_SURF(HPROGRAM,YRECFM,PFIELD2D(:,JPATCH),IRESP,HCOMMENT=YCOMMENT,HDIR=YDIR)
  ENDDO
!
ELSE
!
  YRECFM=ADJUSTL(HFIELDNAME(:LEN_TRIM(HFIELDNAME)))
  CALL READ_SURF(HPROGRAM,YRECFM,PFIELD2D(:,:),IRESP,HCOMMENT=YCOMMENT,HDIR=YDIR)
!
ENDIF
!
IF (PRESENT(HDIR)) HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_FIELD2D',1,ZHOOK_HANDLE)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!-------------------------------------------------------------------------------
!
      END SUBROUTINE READ_SURF_FIELD2D
