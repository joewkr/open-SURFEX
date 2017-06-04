!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SNAP_n (HSELECT, CHN, HPROGRAM)
!     #######################################################################
!
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
USE MODD_CH_SNAP_n, ONLY : CH_EMIS_SNAP_t
!
USE MODI_GET_LUOUT
USE MODI_WRITE_SURF
USE MODI_WRITE_SURF_FIELD2D
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
TYPE(CH_EMIS_SNAP_t), INTENT(INOUT) :: CHN
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
!
!*       0.2   declarations of local variables
!
 CHARACTER(LEN=16)  :: YRECFM   ! article name
 CHARACTER(LEN=100) :: YCOMMENT ! comment
 CHARACTER(LEN=100) :: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
!
INTEGER             :: IRESP    ! I/O error code
INTEGER             :: ILUOUT   ! Unit number for prints
INTEGER             :: JSPEC    ! Loop index for emission species
INTEGER             :: JSNAP    ! Loop index for SNAP categories
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_SNAP_n',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
YCOMMENT = ""
!
YRECFM='EMISPEC_NBR'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,CHN%NEMIS_NBR,IRESP,YCOMMENT)
YRECFM='SNAP_NBR'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,CHN%NEMIS_SNAP,IRESP,YCOMMENT)
YRECFM='SNAP_TIME'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,CHN%CSNAP_TIME_REF,IRESP,YCOMMENT)
!
IF (CHN%CSNAP_TIME_REF=='LEGAL') THEN
  YRECFM='LEGALTIME'
  CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,CHN%XDELTA_LEGAL_TIME(:),IRESP,YCOMMENT)
END IF
!-------------------------------------------------------------------------------
!
DO JSPEC=1,CHN%NEMIS_NBR
! Writes the name of species
  WRITE(YRECFM,'("EMISNAME",I3.3)') JSPEC
  YCOMMENT = CHN%CEMIS_COMMENT(JSPEC)
  CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,CHN%CEMIS_NAME(JSPEC),IRESP,YCOMMENT)
!
! Writes the temporal profiles of all snaps
  YCOMMENTUNIT='-'
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_M"
  CALL WRITE_SURF_FIELD2D(HSELECT, HPROGRAM,CHN%XSNAP_MONTHLY(:,:,JSPEC),YRECFM,&
                          YCOMMENT,YCOMMENTUNIT,HDIR='-',HNAM_DIM="Nemis_snap      ")  
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_D"
  CALL WRITE_SURF_FIELD2D(HSELECT, HPROGRAM,CHN%XSNAP_DAILY(:,:,JSPEC),YRECFM,&
                          YCOMMENT,YCOMMENTUNIT,HDIR='-',HNAM_DIM="Nemis_snap      ")    
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_H"
  CALL WRITE_SURF_FIELD2D(HSELECT, HPROGRAM,CHN%XSNAP_HOURLY(:,:,JSPEC),YRECFM,&
                          YCOMMENT,YCOMMENTUNIT,HDIR='-',HNAM_DIM="Nemis_snap      ")    
! Writes the potential emission of species for each snap
  DO JSNAP=1,CHN%NEMIS_SNAP
    WRITE(YRECFM,'("SN",I2.2,"_",A7)') JSNAP,CHN%CEMIS_NAME(JSPEC)
    CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,CHN%XEMIS_FIELDS_SNAP(:,JSNAP,JSPEC),IRESP,YCOMMENT)
  END DO
!
END DO
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_SNAP_n',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SNAP_n
