!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_CH_EMIS_n (HSELECT, CHE, HPROGRAM)
!     ##########################################################
!
!!****  *WRITESURF_CH_EMIS_n* - routine to write chemistry emission fields
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2004
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CH_EMIS_FIELD_n, ONLY : CH_EMIS_FIELD_t
!
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
!*       0.1   Declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
TYPE(CH_EMIS_FIELD_t), INTENT(INOUT) :: CHE
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    ! at the open of the file in LFI  routines 
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be written
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
 CHARACTER(LEN=80) :: YNAME          ! emitted species name
!
 CHARACTER(LEN=40),DIMENSION(CHE%NEMIS_NBR) :: YEMISPEC_NAMES
INTEGER,           DIMENSION(CHE%NEMIS_NBR) :: INBTIMES
INTEGER,           DIMENSION(CHE%NEMIS_NBR) :: IFIRST,ILAST,INEXT

INTEGER :: JI,JT          ! loop indices
INTEGER :: JSPEC          ! loop index
INTEGER :: INTIMESMAX,ITMP
INTEGER :: IEMISPEC_NBR
LOGICAL           :: GFOUND,LOK
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------
!
!*       1.     Chemical Emission fields :
!               --------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_CH_EMIS_N',0,ZHOOK_HANDLE)
YRECFM='EMISFILE_NBR'
YCOMMENT='Total number of 2D emission files.'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,CHE%NEMIS_NBR,IRESP,HCOMMENT=YCOMMENT)
!
! count emitted species 
IEMISPEC_NBR = 0
DO JI=1,CHE%NEMIS_NBR
  YNAME = TRIM(ADJUSTL(CHE%CEMIS_NAME(JI)))
  GFOUND = .FALSE.
  DO JSPEC = 1,IEMISPEC_NBR
    IF (YEMISPEC_NAMES(JSPEC) == YNAME) THEN
      GFOUND = .TRUE.
      EXIT
    END IF
  END DO
  IF (.NOT. GFOUND) THEN
    IEMISPEC_NBR = IEMISPEC_NBR+1
    YEMISPEC_NAMES(IEMISPEC_NBR) = YNAME
    INBTIMES(IEMISPEC_NBR) = 1
    IFIRST(IEMISPEC_NBR) = JI
    ILAST(IEMISPEC_NBR)  = JI
    INEXT(JI) = 0
  ELSE
    INEXT(ILAST(JSPEC)) = JI
    INEXT(JI)        = 0
    ILAST(JSPEC)        = JI
    INBTIMES(JSPEC) = INBTIMES(JSPEC)+1
  END IF
END DO
!
YRECFM='EMISPEC_NBR '
YCOMMENT='Number of emitted chemical species.'
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,IEMISPEC_NBR,IRESP,HCOMMENT=YCOMMENT)
!
IF (IEMISPEC_NBR > 0) THEN
  !
  DO JSPEC = 1,IEMISPEC_NBR
    CALL WRITE_EMIS_SPEC(INBTIMES(JSPEC))
  ENDDO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_CH_EMIS_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE WRITE_EMIS_SPEC(KSIZE)
!
INTEGER, INTENT(IN) :: KSIZE
INTEGER,DIMENSION(KSIZE) :: ITIME
INTEGER,DIMENSION(KSIZE) :: IINDEX
REAL,DIMENSION(SIZE(CHE%XEMIS_FIELDS,1),KSIZE) :: ZWORK2D
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_CH_EMIS_N:WRITE_EMIS_SPEC',0,ZHOOK_HANDLE)
!
JI = IFIRST(JSPEC)
JT = 0
! fill the emission times array (ITIME)
! and the corresponding indices array (IINDEX)
! for species number JSPEC
DO WHILE(JI /= 0)
  JT = JT+1
  ITIME(JT)  = CHE%NEMIS_TIME(JI)
  IINDEX(JT) = JI
  JI = INEXT(JI)
END DO
IF (JT /= KSIZE) THEN
  CALL ABOR1_SFX('WRITESURF_CH_EMISN: ABNORMAL ERROR')
END IF
! sort indices according to ITIME values
LOK = .TRUE.
DO WHILE (LOK)
  LOK = .FALSE.
  DO JI=2,KSIZE
    IF (ITIME(JI-1) > ITIME(JI)) THEN
      LOK = .TRUE.
      ITMP = ITIME(JI-1)
      ITIME(JI-1) = ITIME(JI)
      ITIME(JI)   = ITMP
      ITMP = IINDEX(JI-1)
      IINDEX(JI-1) = IINDEX(JI)
      IINDEX(JI)   = ITMP
    END IF
  END DO
END DO
! Now fill the ZWORK2D array for writing
ZWORK2D(:,:) = CHE%XEMIS_FIELDS(:,IINDEX(:))
! 
! Write NAME of species JSPEC with AREA and number of emission times 
! stored in the commentary
WRITE(YRECFM,'("EMISNAME",I3.3)') JSPEC
WRITE(YCOMMENT,'(A3,", emission times number:",I5)') CHE%CEMIS_AREA(IINDEX(1)),KSIZE
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,YEMISPEC_NAMES(JSPEC),IRESP,HCOMMENT=YCOMMENT)
! 
WRITE(YRECFM,'("EMISAREA",I3.3)') JSPEC
YCOMMENT = "Emission area" 
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,CHE%CEMIS_AREA(IINDEX(1)),IRESP,HCOMMENT=YCOMMENT)
!
WRITE(YRECFM,'("EMISNBT",I3.3)') JSPEC
YCOMMENT = "Emission times number" 
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,KSIZE,IRESP,HCOMMENT=YCOMMENT)

! Write emission times (ITIME) for species JSPEC
WRITE(YRECFM,'("EMISTIMES",I3.3)') JSPEC  
YCOMMENT = "Emission times in second"
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,ITIME(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-',HNAM_DIM="Temporal_emiss  ")
!
! Finally write emission data for species JSPEC
YRECFM = "E_"//TRIM(YEMISPEC_NAMES(JSPEC))
YCOMMENT = "Emission data (x,y,t),"//TRIM(CHE%CEMIS_COMMENT(IINDEX(1)))
YCOMMENTUNIT='-'
 CALL WRITE_SURF_FIELD2D(HSELECT, &
                  HPROGRAM,ZWORK2D(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT,HNAM_DIM="Temporal_emiss  ")
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_CH_EMIS_N:WRITE_EMIS_SPEC',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_EMIS_SPEC
!
END SUBROUTINE WRITESURF_CH_EMIS_n
