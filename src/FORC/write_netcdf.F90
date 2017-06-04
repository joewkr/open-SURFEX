!****************************************************************************
MODULE MODI_WRITE_NETCDF
!
!****************************************************************************
! Purpose: set of sebroutine to read and write subrooutine.
!-------------------------------------------------------------------------------------------------
!****************************************************************************
!
INTERFACE WRITE_NETCDF
!
    SUBROUTINE WRITE_NETCDF_CHARACTER_0D (KFILE_ID,HNAME,HLONG_NAME,HVAL,KDIM_ID1)
!
USE NETCDF
!
INTEGER,          INTENT(IN) :: KFILE_ID
INTEGER,          INTENT(IN) :: KDIM_ID1
CHARACTER(LEN=*), INTENT(IN) :: HNAME
CHARACTER(LEN=*), INTENT(IN) :: HLONG_NAME
CHARACTER(LEN=*), INTENT(IN) :: HVAL   
END SUBROUTINE WRITE_NETCDF_CHARACTER_0D
!
!-------------------------------------------------------------------------------------------------
    SUBROUTINE WRITE_NETCDF_CHARACTER_1D (KFILE_ID,HNAME,HLONG_NAME,HVAL,KDIM_ID1,KDIM_ID2)
!
USE NETCDF
!
INTEGER,                      INTENT(IN) :: KFILE_ID
INTEGER,                      INTENT(IN) :: KDIM_ID1,KDIM_ID2
CHARACTER(LEN=*),             INTENT(IN) :: HNAME
CHARACTER(LEN=*),             INTENT(IN) :: HLONG_NAME
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: HVAL   
END SUBROUTINE WRITE_NETCDF_CHARACTER_1D
!
!-------------------------------------------------------------------------------------------------
  SUBROUTINE WRITE_NETCDF_INTEGER_0D(KFILE_ID,HNAME,HLONG_NAME,KTAB_0D,HATT_TITLE,HATT_TEXT)
!
USE NETCDF
!
INTEGER,               INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),      INTENT(IN) :: HNAME
CHARACTER(LEN=*),      INTENT(IN) :: HLONG_NAME
INTEGER,               INTENT(IN) :: KTAB_0D
CHARACTER(LEN=*),DIMENSION(:),OPTIONAL, INTENT(IN) :: HATT_TITLE,HATT_TEXT
END  SUBROUTINE WRITE_NETCDF_INTEGER_0D
!
!-------------------------------------------------------------------------------------------------
  SUBROUTINE WRITE_NETCDF_INTEGER_1D(KFILE_ID,HNAME,HLONG_NAME,KTAB_1D,KDIM_ID1)
!
USE NETCDF
!
INTEGER,               INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),      INTENT(IN) :: HNAME
CHARACTER(LEN=*),      INTENT(IN) :: HLONG_NAME
INTEGER,               INTENT(IN) :: KDIM_ID1
INTEGER,DIMENSION(:),  INTENT(IN) :: KTAB_1D
END  SUBROUTINE WRITE_NETCDF_INTEGER_1D
!
!---------------------------------------------------------------------------------------
    SUBROUTINE WRITE_NETCDF_INTEGER_2D(KFILE_ID,HNAME,HLONG_NAME,KTAB_2D,KDIM_ID1,KDIM_ID2)
!
USE NETCDF
!
INTEGER,               INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),      INTENT(IN) :: HNAME
CHARACTER(LEN=*),      INTENT(IN) :: HLONG_NAME
INTEGER,               INTENT(IN) :: KDIM_ID1,KDIM_ID2
INTEGER,DIMENSION(:,:),INTENT(IN) :: KTAB_2D
END  SUBROUTINE WRITE_NETCDF_INTEGER_2D
!
!-------------------------------------------------------------------------------------------------
         SUBROUTINE WRITE_NETCDF_REAL_0D(KFILE_ID,HNAME,HLONG_NAME,PVAL,HATT_TITLE,HATT_TEXT) 
!
USE NETCDF
!
INTEGER,          INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*), INTENT(IN) :: HNAME
CHARACTER(LEN=*), INTENT(IN) :: HLONG_NAME
REAL*4,           INTENT(IN) :: PVAL   
CHARACTER(LEN=*),DIMENSION(:),OPTIONAL, INTENT(IN) :: HATT_TITLE,HATT_TEXT
END SUBROUTINE WRITE_NETCDF_REAL_0D
!
!---------------------------------------------------------------------------------------
    SUBROUTINE WRITE_NETCDF_REAL_1D(KFILE_ID,HNAME,HLONG_NAME,PTAB_1D,KDIM_ID1,HATT_TITLE,HATT_TEXT)
!
USE NETCDF
!
INTEGER,          INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*), INTENT(IN) :: HNAME
CHARACTER(LEN=*), INTENT(IN) :: HLONG_NAME
INTEGER,          INTENT(IN) :: KDIM_ID1
REAL*4,DIMENSION(:),INTENT(IN) :: PTAB_1D
CHARACTER(LEN=*),DIMENSION(:),OPTIONAL, INTENT(IN) :: HATT_TITLE,HATT_TEXT
END SUBROUTINE WRITE_NETCDF_REAL_1D
!
!-------------------------------------------------------------------------------------------------
          SUBROUTINE WRITE_NETCDF_REAL_2D(KFILE_ID,HNAME,HLONG_NAME,PTAB_2D,KDIM_ID1,KDIM_ID2,HATT_TITLE,HATT_TEXT)
!
USE NETCDF
!
INTEGER,            INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),   INTENT(IN) :: HNAME
CHARACTER(LEN=*),   INTENT(IN) :: HLONG_NAME
INTEGER            ,INTENT(IN) :: KDIM_ID1,KDIM_ID2
REAL*4,DIMENSION(:,:),INTENT(IN) :: PTAB_2D
CHARACTER(LEN=*),DIMENSION(:),OPTIONAL, INTENT(IN) :: HATT_TITLE,HATT_TEXT
END SUBROUTINE WRITE_NETCDF_REAL_2D
!
!-------------------------------------------------------------------------------------------------
       SUBROUTINE WRITE_NETCDF_REAL_3D(KFILE_ID,HNAME,HLONG_NAME,PTAB_3D,KDIM_ID1,KDIM_ID2,KDIM_ID3)
!
USE NETCDF
!
INTEGER,              INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),     INTENT(IN) :: HNAME
CHARACTER(LEN=*),     INTENT(IN) :: HLONG_NAME
INTEGER             , INTENT(IN) :: KDIM_ID1,KDIM_ID2,KDIM_ID3
REAL*4,DIMENSION(:,:,:),INTENT(IN) :: PTAB_3D
END SUBROUTINE WRITE_NETCDF_REAL_3D
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
          SUBROUTINE WRITE_NETCDF_REAL_1D_PART(KFILE_ID,HNAME,PTAB_1D,KSTART,KCOUNT,KSTRIDE)
!
USE NETCDF
!
INTEGER,            INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),   INTENT(IN) :: HNAME
REAL,DIMENSION(:),  INTENT(IN) :: PTAB_1D
INTEGER,DIMENSION(:),  INTENT(IN) :: KSTART,KCOUNT,KSTRIDE
END SUBROUTINE WRITE_NETCDF_REAL_1D_PART
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
          SUBROUTINE WRITE_NETCDF_REAL_2D_PART(KFILE_ID,HNAME,PTAB_2D,KSTART,KCOUNT,KSTRIDE)
!
USE NETCDF
!
INTEGER,            INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),   INTENT(IN) :: HNAME
REAL,DIMENSION(:,:),INTENT(IN) :: PTAB_2D
INTEGER,DIMENSION(:),  INTENT(IN) :: KSTART,KCOUNT,KSTRIDE
END SUBROUTINE WRITE_NETCDF_REAL_2D_PART
!-------------------------------------------------------------------------------------------------
!
!
END INTERFACE !WRITE_NETCDF
!
END MODULE MODI_WRITE_NETCDF
!****************************************************************************
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
                SUBROUTINE WRITE_NETCDF_CHARACTER_0D(KFILE_ID,HNAME,HLONG_NAME,HVAL,KDIM_ID1)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,          INTENT(IN) :: KFILE_ID
INTEGER,          INTENT(IN) :: KDIM_ID1
CHARACTER(LEN=*), INTENT(IN) :: HNAME
CHARACTER(LEN=*), INTENT(IN) :: HLONG_NAME
CHARACTER(LEN=*), INTENT(IN) :: HVAL   
!
! ** local variables
!
INTEGER :: JRET,IVAR_ID,ILEN
INTEGER, DIMENSION(1) :: IDIM_ID
INTEGER,DIMENSION(4)::IRET
INTEGER,DIMENSION(1)::ISTART,ICOUNT
CHARACTER(LEN=100):: YPAS
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!--------------------------------------
!define mode
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_CHARACTER_0D',0,ZHOOK_HANDLE)
JRET=NF90_REDEF(KFILE_ID) 

IRET(:)=0
IDIM_ID(1) = KDIM_ID1
!define variabales
IRET(1) = NF90_DEF_VAR      (KFILE_ID,HNAME,NF90_CHAR,IDIM_ID,IVAR_ID)
IF (LEN_TRIM(HLONG_NAME).NE.0) &
  IRET(2) = NF90_PUT_ATT (KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)

!end define mode
IRET(3)=NF90_ENDDEF(KFILE_ID) 
!write variables
ILEN=LEN_TRIM(HVAL)
ISTART(1)=1
!        ICOUNT(1)=3
ICOUNT(1)=ILEN
YPAS=HVAL(:LEN_TRIM(HVAL))
IRET(4) = NF90_PUT_VAR (KFILE_ID,IVAR_ID,YPAS)
!       IRET(4) = NF90_PUT_VARA_TEXT (KFILE_ID,IVAR_ID,ISTART,ICOUNT,YPAS,20)
!test error
DO JRET=1,4
  IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_CHARACTER')
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_CHARACTER_0D',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_CHARACTER_0D
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
           SUBROUTINE WRITE_NETCDF_CHARACTER_1D (KFILE_ID,HNAME,HLONG_NAME,HVAL,KDIM_ID1,KDIM_ID2)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,                        INTENT(IN) :: KFILE_ID
INTEGER,                        INTENT(IN) :: KDIM_ID1,KDIM_ID2
CHARACTER(LEN=*),               INTENT(IN) :: HNAME
CHARACTER(LEN=*),               INTENT(IN) :: HLONG_NAME
CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HVAL   
!
! ** local variables
!
INTEGER :: JRET,IVAR_ID,JSIZE
INTEGER, DIMENSION(20) :: IRET
INTEGER, DIMENSION(2) :: IDIMS_ID,ICOUNT,ISTART
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------
!define mode
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_CHARACTER_1D',0,ZHOOK_HANDLE)
IRET(:)=0
JRET=NF90_REDEF(KFILE_ID) 
!define variable
IDIMS_ID(1)=KDIM_ID1
IDIMS_ID(2)=KDIM_ID2
IRET(1) = NF90_DEF_VAR      (KFILE_ID,HNAME,NF90_CHAR,IDIMS_ID,IVAR_ID)
IF (LEN_TRIM(HLONG_NAME).NE.0) &
  IRET(2) = NF90_PUT_ATT (KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)

!end define mode
IRET(3)=NF90_ENDDEF(KFILE_ID) 
!write variables
ISTART(1)=1 
ICOUNT(1)=19
DO JSIZE=1,SIZE(HVAL) 
  ISTART(2)=JSIZE
  ICOUNT(2)=1
  IRET(4) = NF90_PUT_VAR (KFILE_ID,IVAR_ID,HVAL(JSIZE),ISTART,ICOUNT)
ENDDO

!test error
DO JRET=1,3+SIZE(HVAL) 
  IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_CHARACTER_1D')
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_CHARACTER_1D',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_CHARACTER_1D
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
!
SUBROUTINE WRITE_NETCDF_INTEGER_0D(KFILE_ID,HNAME,HLONG_NAME,KTAB_0D,HATT_TITLE,HATT_TEXT)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,               INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),      INTENT(IN) :: HNAME
CHARACTER(LEN=*),      INTENT(IN) :: HLONG_NAME
INTEGER,               INTENT(IN) :: KTAB_0D
CHARACTER(LEN=*),DIMENSION(:),OPTIONAL, INTENT(IN) :: HATT_TITLE,HATT_TEXT
!
! ** local variables
!
INTEGER :: JRET,IVAR_ID,IATT,JATT,ILEN
INTEGER, DIMENSION(0) :: IDIM_ID
INTEGER, DIMENSION(4) :: IRET
REAL :: ZAVG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------

! Write array only if defined
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_INTEGER_0D',0,ZHOOK_HANDLE)
IF (KTAB_0D /= XUNDEF) THEN
  IRET(:)=0
  !define mode
  JRET=NF90_REDEF(KFILE_ID)

  !define variable
  IRET(1) = NF90_DEF_VAR      (KFILE_ID,HNAME,NF90_INT,IDIM_ID,IVAR_ID)
  IF (LEN_TRIM(HLONG_NAME).NE.0) &
         IRET(2) = NF90_PUT_ATT (KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)

  !Write optional attribute
  IF (PRESENT(HATT_TITLE).AND.PRESENT(HATT_TEXT)) THEN  
    IATT=SIZE(HATT_TITLE)
    IF (IATT .EQ. SIZE(HATT_TITLE)) THEN
      DO JATT=1,IATT
        ILEN=LEN_TRIM(HATT_TEXT(JATT))
        JRET = NF90_PUT_ATT (KFILE_ID,IVAR_ID,HATT_TITLE(JATT),HATT_TEXT(JATT))
      ENDDO
    ENDIF
  ENDIF  

  ! End define mode
  IRET(3)=NF90_ENDDEF(KFILE_ID) 

  !Write variable
  IRET(4)= NF90_PUT_VAR (KFILE_ID,IVAR_ID,KTAB_0D)

  DO JRET=1,4
    IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_INTEGER_0D')
  ENDDO

ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_INTEGER_0D',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_INTEGER_0D
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
!
SUBROUTINE WRITE_NETCDF_INTEGER_1D(KFILE_ID,HNAME,HLONG_NAME,KTAB_1D,KDIM_ID1)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,               INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),      INTENT(IN) :: HNAME
CHARACTER(LEN=*),      INTENT(IN) :: HLONG_NAME
INTEGER,               INTENT(IN) :: KDIM_ID1
INTEGER,DIMENSION(:),  INTENT(IN) :: KTAB_1D
!
! ** local variables
!
INTEGER :: JRET,IVAR_ID
INTEGER, DIMENSION(4) :: IRET
REAL :: ZAVG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_INTEGER_1D',0,ZHOOK_HANDLE)
ZAVG=SUM(KTAB_1D)/SIZE(KTAB_1D)

! Write array only if defined
IF (ZAVG /= XUNDEF) THEN
        IRET(:)=0
!define mode
       JRET=NF90_REDEF(KFILE_ID)

!define variable
       IRET(1) = NF90_DEF_VAR      (KFILE_ID,HNAME,NF90_INT,KDIM_ID1,IVAR_ID)
       IF (LEN_TRIM(HLONG_NAME).NE.0) &
         IRET(2) = NF90_PUT_ATT (KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)

! End define mode
       IRET(3)=NF90_ENDDEF(KFILE_ID) 
!Write variable
       IRET(4)= NF90_PUT_VAR (KFILE_ID,IVAR_ID,KTAB_1D)
!test error
       DO JRET=1,4
          IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_INTEGER_1D')
       ENDDO
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_INTEGER_1D',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_INTEGER_1D
!
!------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
           SUBROUTINE WRITE_NETCDF_INTEGER_2D(KFILE_ID,HNAME,HLONG_NAME,KTAB_2D,KDIM_ID1,KDIM_ID2)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,               INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),      INTENT(IN) :: HNAME
CHARACTER(LEN=*),      INTENT(IN) :: HLONG_NAME
INTEGER,               INTENT(IN) :: KDIM_ID1,KDIM_ID2
INTEGER,DIMENSION(:,:),INTENT(IN) :: KTAB_2D
!
! ** local variables
!
INTEGER :: JRET,IVAR_ID
INTEGER, DIMENSION(2) :: IDIMS_ID
INTEGER, DIMENSION(4) :: IRET
REAL :: ZAVG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------

IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_INTEGER_2D',0,ZHOOK_HANDLE)
ZAVG=SUM(KTAB_2D)/SIZE(KTAB_2D)

! Write array only if defined
IF (ZAVG /= XUNDEF) THEN
        IRET(:)=0
!define mode
       JRET=NF90_REDEF(KFILE_ID)

!define variable
       IDIMS_ID(1)=KDIM_ID1
       IDIMS_ID(2)=KDIM_ID2
       IRET(1) = NF90_DEF_VAR      (KFILE_ID,HNAME,NF90_INT,IDIMS_ID,IVAR_ID)
       IF (LEN_TRIM(HLONG_NAME).NE.0) &
         IRET(2) = NF90_PUT_ATT (KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)

! End define mode
       IRET(3)=NF90_ENDDEF(KFILE_ID) 
!Write variable
       IRET(4)= NF90_PUT_VAR (KFILE_ID,IVAR_ID,KTAB_2D)
!test error
       DO JRET=1,4
          IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_INTEGER_2D')
       ENDDO
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_INTEGER_2D',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_INTEGER_2D
!
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
      SUBROUTINE WRITE_NETCDF_REAL_0D(KFILE_ID,HNAME,HLONG_NAME,PVAL,HATT_TITLE,HATT_TEXT)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,          INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*), INTENT(IN) :: HNAME
CHARACTER(LEN=*), INTENT(IN) :: HLONG_NAME
REAL*4,           INTENT(IN) :: PVAL   
CHARACTER(LEN=*),DIMENSION(:),OPTIONAL, INTENT(IN) :: HATT_TITLE,HATT_TEXT
!
! ** local variables
!
INTEGER :: JRET,IVAR_ID,IATT,JATT,ILEN
INTEGER, DIMENSION(0) :: IDIM_ID
INTEGER,DIMENSION(5) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------



! Write array only if defined
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_0D',0,ZHOOK_HANDLE)
!
IF (PVAL /= XUNDEF) THEN
       IRET(:)=0
       !Put netcdf file in define mode
       IRET(1)=NF90_REDEF(KFILE_ID)
       ! define variables
       IRET(2) = NF90_DEF_VAR(KFILE_ID,HNAME,NF90_REAL,IDIM_ID,IVAR_ID)
       IF (LEN_TRIM(HLONG_NAME).NE.0) &
         IRET(3) = NF90_PUT_ATT(KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)
       !
       !Write optional attribute
       IF (PRESENT(HATT_TITLE).AND.PRESENT(HATT_TEXT)) THEN  
            IATT=SIZE(HATT_TITLE)
            IF (IATT .EQ. SIZE(HATT_TITLE)) THEN
                    DO JATT=1,IATT
                      ILEN=LEN_TRIM(HATT_TEXT(JATT))
                      JRET = NF90_PUT_ATT (KFILE_ID,IVAR_ID,HATT_TITLE(JATT),HATT_TEXT(JATT))
                    ENDDO
            ENDIF
       ENDIF
       !
       ! End define mode
       IRET(4)=NF90_ENDDEF(KFILE_ID) 
       ! Write variables
       IRET(5)= NF90_PUT_VAR (KFILE_ID,IVAR_ID,PVAL)
       !
       ! test error
       DO JRET=1,5
        IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_NETCDF_0D')
       ENDDO
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_0D',1,ZHOOK_HANDLE)
END SUBROUTINE WRITE_NETCDF_REAL_0D
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
      SUBROUTINE WRITE_NETCDF_REAL_1D(KFILE_ID,HNAME,HLONG_NAME,PTAB_1D,KDIM_ID1,HATT_TITLE,HATT_TEXT)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,          INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*), INTENT(IN) :: HNAME
CHARACTER(LEN=*), INTENT(IN) :: HLONG_NAME
INTEGER,          INTENT(IN) :: KDIM_ID1
REAL*4,DIMENSION(:),INTENT(IN) :: PTAB_1D
CHARACTER(LEN=*),DIMENSION(:),OPTIONAL, INTENT(IN) :: HATT_TITLE,HATT_TEXT
!
! ** local variables
!
INTEGER, DIMENSION(4) :: IRET
INTEGER :: IATT,JATT,ILEN
INTEGER :: JRET,IVAR_ID
REAL :: ZAVG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_1D',0,ZHOOK_HANDLE)
!
ZAVG=SUM(PTAB_1D)/SIZE(PTAB_1D)

! Write array only if defined
IF (ZAVG /= XUNDEF) THEN
       IRET(:)=0
!define mode
       JRET=NF90_REDEF(KFILE_ID) 
! define variable
       IRET(1) = NF90_DEF_VAR      (KFILE_ID,HNAME,NF90_REAL,KDIM_ID1,IVAR_ID)
       IF (LEN_TRIM(HLONG_NAME).NE.0) &
         IRET(2) = NF90_PUT_ATT (KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)
!Write optional attribute
       IF (PRESENT(HATT_TITLE).AND.PRESENT(HATT_TEXT)) THEN  
            IATT=SIZE(HATT_TITLE)
            IF (IATT .EQ. SIZE(HATT_TITLE)) THEN
                    DO JATT=1,IATT
                      ILEN=LEN_TRIM(HATT_TEXT(JATT))
                      JRET = NF90_PUT_ATT (KFILE_ID,IVAR_ID,HATT_TITLE(JATT),HATT_TEXT(JATT))
                    ENDDO
            ENDIF
       ENDIF
!end define mode
       IRET(3)=NF90_ENDDEF(KFILE_ID) 
!write variables
       IRET(4) = NF90_PUT_VAR(KFILE_ID,IVAR_ID,PTAB_1D(:))
!test error
    DO JRET=1,4
        IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_NETCDF_REAL_1D') 
    ENDDO
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_1D',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_REAL_1D
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
      SUBROUTINE WRITE_NETCDF_REAL_2D(KFILE_ID,HNAME,HLONG_NAME,PTAB_2D,KDIM_ID1,KDIM_ID2,HATT_TITLE,HATT_TEXT)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,            INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),   INTENT(IN) :: HNAME
CHARACTER(LEN=*),   INTENT(IN) :: HLONG_NAME
INTEGER,            INTENT(IN) :: KDIM_ID1,KDIM_ID2
REAL*4,DIMENSION(:,:),INTENT(IN) :: PTAB_2D
CHARACTER(LEN=*),DIMENSION(:),OPTIONAL, INTENT(IN) :: HATT_TITLE,HATT_TEXT
!
! ** local variables
!
INTEGER :: JRET,IVAR_ID,IATT,JATT,ILEN
INTEGER, DIMENSION(4) :: IRET
INTEGER, DIMENSION(2) :: IDIMS_ID
REAL :: ZAVG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------

IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_2D',0,ZHOOK_HANDLE)
ZAVG=SUM(PTAB_2D)/SIZE(PTAB_2D)
! Write array only if defined
IF (ZAVG /= XUNDEF) THEN
       IRET(:)=0
       ! define mode
       JRET=NF90_REDEF(KFILE_ID)
        !define variables in the netcdf file
       IDIMS_ID(1)=KDIM_ID1
       IDIMS_ID(2)=KDIM_ID2

       IRET(1) = NF90_DEF_VAR      (KFILE_ID,HNAME,NF90_REAL,IDIMS_ID,IVAR_ID)
       IF (LEN_TRIM(HLONG_NAME).NE.0) &
         IRET(2) = NF90_PUT_ATT (KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)
       ! 
       !Write optional attribute
       IF (PRESENT(HATT_TITLE).AND.PRESENT(HATT_TEXT)) THEN  
            IATT=SIZE(HATT_TITLE)
            IF (IATT .EQ. SIZE(HATT_TITLE)) THEN
                    DO JATT=1,IATT
                      ILEN=LEN_TRIM(HATT_TEXT(JATT))
                      JRET = NF90_PUT_ATT (KFILE_ID,IVAR_ID,HATT_TITLE(JATT),HATT_TEXT(JATT))
                    ENDDO
            ENDIF
       ENDIF
        ! end define mode
       IRET(3)=NF90_ENDDEF(KFILE_ID) 
        !Write variables
       IRET(4) = NF90_PUT_VAR(KFILE_ID,IVAR_ID,PTAB_2D)

       ! test error
       DO JRET=1,4 
          IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_NETCDF_2D')
       ENDDO
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_2D',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_REAL_2D
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
         SUBROUTINE WRITE_NETCDF_REAL_3D(KFILE_ID,HNAME,HLONG_NAME,PTAB_3D,KDIM_ID1,KDIM_ID2,KDIM_ID3)

USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,              INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),     INTENT(IN) :: HNAME
CHARACTER(LEN=*),     INTENT(IN) :: HLONG_NAME
INTEGER,              INTENT(IN) :: KDIM_ID1,KDIM_ID2,KDIM_ID3
REAL*4,DIMENSION(:,:,:),INTENT(IN) :: PTAB_3D
!
! ** local variables
!
INTEGER :: JRET,IVAR_ID
INTEGER, DIMENSION(4) :: IRET
INTEGER, DIMENSION(3) :: IDIMS_ID
REAL :: ZAVG
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------

IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_3D',0,ZHOOK_HANDLE)
ZAVG=SUM(PTAB_3D)/SIZE(PTAB_3D)

! Write array only if defined
IF (ZAVG /= XUNDEF) THEN
       IRET(:)=0
       ! define mode
       JRET=NF90_REDEF(KFILE_ID) 
       !define variables in the netcdf file
       IDIMS_ID(1)=KDIM_ID1
       IDIMS_ID(2)=KDIM_ID2
       IDIMS_ID(3)=KDIM_ID3

       IRET(1) = NF90_DEF_VAR      (KFILE_ID,HNAME,NF90_REAL,IDIMS_ID,IVAR_ID)
       IF (LEN_TRIM(HLONG_NAME).NE.0) &
         IRET(2) = NF90_PUT_ATT (KFILE_ID,IVAR_ID,'long_name',HLONG_NAME)
        ! end define mode
       IRET(3)=NF90_ENDDEF(KFILE_ID) 
        !Write variables
       IRET(4) = NF90_PUT_VAR(KFILE_ID,IVAR_ID,PTAB_3D)

       ! test error
       DO JRET=1,4 
          IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_NETCDF_3D')
       ENDDO
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_3D',1,ZHOOK_HANDLE)
END SUBROUTINE WRITE_NETCDF_REAL_3D
!
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
          SUBROUTINE WRITE_NETCDF_REAL_1D_PART(KFILE_ID,HNAME,PTAB_1D,KSTART,KCOUNT,KSTRIDE)
!
USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
! global variables
INTEGER,            INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),   INTENT(IN) :: HNAME
REAL*4,DIMENSION(:),  INTENT(IN) :: PTAB_1D
INTEGER,DIMENSION(:),  INTENT(IN) :: KSTART,KCOUNT,KSTRIDE
!
! local variables
INTEGER :: IVAR_ID,JRET
INTEGER, DIMENSION(2) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------
!find var_id
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_1D_PART',0,ZHOOK_HANDLE)
IRET(1)=NF90_INQ_VARID(KFILE_ID,HNAME,IVAR_ID)
!write_variable
IRET(2) = NF90_PUT_VAR (KFILE_ID,IVAR_ID,PTAB_1D,KSTART,KCOUNT,KSTRIDE)

!check for error
DO JRET=1,2 
          IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_NETCDF_REAL_1D_PART')
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_1D_PART',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_REAL_1D_PART
!-------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------
          SUBROUTINE WRITE_NETCDF_REAL_2D_PART(KFILE_ID,HNAME,PTAB_2D,KSTART,KCOUNT,KSTRIDE)
!
USE MODD_SURF_PAR
USE MODI_HANDLE_ERR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
! global variables
INTEGER,            INTENT(IN) :: KFILE_ID
CHARACTER(LEN=*),   INTENT(IN) :: HNAME
REAL,DIMENSION(:,:),INTENT(IN) :: PTAB_2D
INTEGER,DIMENSION(:),  INTENT(IN) :: KSTART,KCOUNT,KSTRIDE
!
! local variables
INTEGER :: IVAR_ID,JRET
INTEGER, DIMENSION(2) :: IRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!--------------------------------------
!find var_id
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_2D_PART',0,ZHOOK_HANDLE)
IRET(1)=NF90_INQ_VARID(KFILE_ID,HNAME,IVAR_ID)
!write_variable
IRET(2) = NF90_PUT_VAR(KFILE_ID,IVAR_ID,PTAB_2D,KSTART,KCOUNT,KSTRIDE)

!check for error
DO JRET=1,2 
          IF (IRET(JRET).NE.NF90_NOERR) CALL HANDLE_ERR(IRET(JRET),'WRITE_NETCDF_REAL_2D_PART')
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_WRITE_NETCDF:WRITE_NETCDF_REAL_2D_PART',1,ZHOOK_HANDLE)

END SUBROUTINE WRITE_NETCDF_REAL_2D_PART
