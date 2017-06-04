!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_IO_SURF_OL_n (DTCO, U, HPROGRAM,HMASK,HSCHEME,HACTION,HNAME)
!     ######################
!
!!****  *INIT_IO_SURF_OL* Keep in memory the netcdf ID of the output files
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      modified 05/04 by P. LeMoigne *Meteo France*
!!      modified 06/10 by S. Faroux *Meteo France*
!!=================================================================
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_IO_SURF_OL
USE MODD_OL_FILEID, ONLY : XNETCDF_FILEID_OUT, XNETCDF_FILENAME_OUT
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODN_IO_OFFLINE,    ONLY : XTSTEP_OUTPUT
!
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_GET_DIM_FULL_n
USE MODI_GET_SIZE_FULL_n
USE MODI_GET_TYPE_DIM_n
USE MODI_INIT_IO_SURF_MASK_n
USE MODI_WRITE_SURF
!
USE MODD_SURFEX_MPI, ONLY : WLOG_MPI
!
USE NETCDF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM 
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK    
 CHARACTER(LEN=6),  INTENT(IN)  :: HSCHEME 
 CHARACTER(LEN=5),  INTENT(IN)  :: HACTION 
 CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HNAME
!
 CHARACTER(LEN=12), DIMENSION(1) :: YSELECT
REAL              :: ZDEN
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: ILU, IRET, IL, IFULL, JFILE
INTEGER           :: ILUOUT
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!------------------------------------------------------------------------------ 
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_OL_N',0,ZHOOK_HANDLE)
!
LMASK = .TRUE.
!
YSELECT(1) = "            "
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HACTION=='READ') THEN
  CALL READ_SURF('OFFLIN','DIM_FULL',IFULL,IRET)
ELSE 
  CALL GET_DIM_FULL_n(U%NDIM_FULL, IFULL)
ENDIF
!
! size by MPI task. NINDEX is supposed to be initialized at this step.  
 CALL GET_SIZE_FULL_n('OFFLIN',IFULL,U%NSIZE_FULL,ILU)
!
IL = ILU
 CALL GET_TYPE_DIM_n(DTCO, U, HMASK,IL)
 CALL INIT_IO_SURF_MASK_n(DTCO, U, HMASK, IL, ILUOUT, ILU, NMASK)
!
IF (HACTION=='READ' .AND. LHOOK) CALL DR_HOOK('INIT_IO_SURF_OL_N',1,ZHOOK_HANDLE)
IF (HACTION=='READ') RETURN
!
NID_NC = 0
DO JFILE = 1,SIZE(XNETCDF_FILENAME_OUT) 
  IF (TRIM(HNAME)==TRIM(XNETCDF_FILENAME_OUT(JFILE))) THEN
    NID_NC = XNETCDF_FILEID_OUT(JFILE)
    IF (LDEF) IRET = NF90_REDEF(NID_NC)
    EXIT
  ENDIF
ENDDO
!
IF (NRANK==NPIO) THEN
  !  
  YCOMMENT=''
  !
  IF (XTSTEP_OUTPUT == FLOOR(XTSTEP_OUTPUT/86400.)*86400) THEN 
    ZDEN = 86400.
  ELSEIF (XTSTEP_OUTPUT == FLOOR(XTSTEP_OUTPUT/3600.)*3600) THEN
    ZDEN = 3600.
  ELSEIF (XTSTEP_OUTPUT == FLOOR(XTSTEP_OUTPUT/60.)*60) THEN
    ZDEN = 60.
  ELSE
    ZDEN = 1.
  ENDIF
  !
  IF (.NOT.LTIME_WRITTEN.AND..NOT.LDEF) THEN 
    CALL WRITE_SURF(YSELECT,HPROGRAM,'time',XTSTEP_OUTPUT/ZDEN*XSTARTW,IRESP,HCOMMENT=YCOMMENT)
    LTIME_WRITTEN=.TRUE.
  ENDIF
  !
ENDIF
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_OL_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
END SUBROUTINE INIT_IO_SURF_OL_n
