!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########
      PROGRAM PGD
!     ###########
!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!   
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    F. Mereyde                  Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     21/07/95
!!    Modification 26/07/95       Treatment of orography and subgrid-scale
!!                                orography roughness length (V. Masson)
!!    Modification 22/05/96       Variable CSTORAGE_TYPE (V. Masson)
!!    Modification 25/05/96       Modification of splines, correction on z0rel
!!                                and set limits for some surface varaibles
!!    Modification 12/06/96       Treatment of a rare case for ZPGDZ0EFF (Masson)
!!    Modification 22/11/96       removes the filtering. It will have to be 
!!                                performed in ADVANCED_PREP_PGD (Masson)
!!    Modification 15/03/99       **** MAJOR MODIFICATION **** (Masson)
!!                                PGD fields are now defined from the cover
!!                                type fractions in the grid meshes
!!                                User can still include its own data, and
!!                                even additional (dummy) fields
!!    Modificatio 06/00           patch approach, for vegetation related variable (Solmon/Masson)
!                                  averaging is performed on subclass(=patch) of nature
!!                08/03/01        add chemical emission treatment (D.Gazen)
!!    Modification 15/10/01       allow namelists in different orders (I.Mallet)
!!    Modification    07/11       new routine write_pgd_surf_atmn.F90 for writing PGD field (B.Decharme)
!!                                flag_update now in write_pgd_surf_atmn.F90 (B.Decharme)
!!
!!
!!                   ################################
!!    13/10/03       EXTERNALIZED VERSION (V. Masson)
!!                   ################################
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NCOMM, NPROC, NRANK, NPIO, WLOG_MPI, PREP_LOG_MPI,   &
                            END_LOG_MPI, NINDEX, NNUM, NSIZE_TASK
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
!
USE MODN_IO_OFFLINE
!
USE MODD_WRITE_SURF_ATM, ONLY : LFIRST_WRITE,  NCPT_WRITE
!
USE MODD_IO_SURF_ASC
USE MODD_IO_SURF_FA
USE MODD_IO_SURF_LFI
USE MODD_IO_SURF_NC
USE MODD_SURF_CONF
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!      
USE MODI_GET_LONLAT_n
!
USE MODI_INIT_INDEX_MPI
USE MODI_IO_BUFF_CLEAN
USE MODI_PGD_OROG_FILTER
USE MODI_PGD_SURF_ATM
USE MODI_PGD_GRID_SURF_ATM
USE MODI_WRITE_HEADER_FA
USE MODI_WRITE_HEADER_MNH
USE MODI_WRITE_PGD_SURF_ATM_n
USE MODI_INIT_OUTPUT_NC_n
USE MODI_GET_SIZE_FULL_n
!
USE MODE_POS_SURF
!
USE MODN_WRITE_SURF_ATM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
!
USE MODD_OFF_SURFEX_n
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
#ifndef AIX64
!$ INCLUDE 'omp_lib.h'
#endif
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER            :: ILUOUT
INTEGER            :: ILUNAM
LOGICAL            :: GFOUND
!
 CHARACTER(LEN=28)  :: YLUOUT    ='LISTING_PGD'   ! name of the listing
 CHARACTER(LEN=100) :: YNAME 
!
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
INTEGER :: ILEVEL, INFOMPI
INTEGER            :: INW, JNW
INTEGER            :: IRET, ISIZE_FULL
DOUBLE PRECISION :: XTIME0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
#ifdef SFX_MPI
 CALL MPI_INIT_THREAD(MPI_THREAD_MULTIPLE,ILEVEL,INFOMPI)
#endif
!
IF (LHOOK) CALL DR_HOOK('PGD',0,ZHOOK_HANDLE)
!
 CALL SURFEX_ALLOC_LIST(1)
CSOFTWARE='PGD    '
!
#ifdef SFX_MPI
NCOMM = MPI_COMM_WORLD
 CALL MPI_COMM_SIZE(NCOMM,NPROC,INFOMPI)
 CALL MPI_COMM_RANK(NCOMM,NRANK,INFOMPI)
#endif
!
!
!*    1.      Set default names and parallelized I/O
!             --------------------------------------
!
 IF (NRANK>=10) THEN
  WRITE(YNAME,FMT='(A15,I2)') TRIM(YLUOUT),NRANK
ELSE
  WRITE(YNAME,FMT='(A15,I1)') TRIM(YLUOUT),NRANK
ENDIF
 CLUOUT_LFI =  ADJUSTL(ADJUSTR(YLUOUT)//'.txt')
 CLUOUT_NC  =  ADJUSTL(ADJUSTR(YNAME)//'.txt')
 CALL GET_LUOUT('ASCII ',ILUOUT)
CLUOUT_LFI =  ADJUSTL(ADJUSTR(YLUOUT)//'.txt')
OPEN(UNIT=ILUOUT,FILE=ADJUSTL(ADJUSTR(YLUOUT)//'.txt'),FORM='FORMATTED',ACTION='WRITE')
!
!     1.3     output file name read in namelist
!             ---------------------------------
 CALL OPEN_NAMELIST('ASCII ',ILUNAM,CNAMELIST)
 CALL POSNAM(ILUNAM,'NAM_IO_OFFLINE',GFOUND)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_IO_OFFLINE)
 CALL POSNAM(ILUNAM,'NAM_WRITE_SURF_ATM',GFOUND)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_WRITE_SURF_ATM)
 CALL CLOSE_NAMELIST('ASCII ',ILUNAM)
!
CFILEOUT     = ADJUSTL(ADJUSTR(CPGDFILE)//'.txt')      ! output of PGD program
CFILEOUT_FA  = ADJUSTL(ADJUSTR(CPGDFILE)//'.fa')
CFILEOUT_LFI = CPGDFILE
CFILEOUT_NC  = ADJUSTL(ADJUSTR(CPGDFILE)//'.nc')
!
 CALL GOTO_MODEL(1)
!
!$OMP PARALLEL
!$ NBLOCKTOT = OMP_GET_NUM_THREADS()
!$OMP END PARALLEL
!
 CALL PREP_LOG_MPI
!
 CALL WLOG_MPI(' ')
!
 CALL WLOG_MPI('NBLOCKTOT ',KLOG=NBLOCKTOT)
!
#ifdef SFX_MPI
XTIME0 = MPI_WTIME()
#endif
!
!*    2.      Preparation of surface physiographic fields
!             -------------------------------------------
!
 CALL INIT_INDEX_MPI(YSC%DTCO,YSC%U,YSC%UG,YSC%GCP,CSURF_FILETYPE,'PGD',YALG_MPI,XIO_FRAC)
 !
 CALL PGD_GRID_SURF_ATM(YSC%UG, YSC%U, YSC%GCP, CSURF_FILETYPE,&
                        '                            ','      ',.FALSE.,HDIR='H')
 !
 CALL GET_SIZE_FULL_n(CSURF_FILETYPE, YSC%U%NDIM_FULL, YSC%U%NSIZE_FULL, ISIZE_FULL)
YSC%U%NSIZE_FULL = ISIZE_FULL
!
 CALL PGD_SURF_ATM(YSC,CSURF_FILETYPE,'                            ','      ',.FALSE.)
!
 CALL PGD_OROG_FILTER(YSC%U,YSC%UG,CSURF_FILETYPE)
!
!*    3.      writing of surface physiographic fields
!             ---------------------------------------
!
!* building of the header for the opening of the file in case of Arpege file
IF (NRANK==NPIO) THEN
  IF (CSURF_FILETYPE=='FA    ') THEN
    LFANOCOMPACT = .TRUE.
    CALL WRITE_HEADER_FA(YSC%GCP, YSC%UG%G%CGRID, YSC%UG%XGRID_FULL_PAR, CSURF_FILETYPE,'PGD') 
  END IF
END IF
!
ALLOCATE(YSC%DUO%CSELECT(0))
!
LDEF = .TRUE.
!
IF (CSURF_FILETYPE=="NC    ") THEN
  CALL INIT_OUTPUT_NC_n (YSC%TM%BDD, YSC%CHE, YSC%CHN, YSC%CHU,   &
                         YSC%SM%DTS, YSC%TM%DTT, YSC%DTZ, YSC%IM, &
                         YSC%UG, YSC%U, YSC%DUO%CSELECT)
ENDIF
!
INW = 1
IF (CSURF_FILETYPE=="NC    ") INW = 2
!
LFIRST_WRITE = .TRUE.
NCPT_WRITE = 0
!
DO JNW = 1,INW
  !
  IF (LWRITE_COORD) CALL GET_LONLAT_n(YSC%DTCO, YSC%U, YSC%UG, YSC%DUO%CSELECT, CSURF_FILETYPE)
  !
  !* writing of the fields
  CALL IO_BUFF_CLEAN
  ! FLAG_UPDATE now in WRITE_PGD_SURF_ATM_n
  CALL WRITE_PGD_SURF_ATM_n(YSC, CSURF_FILETYPE)
  !
  LDEF = .FALSE.
  LFIRST_WRITE = .FALSE.
  NCPT_WRITE = 0
  CALL IO_BUFF_CLEAN
  !
ENDDO
!
!* closes the file
IF (NRANK==NPIO) THEN
  IF (CSURF_FILETYPE=='FA    ') THEN
#ifdef SFX_FA
    CALL FAIRME(IRET,NUNIT_FA,'UNKNOWN')
#endif    
  END IF
!
  !* add informations in the file
  IF (CSURF_FILETYPE=='LFI   ' .AND. LMNH_COMPATIBLE) CALL WRITE_HEADER_MNH
!
!*    3.     Close parallelized I/O
!            ----------------------
!
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '    ----------------------'
  WRITE(ILUOUT,*) '    | PGD ENDS CORRECTLY |'
  WRITE(ILUOUT,*) '    ----------------------'
!
  WRITE(*,*) ' '
  WRITE(*,*) '    ----------------------'
  WRITE(*,*) '    | PGD ENDS CORRECTLY |'
  WRITE(*,*) '    ----------------------'
      !
  CLOSE(ILUOUT)
  !
ENDIF
!
 CALL SURFEX_DEALLO_LIST
!
IF (ALLOCATED(NINDEX)) DEALLOCATE(NINDEX)
IF (ALLOCATED(NNUM)) DEALLOCATE(NNUM)
IF (ALLOCATED(NSIZE_TASK)) DEALLOCATE(NSIZE_TASK)
!
 CALL END_LOG_MPI
!
IF (LHOOK) CALL DR_HOOK('PGD',1,ZHOOK_HANDLE)
!
#ifdef SFX_MPI
 CALL MPI_FINALIZE(INFOMPI)
#endif
!
!-------------------------------------------------------------------------------
!
END PROGRAM PGD
