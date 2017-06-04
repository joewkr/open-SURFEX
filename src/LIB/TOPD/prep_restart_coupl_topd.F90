!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!######
SUBROUTINE PREP_RESTART_COUPL_TOPD (UG, U, &
                                    HPROGRAM,KI)
!###################################################################
!
!!****  * PREP_RESTART_COUPL_TOPD*  
!!
!!    PURPOSE
!!    -------
!!   
!!    Write all files needed in case of restart of a simulation coupling SURFEX
!!     and TOPODYN
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Vincendon
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/06/11 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN,       ONLY : NNCAT, XQTOT, NNB_TOPD_STEP,&
                                 XQB_RUN, XQB_DR
USE MODD_COUPLING_TOPD, ONLY : XAS_NATURE,&
                                 NNB_STP_RESTART, XWTOPT,&
                                 XRUN_TOROUT, XDR_TOROUT
!
!
USE MODI_GET_LUOUT
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODI_WRITE_FILE_MAP
USE MODI_UNPACK_SAME_RANK
USE MODI_WRITE_FILE_ISBAMAP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6), INTENT(IN)         :: HPROGRAM ! program calling surf. schemes
INTEGER,          INTENT(IN)         :: KI       ! Surfex grid dimension
!
!*      0.2    declarations of local variables
!
INTEGER                        :: ILUOUT      ! unit of output listing file
INTEGER                        :: JSTP, JJ    ! loop control indexes
REAL, DIMENSION(:),ALLOCATABLE :: ZAS         ! Saturated area fraction for each Isba meshes
 CHARACTER(LEN=30)              :: YVAR        ! name of results file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PREP_RESTART_COUPL_TOPD',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
! * 1. Write stock files
!          
WRITE(ILUOUT,*) 'Write STOCK file'
!
 CALL OPEN_FILE('ASCII ',NUNIT,HFILE='stocks_sav.txt',HFORM='FORMATTED',HACTION='WRITE')
DO JSTP = 1,NNB_STP_RESTART
  WRITE(NUNIT,*)  XRUN_TOROUT(1:NNCAT,JSTP+NNB_TOPD_STEP), XDR_TOROUT(1:NNCAT,JSTP+NNB_TOPD_STEP)
ENDDO
 CALL CLOSE_FILE('ASCII ',NUNIT)
!  
! * 2. Write pixels water content
!
WRITE(ILUOUT,*) 'Write pixels water content files'
!
YVAR = '_xwtop_sav.map'
 CALL WRITE_FILE_MAP(XWTOPT,YVAR)
! 
! * 3. Write Asat files
! 
WRITE(ILUOUT,*) 'Write Asat files'
!
ALLOCATE(ZAS(KI))
 CALL UNPACK_SAME_RANK(U%NR_NATURE,XAS_NATURE,ZAS)
!
 CALL OPEN_FILE('ASCII ',NUNIT,HFILE='surfcont_sav.map',HFORM='FORMATTED',HACTION='WRITE')
 CALL WRITE_FILE_ISBAMAP(UG, &
                         NUNIT,ZAS,KI)
 CALL CLOSE_FILE('ASCII ',NUNIT)
!
IF (LHOOK) CALL DR_HOOK('PREP_RESTART_COUPL_TOPD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_RESTART_COUPL_TOPD
