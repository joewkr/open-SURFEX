!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
!     ##########################
      SUBROUTINE WRITE_BUDGET_COUPL_ROUT
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!        
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
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
!!      L. Bouilloud & B. Vincendon     * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original  03/2008 
!!                03/2014 (B. Vincendon) add new control variables
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN,       ONLY : CCAT, NNCAT, NNB_TOPD_STEP
USE MODD_BUDGET_COUPL_ROUT
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!      none
!*      0.2    declarations of local variables
INTEGER           :: JCAT,JSTP ! loop control
INTEGER           :: INB_VAR   ! number of variable to write
 CHARACTER(LEN=28) :: YFILE     ! file name
 CHARACTER(LEN=40) :: YFORM     ! Writing format
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_BUDGET_COUPL_ROUT',0,ZHOOK_HANDLE)
!
!*       1.     WRITING BUDGET FILES
!               ------------
INB_VAR=12
!
DO JCAT=1,NNCAT
  !
  YFORM='(i6,12f15.1)'
  YFILE=TRIM('bilan_bv_')//TRIM(CCAT(JCAT))//TRIM('.txt')
  !
  CALL OPEN_FILE('ASCII ',NUNIT,HFILE=YFILE,HFORM='FORMATTED',HACTION='WRITE')
  !
  WRITE(NUNIT,*) '     T','         ',YB_VAR(1),'         ',YB_VAR(2),'         ',&
                                      YB_VAR(3),'         ',YB_VAR(4),'         ',&
                                      YB_VAR(5),'         ',YB_VAR(6),'         ',&
                                      YB_VAR(7),'         ',YB_VAR(8),'         ',&
                                      YB_VAR(9),'         ',YB_VAR(10),'         ',&
                                      YB_VAR(11),'         ',YB_VAR(12)
  !
  DO JSTP=1,NNB_TOPD_STEP
    WRITE(NUNIT,YFORM) JSTP,XB_VAR_BV(JSTP,JCAT,1:INB_VAR)
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',NUNIT)
  ! 
  YFILE=TRIM('bilan_nobv_')//TRIM(CCAT(JCAT))//TRIM('.txt')
  !
  CALL OPEN_FILE('ASCII ',NUNIT,HFILE=YFILE,HFORM='FORMATTED',HACTION='WRITE')
  ! 
  WRITE(NUNIT,*) '     T','         ',YB_VAR(1),'         ',YB_VAR(2),'         ',&
                                      YB_VAR(3),'         ',YB_VAR(4),'         ',&
                                      YB_VAR(5),'         ',YB_VAR(6),'         ',&
                                      YB_VAR(7),'         ',YB_VAR(8),'         ',&
                                      YB_VAR(9),'         ',YB_VAR(10),'         ',&
                                      YB_VAR(11),'         ',YB_VAR(12)

  !
  DO JSTP=1,NNB_TOPD_STEP
    WRITE(NUNIT,YFORM) JSTP,XB_VAR_NOBV(JSTP,JCAT,1:INB_VAR)
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',NUNIT)
  !
  YFORM='(i6,5f15.1)'
  YFILE=TRIM('bilan_q.txt')
  !
  CALL OPEN_FILE('ASCII ',NUNIT,HFILE=YFILE,HFORM='FORMATTED',HACTION='WRITE')
  !
  WRITE(NUNIT,*) '     T','         ',YB_VARQ(1),'         ',YB_VARQ(2),'         ',&
                                      YB_VARQ(3),'         ',YB_VARQ(4),'         ',&
                                      YB_VARQ(5),'         '
  ! 
  DO JSTP=1,NNB_TOPD_STEP
    WRITE(NUNIT,YFORM) JSTP,XB_VAR_Q(JSTP,JCAT,1:5)
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',NUNIT)
!  ENDIF 
  !
ENDDO !JCAT

YFORM='(i6,12f15.1)'
YFILE=TRIM('bilan_tot.txt')
!
 CALL OPEN_FILE('ASCII ',NUNIT,HFILE=YFILE,HFORM='FORMATTED',HACTION='WRITE')
!   
WRITE(NUNIT,*) '     T','         ',YB_VAR(1),'         ',YB_VAR(2),'         ',&
                                    YB_VAR(3),'         ',YB_VAR(4),'         ',&
                                    YB_VAR(5),'         ',YB_VAR(6),'         ',&
                                    YB_VAR(7),'         ',YB_VAR(8),'         ',&
                                    YB_VAR(9),'         ',YB_VAR(10),'         ',&
                                      YB_VAR(11),'         ',YB_VAR(12)

!
DO JSTP=1,NNB_TOPD_STEP
  WRITE(NUNIT,YFORM) JSTP,XB_VAR_TOT(JSTP,1:INB_VAR)
ENDDO
!
 CALL CLOSE_FILE('ASCII ',NUNIT)
!
IF (LHOOK) CALL DR_HOOK('WRITE_BUDGET_COUPL_ROUT',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_BUDGET_COUPL_ROUT
