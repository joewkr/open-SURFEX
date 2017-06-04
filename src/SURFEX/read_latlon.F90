!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_LATLON (UG, U, USS, &
                              HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME)
!     #########################################################
!
!!**** *READ_LATLON* reads a latlon file and call treatment subroutine
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    11/09/95
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_PGD_GRID,   ONLY : XMESHLENGTH
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_READHEAD
USE MODI_INI_SSOWORK
USE MODI_PT_BY_PT_TREATMENT
!
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
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM      ! Type of program
 CHARACTER(LEN=6),  INTENT(IN) :: HSCHEME       ! Scheme treated
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
!
!*    0.2    Declaration of local variables read in the data file head
!            ---------------------------------------------------------
!
REAL    :: ZGLBLATMIN                 ! minimum latitude of data box in the file
REAL    :: ZGLBLONMIN                 ! minimum longitude of data box in the file
REAL    :: ZGLBLATMAX                 ! maximum latitude of data box in the file
REAL    :: ZGLBLONMAX                 ! maximum longitude of data box in the file
INTEGER :: INBLINE                    ! number of latitude rows (number of lines
INTEGER :: INBCOL                     ! number of longitude rows (number of columns)
REAL    :: ZNODATA                    ! value below which data are not considered
!
!*    0.3    Declaration of local variables
!            ------------------------------
!
INTEGER :: IFILE                      ! logical units
INTEGER :: ILUOUT                     ! output listing logical unit
INTEGER :: IERR                       ! return codes
!
INTEGER :: JLOOP, IFACT                ! loop index
 CHARACTER(LEN=100):: YSTRING          ! string
!
REAL    :: ZDLAT                      ! latitude mesh in the data file
REAL    :: ZDLON                      ! longitude mesh in the data file
INTEGER :: JLINE                      ! index of line
INTEGER :: JCOL                       ! index of column
!
REAL, DIMENSION(:), ALLOCATABLE :: ZVALUE ! value of a record of data points
REAL, DIMENSION(:), POINTER     :: ZLAT   ! latitude of data points
REAL, DIMENSION(:), POINTER     :: ZLON   ! longitude of data points
LOGICAL :: GCOMPRESS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_LATLON',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.     Openning of header
!            ------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,IFILE,HFILENAME)
!
!----------------------------------------------------------------------------
!
!*    2.     Reading of the global field
!            ---------------------------
!
!*    2.1    Head of data file
!            -----------------
!
 CALL READHEAD(IFILE,ZGLBLATMIN,ZGLBLATMAX,ZGLBLONMIN,ZGLBLONMAX, &
               INBLINE,INBCOL,ZNODATA,ZDLAT,ZDLON,ZLAT,ZLON,IERR,IFACT,&
               GCOMPRESS)  
IF (IERR/=0) THEN
  CALL ABOR1_SFX('READ_LATLON: PROBLEM IN FILE HEADER')
END IF
!
!*    2.2    Closing of header
!            -----------------
!
 CALL CLOSE_NAMELIST(HPROGRAM,IFILE)
!
!----------------------------------------------------------------------------
!
!*    3.     Adapt subgrid mesh to input file resolution
!            -------------------------------------------
!
IF (HSUBROUTINE=='A_OROG') CALL INI_SSOWORK(XMESHLENGTH,ZDLAT,ZDLON)
!
!----------------------------------------------------------------------------
!
!*    4.     Openning of file
!            ----------------
!
 CALL OPEN_FILE(HPROGRAM,IFILE,HFILENAME,'FORMATTED',HACTION='READ')
DO JLOOP=1,8
  READ(IFILE,FMT='(A100)') YSTRING
END DO
!
!----------------------------------------------------------------------------
!
!*    5.     Allocation of array containing the data
!            ---------------------------------------
!
ALLOCATE(ZVALUE(INBCOL))
!
!----------------------------------------------------------------------------
!
!*    6.     Loop on lines
!            -------------
!
DO JLINE=1,INBLINE
!
!----------------------------------------------------------------------------
!
!*    7.     Reading in the file
!            -------------------
!
  READ(IFILE,FMT=*) ZVALUE(:)
!
!
!----------------------------------------------------------------------------
!
!*    8.     Loop on columns
!            ---------------
!
  DO JCOL=1,INBCOL

!-------------------------------------------------------------------------------
!
!*    9.     value not valid
!            ---------------
!
    IF (ABS(ZVALUE(JCOL)-ZNODATA)<=1.E-10) CYCLE
!
!-------------------------------------------------------------------------------
!
!*   10.     Call to the adequate subroutine (point by point treatment)
!            ----------------------------------------------------------
!
    ZVALUE(:) = ZVALUE(:) / FLOAT(IFACT)

    CALL PT_BY_PT_TREATMENT(UG, U, USS, &
                            ILUOUT,ZLAT(JLINE:JLINE),ZLON(JCOL:JCOL),ZVALUE(JCOL:JCOL),&            
                              HSUBROUTINE                                              )  
!
!-------------------------------------------------------------------------------
  END DO
!-------------------------------------------------------------------------------
END DO
!-------------------------------------------------------------------------------
!
!*   11.     deallocations
!            -------------
!
DEALLOCATE(ZLAT)
DEALLOCATE(ZLON)
DEALLOCATE(ZVALUE)
!
!-------------------------------------------------------------------------------
!
!*   12.     closes the file
!            ---------------
!
 CALL CLOSE_FILE(HPROGRAM,IFILE)
IF (LHOOK) CALL DR_HOOK('READ_LATLON',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_LATLON
