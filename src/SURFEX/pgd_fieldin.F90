!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_PGD_FIELDIN
CONTAINS
      SUBROUTINE PGD_FIELDIN (DTCO, UG, U, USS, &
                            HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,PFIELD,&
                            OPRESENT,PVEGTYPE)
!     ##############################################################
!
!!**** *PGD_FIELDIN* monitor for averaging and interpolations of ISBA physiographic fields
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!    09/2010 (E. Kourzeneva):   interpolation of the lake depth
!!                               is not allowed and not necessary
!!
!!    02/2014 (B. Decharme):     interpolation of the lake depth
!!                               re-allowed but using the nearest point
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NPROC
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_PGDWORK,        ONLY : XALL, NSIZE_ALL, CATYPE, NSIZE, XSUMVAL,   &
                                NVALNBR, NVALCOUNT, XVALLIST, JPVALMAX
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGD_GRID,       ONLY : NL
!
USE MODD_DATA_COVER_PAR, ONLY : NTYPE, LVEG_PRES, NVEGTYPE
!
USE MODI_GET_LUOUT
USE MODI_TREAT_FIELD
USE MODI_INTERPOL_FIELD
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
 CHARACTER(LEN=*),  INTENT(IN) :: HFIELD    ! field name for prints
 CHARACTER(LEN=3),  INTENT(IN) :: HAREA     ! area where field is defined
!                                           ! 'ALL' : everywhere
!                                           ! 'NAT' : on nature
!                                           ! 'TWN' : on town
!                                           ! 'SEA' : on sea
!                                           ! 'WAT' : on inland waters
 CHARACTER(LEN=28), INTENT(IN) :: HFILE     ! data file name
 CHARACTER(LEN=6),  INTENT(INOUT) :: HFILETYPE ! data file type
REAL,              INTENT(IN) :: PUNIF     ! prescribed uniform value for field
REAL, DIMENSION(:,:),INTENT(OUT):: PFIELD    ! physiographic field
LOGICAL, OPTIONAL, INTENT(OUT) :: OPRESENT
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PVEGTYPE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: ILUOUT ! output listing logical unit
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
INTEGER                        :: IDIM   !
INTEGER :: JI
!
REAL, DIMENSION(:), ALLOCATABLE :: ZVEGTYPE
INTEGER :: JJ, JT, JTN
 CHARACTER(LEN=20)   :: YFIELD
 CHARACTER(LEN=6)    :: YMASK
INTEGER             :: INPTS     ! number of points used for interpolation
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFIELD    ! physiographic field on full grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_FIELDIN',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*    2.      Output listing logical unit
!             ---------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (LEN_TRIM(HFILE)/=0 .OR. PUNIF/=XUNDEF) THEN
  !
  IF (PRESENT(OPRESENT)) OPRESENT=.TRUE.
  !
  IF (HFILETYPE=='DIRTYP') THEN
    ALLOCATE(ZFIELD(NL,SUM(NTYPE)))
  ELSE
    ALLOCATE(ZFIELD(NL,1))
  ENDIF
  !-------------------------------------------------------------------------------
  !
  !*    6.      Mask for the field
  !             ------------------
  !
  YMASK = ''
  SELECT CASE (HAREA)
    CASE ('LAN')
          YMASK = 'LAND  '
    CASE ('TWN')
          YMASK = 'TOWN  '
    CASE ('BLD')
          YMASK = 'TOWN '
    CASE ('NAT')
          YMASK = 'NATURE'
    CASE ('SEA')
          YMASK = 'SEA   '
    CASE ('WAT')
          YMASK = 'WATER '
    CASE DEFAULT
          YMASK = 'FULL  '
  END SELECT

  CALL GET_TYPE_DIM_n(DTCO, U, YMASK,IDIM)
  IF (IDIM/=SIZE(PFIELD,1)) THEN
     WRITE(ILUOUT,*)'Wrong dimension of MASK: ',IDIM,SIZE(PFIELD,1)
     CALL ABOR1_SFX('PGD_FIELDIN: WRONG DIMENSION OF MASK')
  ENDIF

  ALLOCATE(IMASK(IDIM))
  ILU=0
  CALL GET_SURF_MASK_n(DTCO, U, YMASK,IDIM,IMASK,ILU,ILUOUT)
!
ELSE
  !
  IF (PRESENT(OPRESENT)) THEN
    OPRESENT=.FALSE.
    PFIELD(:,:) = XUNDEF
    IF (LHOOK) CALL DR_HOOK('PGD_FIELDIN',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of field : ', HFIELD
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_FIELDIN: NO PRESCRIBED VALUE NOR INPUT FILE FOR '//HFIELD)
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    3.      Read from file
!             --------------
!
IF (LEN_TRIM(HFILE)/=0) THEN
!
!-------------------------------------------------------------------------------
!
!*    4.      Averages the field
!             ------------------
!
  ALLOCATE(NSIZE_ALL (U%NDIM_FULL,1))
!
  NSIZE_ALL(:,1) = 0
!
  IF (CATYPE=='MAJ') THEN
    ALLOCATE(NVALNBR  (U%NDIM_FULL,1))
    ALLOCATE(NVALCOUNT(U%NDIM_FULL,JPVALMAX,1))
    ALLOCATE(XVALLIST (U%NDIM_FULL,JPVALMAX,1))
    NVALNBR   = 0
    NVALCOUNT = 0
    XVALLIST  = XUNDEF
    INPTS     = 1
  ELSE
    ALLOCATE(XALL(U%NDIM_FULL,1,1))
    XALL(:,:,:) = 0.
    INPTS       = 3
  ENDIF
!
  IF(HFIELD=="water depth") THEN
    INPTS = 1
  ENDIF
!
  YFIELD = '                    '
  YFIELD = HFIELD(1:MIN(LEN(HFIELD),20))
!
  ZFIELD(:,:) = XUNDEF
!
  CALL TREAT_FIELD(UG, U, USS, &
                   HPROGRAM,'SURF  ',HFILETYPE,'A_MESH',HFILE,   &
                   YFIELD,ZFIELD                   )
!
!-------------------------------------------------------------------------------
!
!*    4.      Mask for the interpolations
!             ---------------------------
!
  DO JT=1,SIZE(NSIZE,2)

    SELECT CASE (HAREA)

      CASE ('LAN')
        WHERE ((U%XTOWN(:)+U%XNATURE(:))==0. .AND. NSIZE(:,JT)==0 ) NSIZE(:,JT) = -1

      CASE ('TWN')
        WHERE (U%XTOWN  (:)==0. .AND. NSIZE(:,JT)==0 ) NSIZE(:,JT) = -1

      CASE ('BLD')
        WHERE (U%XTOWN  (:)==0. .AND. NSIZE(:,JT)==0 ) NSIZE(:,JT) = -1

      CASE ('NAT')
        WHERE (U%XNATURE(:)==0. .AND. NSIZE(:,JT)==0 ) NSIZE(:,JT) = -1
        !
        ! for fields calculated by vegtype
        IF (PRESENT(PVEGTYPE) .AND.SIZE(NSIZE,2)>1) THEN
          ! only for the natural part
          IF (U%LECOSG) THEN
            JTN = JT - SUM(NTYPE(1:2))
          ELSE
            JTN = JT
          ENDIF
          IF ( JTN <= SIZE(PVEGTYPE,2) ) THEN
            ALLOCATE(ZVEGTYPE(U%NSIZE_FULL))
            CALL UNPACK_SAME_RANK(IMASK,PVEGTYPE(:,JTN),ZVEGTYPE)
            WHERE (ZVEGTYPE(:)==0. .AND. NSIZE(:,JT)==0) NSIZE(:,JT) = -1
            DEALLOCATE(ZVEGTYPE)
          ELSE
            NSIZE(:,JT) = -1
          ENDIF
        ENDIF

      CASE ('SEA')
        WHERE (U%XSEA   (:)==0. .AND. NSIZE(:,JT)==0 ) NSIZE(:,JT) = -1

      CASE ('WAT')
        WHERE (U%XWATER (:)==0. .AND. NSIZE(:,JT)==0 ) NSIZE(:,JT) = -1

    END SELECT

  ENDDO
!
!-------------------------------------------------------------------------------
!
!*    5.      Interpolation if some points are not initialized (no data for these points)
!             ------------------------------------------------
!
  DO JT=1,SIZE(NSIZE,2)

    IF (.NOT.U%LECOSG.AND.JT>NVEGTYPE) EXIT

    !multitype input file
    IF (SIZE(ZFIELD,2)>1) THEN

      !fields defined only on the not-bare soil part
      IF ( (YFIELD(1:3)=='LAI'.OR.YFIELD(1:10)=='ALBNIR_VEG'.OR.YFIELD(1:10)=='ALBVIS_VEG') .OR. &
           (SIZE(ZFIELD,2)>1.AND.YFIELD(1:6)=='H_TREE') ) THEN

        IF ( (.NOT.U%LECOSG.AND.JT<=3).OR.(U%LECOSG.AND.JT<=SUM(NTYPE(1:2))+3) ) THEN
          ZFIELD(:,JT) = 0.
          NSIZE (:,JT) = 1
        ENDIF

        ! height of trees only defined for tree vegtypes
        IF (YFIELD(1:6)=='H_TREE') THEN
          IF ((.NOT.U%LECOSG.AND.((JT>=7 .AND. JT<=12) .OR. (JT>=18 .AND. JT<=19))).OR. &
              (     U%LECOSG.AND.(JT<=(SUM(NTYPE(1:2))+3).OR.JT>=(SUM(NTYPE(1:2))+13)) ) ) THEN
            ZFIELD(:,JT) = 0.
            NSIZE (:,JT) = 1.
          ENDIF
        ENDIF

      ENDIF

      ! if the cover / vegtype is not present on the area
      IF( (U%LECOSG.AND..NOT.U%LCOVER(JT)) .OR. (.NOT.U%LECOSG.AND..NOT.LVEG_PRES(JT)) ) THEN
        ZFIELD(:,JT) = 0.
        NSIZE (:,JT) = 1.
      ENDIF

    ENDIF

    IF (PUNIF/=XUNDEF) THEN
      CALL INTERPOL_FIELD(UG, U, HPROGRAM,ILUOUT,NSIZE(:,JT),ZFIELD(:,JT),HFIELD,PDEF=PUNIF,KNPTS=INPTS)
    ELSE
      CALL INTERPOL_FIELD(UG, U, HPROGRAM,ILUOUT,NSIZE(:,JT),ZFIELD(:,JT),HFIELD)
    ENDIF

  ENDDO
!
  DEALLOCATE(NSIZE    )
  DEALLOCATE(XSUMVAL  )
!
!-------------------------------------------------------------------------------
!
ELSEIF (PUNIF/=XUNDEF) THEN
!
!*    3.1     Use of the presribed field
!             --------------------------
!
  ZFIELD(:,:) = PUNIF
!
!
END IF
!
! only the case nature is treated for now, to adapt for town later
IF (HAREA=='NAT'.AND.SIZE(ZFIELD,2)>SIZE(PFIELD,2).AND.U%LECOSG) THEN
  CALL PACK_SAME_RANK(IMASK,ZFIELD(:,SUM(NTYPE(1:2))+1:SUM(NTYPE(1:3))),PFIELD(:,:))
ELSE
  CALL PACK_SAME_RANK(IMASK,ZFIELD(:,1:SIZE(PFIELD,2)),PFIELD(:,:))
ENDIF
!
DEALLOCATE(ZFIELD)
DEALLOCATE(IMASK)
!
IF (LHOOK) CALL DR_HOOK('PGD_FIELDIN',1,ZHOOK_HANDLE)

!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FIELDIN
END MODULE MODI_PGD_FIELDIN
