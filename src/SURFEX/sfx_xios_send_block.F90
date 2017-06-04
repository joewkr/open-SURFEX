!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_SEND_BLOCK(HDTAG,PFIELD,PFIELD2,PFIELD3,&
                HDOMAIN,HAXIS,HAXIS2,HDCOMMENT,KFREQOP)
!!
!!
!!     PURPOSE 
!!     --------
!!
!!     Front-end to XIOS for client models
!!
!!     It performes field declaration to XIOS if needed, provided it is
!!     not too late with respect to xios context definition closing
!!     (see sfx_xios_declare_field)  
!!
!!     It copes with client models which process fields by 'blocks'
!!     over the first dimension, and wish to send them by blocks too,
!!     a set of blocks being provided, duly ordered, between two
!!     calendar updates
!!
!!     It gathers field blocks and send them to XIOS, using
!!     xios_send_field, as soon as the field is complete (i.e. enough
!!     blocks have been received, compared to a MODD_XIOS variable)
!!
!!     METHOD :
!!     -------------------- 
!!     
!!     For each new field name received, create an entry in buffer
!!     array and records full MPI-task field size (as known by
!!     Xios). 
!!
!!     For all field names, add the block to the buffer and, if field
!!     is complete, send it to Xios and clears the buffer
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
!!
!!
!!     REFERENCE
!!     ---------
!!
!!     XIOS Reference guide - Yann Meurdesoif - 10/10/2014 - 
!!     svn co --r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> 
!!     cd <dir>/doc ; ....
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    01/2016
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK
!
USE MODD_XIOS, ONLY : LXIOS, LXIOS_DEF_CLOSED, NBLOCK , NTIMESTEP
!
! NBLOCK dans arpege : YOMDIM:NGPBLKS
#ifdef WXIOS 
USE MODI_SFX_XIOS_DECLARE_FIELD
USE XIOS ,ONLY : XIOS_IS_DEFINED_FIELD_ATTR, XIOS_GET_FIELD_ATTR, &
      XIOS_IS_DEFINED_GRID_ATTR, XIOS_GET_GRID_ATTR, &
      XIOS_IS_DEFINED_DOMAIN_ATTR, XIOS_GET_DOMAIN_ATTR, &
      XIOS_IS_VALID_FIELD, XIOS_SEND_FIELD, XIOS_SET_FIELD_ATTR,&
      XIOS_GET_HANDLE, XIOS_ADD_CHILD, XIOS_SET_ATTR,&
      XIOS_FIELD, XIOS_FIELDGROUP, XIOS_FIELD_IS_ACTIVE,&
      XIOS_UPDATE_CALENDAR
#endif
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK
USE PARKIND1 , ONLY : JPIM, JPRB
!
IMPLICIT NONE
!
!   Arguments
!
 CHARACTER(LEN=*), INTENT(IN)               :: HDTAG   ! Field name
REAL(KIND=JPRB) , INTENT(IN), OPTIONAL, DIMENSION(:)  :: PFIELD  ! Field data block
REAL(KIND=JPRB) , INTENT(IN), OPTIONAL, DIMENSION(:,:):: PFIELD2 ! (or) 2d field data block
REAL(KIND=JPRB) , INTENT(IN), OPTIONAL, DIMENSION(:,:,:):: PFIELD3 ! (or) 3d field data block
 CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: HDOMAIN ! Field domain name, defaults to 'FULL' 
 CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: HAXIS   ! Axis name, for 2d fields
 CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: HAXIS2  ! 2nd axis name, for 3d fields
 CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: HDCOMMENT ! Comment 'a la Surfex' (i.e. '<long name> (<units>)')
 INTEGER         , INTENT(IN), OPTIONAL     :: KFREQOP ! Sampling frequency, in minutes
!
!  Local variables
!
#ifdef WXIOS
!
! A basic type for handling fields and their buffer
!
TYPE BUF_t
   CHARACTER(LEN=100) :: YLNAME   ! Field name , as for XIOS
   INTEGER(KIND=JPIM) :: ISIZEMAX ! Expected size of the complete field
                                  ! for the whole of the MPI task
   INTEGER(KIND=JPIM) :: ISIZE    ! Current usable size (ie. over received blocks)
   INTEGER(KIND=JPIM) :: INDIM    ! Number of dimensions 
   INTEGER(KIND=JPIM) :: ILEV     ! Size of 2nd dim  (from first call)
   INTEGER(KIND=JPIM) :: ILEV2    ! Size of 3rd dim (from first call)
   INTEGER(KIND=JPIM) :: IBLOCK   ! Number of blocks received for current timestep
   REAL(KIND=JPRB), ALLOCATABLE, DIMENSION (:,:,:) :: ZFIELD ! Accumulate received blocks
END TYPE BUF_t
!
TYPE(xios_field) :: field_hdl, other_field_hdl
TYPE(xios_fieldgroup) :: fieldgroup_hdl
INTEGER(KIND=JPIM)             :: ISIZE = 1000         ! Initial number of managed field entries 
INTEGER(KIND=JPIM), PARAMETER  :: INCR = 100          ! Increment in field entries number when reallocating
INTEGER(KIND=JPIM), PARAMETER  :: IMAXSIZE = 10000    ! Max number of field entries
!
TYPE(BUF_t), POINTER                   :: YLF       ! Current buffer entry
TYPE(BUF_t), ALLOCATABLE, TARGET, SAVE :: YLFIELDS(:)! Array of buffer entries
TYPE(BUF_t), ALLOCATABLE               :: YLTEMP(:) ! id - temporary
!
INTEGER(KIND=JPIM)      :: JI, IL, IEMPTY, IIDIM, ITAKE, ILEV, INFIELDS
 CHARACTER(LEN=100)      :: YLTAG   ! Field name
 CHARACTER(LEN=300)      :: YLAXIS, YLAXIS2 
 CHARACTER(LEN=300)      :: YLDOMAIN
 CHARACTER(LEN=300)      :: YLGRID
 CHARACTER(LEN=300)      :: YLCOMMENT
!
INTEGER                 :: IFREQOP 
!
LOGICAL :: GLISDEF
!
#endif
!
REAL(KIND=JPRB)          :: ZHOOK_HANDLE
!
!#include "abor1.intfb.h"
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_1',0,ZHOOK_HANDLE)
!
IF (.NOT. LXIOS) THEN
  IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_1',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
#ifdef WXIOS
!
IF (LXIOS_DEF_CLOSED) THEN 
  IF (.NOT. XIOS_FIELD_IS_ACTIVE(HDTAG) ) THEN 
    IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_1',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
ENDIF
YLTAG = TRIM(HDTAG)
!
!$OMP SINGLE
!
ALLOCATE(YLFIELDS(ISIZE))
YLFIELDS(:)%YLNAME = ''
!
!   Search if field is known - a simple loop on the table -
!   probably not much quick ...
!
IL = 0
DO JI=1,ISIZE
  IF (YLFIELDS(JI)%YLNAME == YLTAG) THEN 
    IL = JI
    EXIT
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_1',1,ZHOOK_HANDLE)
!
IF ( IL==0 ) THEN
  !
  ! Field is not yet recorded -> ask XIOS if field is known,  and what's its size
  ! By exception : if XIOS still in init phase, declare field domain if needed
  !
  IF (.NOT. LXIOS_DEF_CLOSED) THEN

IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_2',0,ZHOOK_HANDLE)

    YLDOMAIN=''
    IF (PRESENT(HDOMAIN)) YLDOMAIN = TRIM(HDOMAIN)
    YLCOMMENT=''
    IF (PRESENT(HDCOMMENT)) YLCOMMENT = TRIM(HDCOMMENT)
    IFREQOP=0
    IF (PRESENT(KFREQOP)) IFREQOP = KFREQOP
    !
    IF (PRESENT(PFIELD)) THEN 
      CALL SFX_XIOS_DECLARE_FIELD(YLTAG, YLDOMAIN, HCOMMENT=YLCOMMENT, KFREQOP=IFREQOP)
    ELSEIF (PRESENT(PFIELD2)) THEN 
      YLAXIS=''
      IF (PRESENT(HAXIS)) YLAXIS = TRIM(HAXIS)
      CALL SFX_XIOS_DECLARE_FIELD(YLTAG, YLDOMAIN, HAXIS=YLAXIS, &
              KLEV=SIZE(PFIELD2,2), HCOMMENT=YLCOMMENT,KFREQOP=IFREQOP)
    ELSEIF (PRESENT(PFIELD3)) THEN 
      YLAXIS ='' ; IF (PRESENT(HAXIS))  YLAXIS  = TRIM(HAXIS)
      YLAXIS2='' ; IF (PRESENT(HAXIS2)) YLAXIS2 = TRIM(HAXIS2)
      CALL SFX_XIOS_DECLARE_FIELD(YLTAG, YLDOMAIN, &
              HAXIS =YLAXIS , KLEV =SIZE(PFIELD3,2), &
              HAXIS2=YLAXIS2, KLEV2=SIZE(PFIELD3,3), HCOMMENT=YLCOMMENT,KFREQOP=IFREQOP)
    ELSE
      CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: NO PFIELDx FOR "//TRIM(YLTAG))
    ENDIF
    !
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_2',1,ZHOOK_HANDLE)

  ELSE
    !
    IF (.NOT. XIOS_IS_VALID_FIELD(YLTAG)) THEN
      CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: FIELD "//TRIM(YLTAG)//&
              &" WASN'T DECLARED TO XIOS (NEITHER IN XML CONFIG FILE, NOR SOON ENOUGH FROM CODE)")
    ENDIF
    !
    ! Find a place (i.e. index IEMPTY) to record the new field
    !
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_3',0,ZHOOK_HANDLE)

    IEMPTY = 0
    DO JI = 1,ISIZE
      IF (TRIM(YLFIELDS(JI)%YLNAME) == '') THEN 
        IEMPTY = JI
        EXIT
      ENDIF
    ENDDO
    !
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_3',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_4',0,ZHOOK_HANDLE)


    IF ( IEMPTY == 0 ) THEN
      ! The fields table is full. Allocate a new one and copy the content
      IF (ISIZE > IMAXSIZE) THEN 
        CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: MAX BUFFER ENTRIES NUMBER WAS REACHED")
      ENDIF
      !
      ALLOCATE(YLTEMP(ISIZE))
      YLTEMP = YLFIELDS
      DEALLOCATE(YLFIELDS)
      !
      ALLOCATE(YLFIELDS(ISIZE+INCR))
      YLFIELDS(1:ISIZE) = YLTEMP(1:ISIZE)
      DEALLOCATE(YLTEMP)
      !
      YLFIELDS(ISIZE+1:ISIZE+INCR)%YLNAME = ''
      !
      IEMPTY = ISIZE+1
      ISIZE  = ISIZE+INCR
      !
    ENDIF
    !
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_4',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_5',0,ZHOOK_HANDLE)

    ! Record the new field attributes (not its data)
    !
    YLF => YLFIELDS(IEMPTY)
    YLF%YLNAME = TRIM(YLTAG)
    !
    CALL XIOS_IS_DEFINED_FIELD_ATTR(YLTAG, grid_ref=GLISDEF)
    IF (GLISDEF)  THEN 
      CALL XIOS_GET_FIELD_ATTR(YLTAG, grid_ref=YLGRID)
      IF (YLGRID(1:4)=='FULL') THEN 
        YLDOMAIN='FULL'
      ELSEIF (YLGRID(1:3)=='SEA') THEN 
        YLDOMAIN='SEA'
      ELSEIF (YLGRID(1:5)=='WATER') THEN 
        YLDOMAIN='WATER'
      ELSEIF (YLGRID(1:6)=='NATURE') THEN 
        YLDOMAIN='NATURE'
      ELSEIF (YLGRID(1:4)=='TOWN') THEN 
        YLDOMAIN='TOWN'
      ENDIF
    ELSE
      CALL XIOS_IS_DEFINED_FIELD_ATTR(YLTAG, domain_ref=GLISDEF)
      IF (GLISDEF)  THEN 
        CALL XIOS_GET_FIELD_ATTR(YLTAG, domain_ref=YLDOMAIN)
      ELSE
        CALL ABOR1_SFX('SFX_XIOS_SEND_BLOCK : FIELD '//TRIM(YLTAG)//' HAS NO DOMAIN')
      ENDIF
    ENDIF
    !
    CALL XIOS_GET_DOMAIN_ATTR(YLDOMAIN, data_ni=IIDIM)
    !
    YLF%ISIZEMAX = IIDIM
    !
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_5',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_6',0,ZHOOK_HANDLE)

    INFIELDS = 0
    IF (PRESENT(PFIELD)) THEN
       YLF%ILEV  = 1
       YLF%ILEV2 = 1
       YLF%INDIM = 1
       INFIELDS  = INFIELDS+1
    ENDIF
    IF (PRESENT(PFIELD2)) THEN
      YLF%ILEV  = SIZE(PFIELD2,2)
      YLF%ILEV2 = 1
      YLF%INDIM = 2
      INFIELDS  = INFIELDS+1
    ENDIF
    IF (PRESENT(PFIELD3)) THEN
      YLF%ILEV  = SIZE(PFIELD3,2)
      YLF%ILEV2 = SIZE(PFIELD3,3)
      YLF%INDIM = 3
      INFIELDS  = INFIELDS+1
    ENDIF
    IF (INFIELDS /= 1 ) &
      CALL ABOR1_SFX('SFX_XIOS_SEND_BLOCK : TOO FEW OR MANY PFIELDx ARGS FOR '//HDTAG)
    ALLOCATE(YLF%ZFIELD(YLF%ISIZEMAX,YLF%ILEV,YLF%ILEV2))
    YLF%ISIZE  = 0
    YLF%IBLOCK = 0
    !
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_6',1,ZHOOK_HANDLE)

  ENDIF
  !
ELSE
  !
  YLF => YLFIELDS(IL)
  !
ENDIF
!
IF (LXIOS_DEF_CLOSED)  THEN 
  !
  ! Check consistency between calls : discarded for efficiency purpose
  !IF (ILEV .NE. YLF%ILEV) THEN
  !    !   CALL ABOR1_SFX('SFX_XIOS_SEND_BLOCK : INCONSISTENT LEVELS # FOR '//YLTAG)
  !ENDIF
  !
  ! Allocate the data buffer if needed
  !
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_7',0,ZHOOK_HANDLE)
  IF (.NOT. ALLOCATED(YLF%ZFIELD)) THEN
    ALLOCATE(YLF%ZFIELD(YLF%ISIZEMAX,YLF%ILEV,YLF%ILEV2))
  ENDIF
  !
  ! Add the block data to the field buffer and send the field if it is
  ! complete
  !
  IF (YLF%INDIM ==1 ) THEN 
    ITAKE=SIZE(PFIELD)
  ELSEIF (YLF%INDIM ==2 ) THEN 
    ITAKE=SIZE(PFIELD2,1)
  ELSEIF (YLF%INDIM ==3 ) THEN 
    ITAKE=SIZE(PFIELD3,1)
  ENDIF
  !
  YLF%IBLOCK = YLF%IBLOCK+1
  IF ((YLF%ISIZE + ITAKE) > YLF%ISIZEMAX) THEN 
    ! xxx a modifier : le dernier blc arpege arrive avec taille NPROMA
    IF (YLF%IBLOCK .NE. NBLOCK) THEN 
      CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: FIELD "//TRIM(YLTAG)//&
              " OVERFLOWS - CHECK ITS DECLARATION TO XIOS (MAYBE TWO INCONSISTENT DECLARATIONS ?)")
    ENDIF
    ITAKE = YLF%ISIZEMAX - YLF%ISIZE 
  ENDIF
  ! Store the field and update its size
  IF (ITAKE > 0 ) THEN 
    IF (YLF%INDIM==1) THEN 
      YLF%ZFIELD(YLF%ISIZE+1:YLF%ISIZE+ITAKE,1,1) = PFIELD(1:ITAKE)
    ELSEIF (YLF%INDIM==2) THEN 
      YLF%ZFIELD(YLF%ISIZE+1:YLF%ISIZE+ITAKE,:,1) = PFIELD2(1:ITAKE,:)
    ELSEIF (YLF%INDIM==3) THEN 
      YLF%ZFIELD(YLF%ISIZE+1:YLF%ISIZE+ITAKE,:,:) = PFIELD3(1:ITAKE,:,:)
    ENDIF
  ELSEIF ( ITAKE < 0 ) THEN
    CALL ABOR1_SFX('SFX_XIOS_SEND_BLOCK :isizemax < isize')     
  ENDIF
  !
  YLF%ISIZE = YLF%ISIZE + ITAKE
  !
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_7',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_8',0,ZHOOK_HANDLE)
  IF (YLF%IBLOCK==NBLOCK) THEN
    ! Send field and clears the buffer (incl. de-allocation)
    IF (YLF%INDIM==1) THEN 
      CALL XIOS_SEND_FIELD(trim(YLTAG),YLF%ZFIELD(:,1,1))
    ELSEIF (YLF%INDIM==2) THEN 
      CALL XIOS_SEND_FIELD(trim(YLTAG),YLF%ZFIELD(:,:,1))
    ELSEIF (YLF%INDIM==3) THEN 
      CALL XIOS_SEND_FIELD(trim(YLTAG),YLF%ZFIELD(:,:,:))
    ENDIF
    YLF%IBLOCK = 0
    YLF%ISIZE  = 0
    DEALLOCATE(YLF%ZFIELD)
  ENDIF  
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK_8',1,ZHOOK_HANDLE)
ENDIF
!
!$OMP END SINGLE
!
IF (ALLOCATED(YLFIELDS)) DEALLOCATE(YLFIELDS)
!
#endif
!
END SUBROUTINE SFX_XIOS_SEND_BLOCK 
