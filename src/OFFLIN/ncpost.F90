!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
        PROGRAM NCPOST
!
!!    MODIFICATIONS
!!    -------------
!!      B. Decharme : partition pgd/prep (grid attributes are only in the PGD file)
!!
!-------------------------------------------------------------------------------
!
USE MODD_OFF_SURFEX_n
!
        USE MODD_IO_SURF_ASC
        USE MODD_SURF_PAR
        USE MODI_OPEN_FILEIN_OL
        USE MODI_CLOSE_FILEIN_OL
        USE MODI_READ_SURF
        USE MODE_POS_SURF
!
        USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
        USE PARKIND1  ,ONLY : JPRB
!
        USE MODI_END_IO_SURF_n
        USE MODI_INIT_IO_SURF_n
        IMPLICIT NONE        

        REAL, ALLOCATABLE, DIMENSION(:)   ::   ZLOC
        REAL, ALLOCATABLE, DIMENSION(:)   ::   ZWRK
        REAL, ALLOCATABLE, DIMENSION(:)   ::   XLON
        REAL, ALLOCATABLE, DIMENSION(:)   ::   XLAT
        INTEGER, ALLOCATABLE, DIMENSION(:)::   IWRK2
        CHARACTER(LEN=50)                 ::   YCOMMENT
        CHARACTER(LEN=50)                 ::   NOM_ARTICLE
        CHARACTER(LEN=12)                 ::   HREC
        CHARACTER(LEN=1)                  ::   PATCHFLAG
        CHARACTER(LEN=2)                  ::   YPAS,YLVL
        CHARACTER(LEN=10)                 ::   CGRID_TYPE
        LOGICAL                           ::   GFOUND
        LOGICAL                           ::   LINITS ! true if PGD has been run
        LOGICAL                           ::   LSXNAM ! true if NCPOST.nam present
        LOGICAL                           ::   LCOORD ! true if LONLAT.dat present
        LOGICAL                           ::   LGEO=.TRUE.  !

        INTEGER    ::   IRET
        INTEGER    ::   INI
        INTEGER    ::   INJ
        INTEGER    ::   IF, IC, IP
        INTEGER    ::   IFIELD, IWFIELD
        INTEGER    ::   IPATCH, JPATCH
        INTEGER    ::   IBEG, IEND

        

        !plm
        !=====================================================================
        real, allocatable, dimension(:,:,:)   ::   zfield3d
        real, allocatable, dimension(:,:)     ::   zfield2d
        character (len=56) :: comlink
        integer    ::   inb_forc
        integer    ::   ji
        REAL(KIND=JPRB) :: ZHOOK_HANDLE
        !=====================================================================

        IF (LHOOK) CALL DR_HOOK('NCPOST',0,ZHOOK_HANDLE)
        CALL SURFEX_ALLOC_LIST(1)  
       CALL GOTO_MODEL(1)

        !=====================================================================
        !*
        !** get domain size and read latitudes and longitudes 
        !*
        !=====================================================================

        INQUIRE(FILE='LONLAT.dat',EXIST=LCOORD)
        IF (.NOT.LCOORD) THEN

           INQUIRE(FILE='PGD.txt', EXIST=LINITS)
      
           IF (.NOT. LINITS) THEN
              WRITE(*,*)' Now grid attributes are only in the PGD file'
              WRITE(*,*)' NO INPUT FILE FOUND FOR NCPOST'
              WRITE(*,*)' YOU SHOULD AT LEAST RUN PGD! '
              STOP
           ELSE
              CFILEIN='PGD.txt'
           ENDIF

           CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, 'ASCII ','FULL  ','SURF  ','READ ')

           CALL READ_SURF('ASCII ','DIM_FULL', INI, IRET)
           CALL READ_SURF('ASCII ','GRID_TYPE', CGRID_TYPE, IRET)

        
           ALLOCATE(XLON(INI))
           ALLOCATE(XLAT(INI))

           IF (CGRID_TYPE=='GAUSS     ') THEN
              CALL POSNAM(NUNIT,'FULL  '//' '//'LONGAUSS',GFOUND,NLUOUT)
           ELSE
              CALL POSNAM(NUNIT,'FULL  '//' '//'XLON',GFOUND,NLUOUT)
           ENDIF

           READ(NUNIT,FMT=*)
           READ(NUNIT,FMT='(A50)') YCOMMENT
           READ(NUNIT,FMT=*,ERR=100) XLON(:)

           IF (CGRID_TYPE=='GAUSS     ') THEN
              CALL POSNAM(NUNIT,'FULL  '//' '//'LATGAUSS',GFOUND,NLUOUT)
           ELSE
              CALL POSNAM(NUNIT,'FULL  '//' '//'XLAT',GFOUND,NLUOUT)
           ENDIF

           READ(NUNIT,FMT=*)
           READ(NUNIT,FMT='(A50)') YCOMMENT
           READ(NUNIT,FMT=*,ERR=100) XLAT(:)

           OPEN(UNIT=30,FILE='LONLAT.dat',FORM='FORMATTED')
           DO IP=1,INI
              WRITE(30,*)XLON(IP),XLAT(IP)
           ENDDO

           CALL END_IO_SURF_n('ASCII ')
                   
        ENDIF

        !=====================================================================
        !*
        !** read fields from netcdf output file
        !*
        !=====================================================================

        INQUIRE(FILE='NCPOST.nam',EXIST=LSXNAM)
        IF (.NOT.LSXNAM) THEN
           WRITE(*,*)' > NCPOST.nam does not exist'
           STOP
        ENDIF
        OPEN(UNIT=46,FILE='NCPOST.nam',FORM='FORMATTED')
        READ(46,'(A1,1X,A6,1X,A16,1X,A40)')PATCHFLAG,CMASK,HREC,CFILE

        CALL OPEN_FILEIN_OL
        CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, 'OFFLIN','FULL  ','SURF  ','READ ')
        
        CALL READ_SURF('OFFLIN','DIM_FULL', INI, IRET)
        ALLOCATE(XLON(INI))
        ALLOCATE(XLAT(INI))
        OPEN(UNIT=30,FILE='LONLAT.dat',FORM='FORMATTED')
        DO IP=1,INI
           READ(30,*)XLON(IP),XLAT(IP)
        ENDDO

        CALL READ_SURF('OFFLIN','NB_TIMESTP', INB_FORC, IRET)
        CALL READ_SURF('OFFLIN','PATCH_NUMBER', IPATCH, IRET)
        CALL system('rm SXPOST.nc')
        comlink='ln -s '//CFILE//' SXPOST.nc'
        CALL system(comlink)

        IF (CMASK == 'FORC') THEN
           allocate(zfield2d(inb_forc-1,ini))
           CALL READ_SURF('OFFLIN',HREC,zfield2d(:,:), IRET)
           do ji=1,ini
              write(50,*)xlon(ji),xlat(ji),zfield2d(1,ji)
           enddo
        ELSEIF (CMASK == 'SIMU') THEN
           IF (PATCHFLAG == '+') THEN
              allocate(zfield3d(ini,ipatch,inb_forc-1))
              CALL READ_SURF('OFFLIN',HREC,zfield3d(:,:,:), IRET)
              do ji=1,ini
                 write(50,*)xlon(ji),xlat(ji),zfield3d(ji,1,1)
              enddo
           ELSE IF (PATCHFLAG == '-') THEN
              allocate(zfield2d(ini,inb_forc-1))
              CALL READ_SURF('OFFLIN',HREC,zfield2d(:,:), IRET)
              do ji=1,ini
                 write(50,*)xlon(ji),xlat(ji),zfield2d(ji,1)
              enddo
           ENDIF
        ELSE
           write(*,*)' > ',CMASK,'NOT ALLOWED (only FORC|SIMU)'
           write(*,*)' > Update NCPOST.nam'
           STOP
        ENDIF

        CALL CLOSE_FILEIN_OL
        CALL SURFEX_DEALLO_LIST

        STOP
 100    CONTINUE
        WRITE(NLUOUT,*) ' '
        WRITE(NLUOUT,*) ' ERROR WHEN READING ARTICLE',HREC
        WRITE(NLUOUT,*) ' '
        

        IF (LHOOK) CALL DR_HOOK('NCPOST',1,ZHOOK_HANDLE)
        CONTAINS

        SUBROUTINE ERR_STOP(HREC,CFILEIN,NLUOUT)
        CHARACTER(LEN=12)   ::   HREC
        CHARACTER(LEN=*)    ::   CFILEIN
        INTEGER             ::   NLUOUT
        REAL(KIND=JPRB) :: ZHOOK_HANDLE
        IF (LHOOK) CALL DR_HOOK('ERR_STOP',0,ZHOOK_HANDLE)
        WRITE(NLUOUT,*) ' '
        WRITE(NLUOUT,*) ' ARTICLE ',TRIM(HREC),' NOT FOUND IN FILE ', CFILEIN
        WRITE(NLUOUT,*) ' '
        WRITE(*,*) ' '
        WRITE(*,*) ' ARTICLE ',TRIM(HREC),' NOT FOUND IN FILE ', CFILEIN
        WRITE(*,*) ' '
        STOP
        IF (LHOOK) CALL DR_HOOK('ERR_STOP',1,ZHOOK_HANDLE)
        END SUBROUTINE ERR_STOP

        !=====================================================================


        END PROGRAM NCPOST
