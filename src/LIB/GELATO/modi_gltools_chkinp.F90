!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!GLT_LIC The GELATO model is a seaice model used in stand-alone or embedded mode. 
!GLT_LIC  It has been developed by Meteo-France. The holder of GELATO is Meteo-France.
!GLT_LIC  
!GLT_LIC  This software is governed by the CeCILL-C license under French law and biding
!GLT_LIC  by the rules of distribution of free software. See the CeCILL-C_V1-en.txt
!GLT_LIC  (English) and CeCILL-C_V1-fr.txt (French) for details. The CeCILL is a free
!GLT_LIC  software license, explicitly compatible with the GNU GPL
!GLT_LIC  (see http://www.gnu.org/licenses/license-list.en.html#CeCILL)
!GLT_LIC  
!GLT_LIC  The CeCILL-C licence agreement grants users the right to modify and re-use the
!GLT_LIC  software governed by this free software license. The exercising of this right
!GLT_LIC  is conditional upon the obligation to make available to the community the
!GLT_LIC  modifications made to the source code of the software so as to contribute to
!GLT_LIC  its evolution.
!GLT_LIC  
!GLT_LIC  In consideration of access to the source code and the rights to copy, modify
!GLT_LIC  and redistribute granted by the license, users are provided only with a limited
!GLT_LIC  warranty and the software's author, the holder of the economic rights, and the
!GLT_LIC  successive licensors only have limited liability. In this respect, the risks
!GLT_LIC  associated with loading, using, modifying and/or developing or reproducing the
!GLT_LIC  software by the user are brought to the user's attention, given its Free
!GLT_LIC  Software status, which may make it complicated to use, with the result that its
!GLT_LIC  use is reserved for developers and experienced professionals having in-depth
!GLT_LIC  computer knowledge. Users are therefore encouraged to load and test the
!GLT_LIC  suitability of the software as regards their requirements in conditions enabling
!GLT_LIC  the security of their systems and/or data to be ensured and, more generally, to
!GLT_LIC  use and operate it in the same conditions of security. 
!GLT_LIC  
!GLT_LIC  The GELATO sofware is cureently distibuted with the SURFEX software, available at 
!GLT_LIC  http://www.cnrm.meteo.fr/surfex. The fact that you download the software deemed that
!GLT_LIC  you had knowledge of the CeCILL-C license and that you accept its terms.
!GLT_LIC  Attempts to use this software in a way not complying with CeCILL-C license
!GLT_LIC  may lead to prosecution. 
!GLT_LIC 
! =======================================================================
! ====================== MODULE modi_gltools_chkinp =======================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that prints for every glt_gelato 
! input field :
!   - global minimum
!   - global maximum
!   - global average (no weighing with grid cell surfaces)
!   - values for every field at a specified grid point
!   Note (for further development). Compared to modi_gltools_chkout, the
! name of the list of input fields (yfld) is different from that of
! glt_output fields in chkout. 
!
! Created : 2003/12 (D. Salas y Melia)
! Modified: 2008/12 (D. Salas y Melia) rewriting
! Modified: 2009/08 (D. Salas y Melia) double or single physics
! Modified: 2012/11 (D. Salas y Melia) parallelism & super-type
! Modified: 2014/01 (D. Salas y Melia) use standard wrivai for writing
!
! ------------------- BEGIN MODULE modi_gltools_chkinp --------------------
!
!THXS_SFX!MODULE modi_gltools_chkinp
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_chkinp( kdate,tpglt )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  INTEGER, INTENT(in) ::  &
!THXS_SFX!    kdate
!THXS_SFX!  TYPE(t_glt), INTENT(in) ::  &
!THXS_SFX!    tpglt
!THXS_SFX!END SUBROUTINE gltools_chkinp
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_chkinp
!
! -------------------- END MODULE modi_gltools_chkinp ---------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE gltools_chkinp ---------------------------
!
! * Subroutine that prints mini, maxi and average of every Gelato input
! field, plus field values at one specified grid point.
!
SUBROUTINE gltools_chkinp( kdate,tpglt )
!
  USE modd_types_glt
  USE modd_glt_param
  USE modi_gltools_nwords
  USE modi_gltools_strsplit
  USE mode_gltools_wrivais
  USE lib_mpp
  IMPLICIT none
!
! .. Dummy arguments
!
  INTEGER, INTENT(in) ::  &
    kdate
  TYPE(t_glt), INTENT(in) ::  &
    tpglt
!
! .. Local variables
!
  LOGICAL ::  &
        y3d,y3dd,ydo
  CHARACTER(2) ::  &
        ynum
  CHARACTER(80) ::  &
        ystep,yfile
  CHARACTER(200) ::  &
        yfld
  CHARACTER(80), DIMENSION(:), ALLOCATABLE ::  &
        ylistfld
  INTEGER ::  &
        infld,imonth,jf,jk
  REAL ::  &
        zofac,zmin,zmax,zsum
  REAL, DIMENSION(SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        zwork2
  REAL, DIMENSION(SIZE(tpglt%atm_ice,1),SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        zwork3
  REAL, DIMENSION(SIZE(tpglt%sit_d,1),SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        zwork3d
  REAL, DIMENSION(nxglo,nyglo) ::  &
        zbathy
  TYPE(t_def) ::  &
        tznam
!  
!
! 1. Initialisations
! ===================
!
  IF ( nprinto>=2 ) THEN
!
! .. List of all fields to be analysed (in a single string variable)
! The user can change this list at will, provided that:
!       - the length of this string (named yallfld) is less than 
!       the declared value 
!       - every word in this string is associated to an action in 
!       the case construct that follows.
!
      IF ( nnflxin==0 ) THEN
          yfld='pbat pqml pqoc ptml psml pssh puml pvml  &
&           plip psop pztx pmty   &
&           pnsfm pdflm pswam pevam'
        ELSE
          yfld='pbat pqml pqoc ptml psml pssh puml pvml  &
&           plip psop pztx pmty   &
&           pnsfi pdfli pswai pevai pnsfw pdflw pswaw pevaw'
      ENDIF
!
      IF ( ntd/=0 ) THEN
        yfld = TRIM(yfld) // ' asn fsi hsi hsn rsn tsf ssi age'
      ENDIF
!
! .. Get number of words in yfld, then get the list of fields in a vector
!
      infld = gltools_nwords( yfld )
      ALLOCATE( ylistfld(infld) )
!
      ylistfld = gltools_strsplit( yfld,infld )
!
! .. Surface grid factor 
!
      zofac = 1. / xdomsrf_g
!
!
!
! 2. Print information
! =====================
!
! 2.1. General prints
! --------------------
!
      IF ( lwg ) THEN
        WRITE(noutlu,*) 
        WRITE(noutlu,*)  &
        '===================== Control Gelato input data ' //  &
        '====================='
!
        WRITE(noutlu,*) 'First Time-Step  :',tpglt%ind%beg
        WRITE(noutlu,*) 'Current Time-Step:',tpglt%ind%cur
        WRITE(noutlu,*) 'Last Time-Step   :',tpglt%ind%end
!
        WRITE(noutlu,*)
        WRITE(noutlu,*)  &
          '  minimum         maximum         average'
        WRITE(noutlu,*)  &
          '  ============    ============    ============'
      ENDIF
!
!
! 2.2. Loop on fields
! --------------------
!
      DO jf=1,infld
!
! .. Determine to which field the input label is associated
!
        y3d = .FALSE.
        y3dd = .FALSE. 
!
        IF ( TRIM(ylistfld(jf))=='pbat' ) THEN
            zwork2(:,:) = tpglt%bat(:,:)
          ELSE IF ( TRIM(ylistfld(jf))=='pqml' ) THEN
            zwork2(:,:) = tpglt%oce_all(:,:)%qml
          ELSE IF ( TRIM(ylistfld(jf))=='pqoc' ) THEN
            zwork2(:,:) = tpglt%oce_all(:,:)%qoc
          ELSE IF ( TRIM(ylistfld(jf))=='ptml' ) THEN
            zwork2(:,:) = tpglt%oce_all(:,:)%tml
          ELSE IF ( TRIM(ylistfld(jf))=='psml' ) THEN
            zwork2(:,:) = tpglt%oce_all(:,:)%sml
          ELSE IF ( TRIM(ylistfld(jf))=='pssh' ) THEN
            zwork2(:,:) = tpglt%oce_all(:,:)%ssh
          ELSE IF ( TRIM(ylistfld(jf))=='puml' ) THEN
            zwork2(:,:) = tpglt%oce_all(:,:)%uml
          ELSE IF ( TRIM(ylistfld(jf))=='pvml' ) THEN
            zwork2(:,:) = tpglt%oce_all(:,:)%vml
          ELSE IF ( TRIM(ylistfld(jf))=='plip' ) THEN
            zwork2(:,:) = tpglt%atm_all(:,:)%lip
          ELSE IF ( TRIM(ylistfld(jf))=='psop' ) THEN
            zwork2(:,:) = tpglt%atm_all(:,:)%sop
          ELSE IF ( TRIM(ylistfld(jf))=='pztx' ) THEN
            zwork2(:,:) = tpglt%atm_all(:,:)%ztx
          ELSE IF ( TRIM(ylistfld(jf))=='pmty' ) THEN
            zwork2(:,:) = tpglt%atm_all(:,:)%mty
          ELSE IF ( TRIM(ylistfld(jf))=='pnsfm' ) THEN
            zwork2(:,:) = tpglt%atm_mix(1,:,:)%nsf
          ELSE IF ( TRIM(ylistfld(jf))=='pdflm' ) THEN
            zwork2(:,:) = tpglt%atm_mix(1,:,:)%dfl
          ELSE IF ( TRIM(ylistfld(jf))=='pswam' ) THEN
            zwork2(:,:) = tpglt%atm_mix(1,:,:)%swa
          ELSE IF ( TRIM(ylistfld(jf))=='pevam' ) THEN
            zwork2(:,:) = tpglt%atm_mix(1,:,:)%eva
          ELSE IF ( TRIM(ylistfld(jf))=='pnsfw' ) THEN
            zwork2(:,:) = tpglt%atm_wat(:,:)%nsf
          ELSE IF ( TRIM(ylistfld(jf))=='pdflw' ) THEN
            zwork2(:,:) = tpglt%atm_wat(:,:)%dfl
          ELSE IF ( TRIM(ylistfld(jf))=='pswaw' ) THEN
            zwork2(:,:) = tpglt%atm_wat(:,:)%swa
          ELSE IF ( TRIM(ylistfld(jf))=='pevaw' ) THEN
            zwork2(:,:) = tpglt%atm_wat(:,:)%eva
          ELSE IF ( TRIM(ylistfld(jf))=='pnsfi' ) THEN
            y3d = .TRUE.
            zwork3(:,:,:) = tpglt%atm_ice(:,:,:)%nsf
          ELSE IF ( TRIM(ylistfld(jf))=='pdfli' ) THEN
            y3d = .TRUE.
            zwork3(:,:,:) = tpglt%atm_ice(:,:,:)%dfl
          ELSE IF ( TRIM(ylistfld(jf))=='pswai' ) THEN
            y3d = .TRUE.
            zwork3(:,:,:) = tpglt%atm_ice(:,:,:)%swa
          ELSE IF ( TRIM(ylistfld(jf))=='pevai' ) THEN
            y3d = .TRUE.
            zwork3(:,:,:) = tpglt%atm_ice(:,:,:)%eva
          ELSE IF ( TRIM(ylistfld(jf))=='asn' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%asn
          ELSE IF ( TRIM(ylistfld(jf))=='fsi' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%fsi        
          ELSE IF ( TRIM(ylistfld(jf))=='hsi' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%hsi        
          ELSE IF ( TRIM(ylistfld(jf))=='hsn' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%hsn        
          ELSE IF ( TRIM(ylistfld(jf))=='rsn' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%rsn        
          ELSE IF ( TRIM(ylistfld(jf))=='tsf' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%tsf        
          ELSE IF ( TRIM(ylistfld(jf))=='ssi' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%ssi        
          ELSE IF ( TRIM(ylistfld(jf))=='age' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%age 
          ELSE IF ( TRIM(ylistfld(jf))=='vmp' ) THEN
            y3dd = .TRUE.
            zwork3d(:,:,:) = tpglt%sit_d(:,:,:)%vmp
          ELSE
            IF ( lwg ) THEN
              WRITE(noutlu,*) '**** WARNING ****'
              WRITE(noutlu,*) '   In routine imod_tools_chkinp'
              WRITE(noutlu,*) '     ==> field ' // TRIM(ylistfld(jf)) //  &
                ' is unknown.'
            ENDIF
        ENDIF
!
! .. Print information
!
        IF ( y3d ) THEN
            DO jk=1,nnflxin
              zmin = MINVAL( zwork3(jk,:,:),MASK=tpglt%dom%tmk==1 )
              zmax = MAXVAL( zwork3(jk,:,:),MASK=tpglt%dom%tmk==1 )
              zsum = SUM( zwork3(jk,:,:)*tpglt%dom%srf ) * zofac
              CALL mpp_min(zmin)
              CALL mpp_max(zmax)
              CALL mpp_sum(zsum)
              IF ( lwg ) THEN
                WRITE(noutlu,1020) jk
                WRITE(noutlu,1010) ADJUSTL(ylistfld(jf)),zmin,zmax,zsum
              ENDIF
            END DO
          ELSE IF ( y3dd ) THEN
            DO jk=1,ntd
              zmin = MINVAL( zwork3d(jk,:,:),MASK=tpglt%dom%tmk==1 )
              zmax = MAXVAL( zwork3d(jk,:,:),MASK=tpglt%dom%tmk==1 )
              zsum = SUM( zwork3d(jk,:,:)*tpglt%dom%srf ) * zofac
              CALL mpp_min(zmin)
              CALL mpp_max(zmax)
              CALL mpp_sum(zsum)
              IF ( lwg ) THEN
                WRITE(noutlu,1020) jk
                WRITE(noutlu,1010) ADJUSTL(ylistfld(jf)),zmin,zmax,zsum
              ENDIF
            END DO
          ELSE
            zmin = MINVAL( zwork2(:,:),MASK=tpglt%dom%tmk==1 ) 
            zmax = MAXVAL( zwork2(:,:),MASK=tpglt%dom%tmk==1 )
            zsum = SUM( zwork2(:,:)*tpglt%dom%srf ) * zofac
            IF ( lwg ) WRITE(noutlu,1010) ADJUSTL(ylistfld(jf)),zmin,zmax,zsum
        ENDIF
!
      END DO
!
      IF ( lwg ) THEN
        WRITE(noutlu,*) 
        WRITE(noutlu,*)  &
          '===================================================================='
        WRITE(noutlu,*) 
      ENDIF
      DEALLOCATE( ylistfld )
!
  ENDIF
!
!
!
! 3. Save gelato input fields
! ============================
!
  ydo = nsavinp==1 .AND. lwg
!
  IF ( ydo ) THEN
    WRITE(noutlu,*) 'CHKINP: Saving Gelato input data'
    WRITE(noutlu,*) '=================================='
    WRITE(noutlu,*) ' '
!
! .. Compute file name
!
    imonth = ( kdate - 10000*( kdate/10000 ) ) / 100
    WRITE( ystep,FMT='(I7)' ) tpglt%ind%cur
    WRITE( yfile,FMT='("/inpfld_",I2.2,"_",A)' )  &
      imonth,TRIM( ADJUSTL(ystep) )
    yfile = TRIM(ciopath) // TRIM( ADJUSTL(yfile) )
!
! .. Open and write file
!
    OPEN( UNIT=nsavlu, FILE=yfile, FORM='UNFORMATTED' ) 
  ENDIF
!
  IF ( nsavinp==1 ) THEN
    tznam = t_def( "","","BATHYOCE","","T","SCALAR" )
    CALL gltools_wrivai( tznam,tpglt%bat,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","OIQMLQML","","T","SCALAR" )
    CALL gltools_wrivai( tznam,tpglt%oce_all%qml,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","OIHEFHEF","","T","SCALAR" )
    CALL gltools_wrivai( tznam,tpglt%oce_all%qoc,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","OITMLTML","","T","SCALAR" )
    CALL gltools_wrivai( tznam,tpglt%oce_all%tml,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","OISMLSML","","T","SCALAR" )
    CALL gltools_wrivai( tznam,tpglt%oce_all%sml,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","OISSHSSH","","T","SCALAR" )
    CALL gltools_wrivai( tznam,tpglt%oce_all%ssh,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","OIUMLUML","","U","VECTOR" )
    CALL gltools_wrivai( tznam,tpglt%oce_all%uml,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","OIVMLVML","","V","VECTOR" )
    CALL gltools_wrivai( tznam,tpglt%oce_all%vml,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","AILIPLIP","","T","SCALAR" )
    CALL gltools_wrivai( tznam,tpglt%atm_all%lip,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","AISOPSOP","","T","SCALAR" )
    CALL gltools_wrivai( tznam,tpglt%atm_all%sop,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","AIZTXZTX","","U","VECTOR" )
    CALL gltools_wrivai( tznam,tpglt%atm_all%ztx,kunit=nsavlu,kdbl=1 ) 
    tznam = t_def( "","","AIMTYMTY","","V","VECTOR" )
    CALL gltools_wrivai( tznam,tpglt%atm_all%mty,kunit=nsavlu,kdbl=1 ) 
!
    IF ( nnflxin/=0 ) THEN
      DO jk=1,nnflxin
        WRITE(ynum,'(I2.2)') jk
        tznam = t_def( "","","AINSFI"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%atm_ice(jk,:,:)%nsf,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","AIDFLI"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%atm_ice(jk,:,:)%dfl,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","AISWAI"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%atm_ice(jk,:,:)%swa,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","AIEVAI"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%atm_ice(jk,:,:)%eva,kunit=nsavlu,kdbl=1 ) 
      END DO
!
      tznam = t_def( "","","AINSFWAT","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%atm_wat%nsf,kunit=nsavlu,kdbl=1 ) 
      tznam = t_def( "","","AIDFLWAT","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%atm_wat%dfl,kunit=nsavlu,kdbl=1 ) 
      tznam = t_def( "","","AISWAWAT","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%atm_wat%swa,kunit=nsavlu,kdbl=1 ) 
      tznam = t_def( "","","AIEVAWAT","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%atm_wat%eva,kunit=nsavlu,kdbl=1 ) 
!
    ELSE
!
      tznam = t_def( "","","AINSFMIX","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%atm_mix(1,:,:)%nsf,kunit=nsavlu,kdbl=1 ) 
      tznam = t_def( "","","AIDFLMIX","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%atm_mix(1,:,:)%dfl,kunit=nsavlu,kdbl=1 ) 
      tznam = t_def( "","","AISWAMIX","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%atm_mix(1,:,:)%swa,kunit=nsavlu,kdbl=1 ) 
      tznam = t_def( "","","AIEVAMIX","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%atm_mix(1,:,:)%eva,kunit=nsavlu,kdbl=1 ) 
!
    ENDIF
!
    IF ( ntd/=0 ) THEN
      DO jk=1,ntd
        WRITE(ynum,'(I2.2)') jk
        tznam = t_def( "","","ASNASN"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%asn,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","FSIFSI"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%fsi,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","HSIHSI"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%hsi,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","HSNHSN"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%hsn,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","RSNRSN"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%rsn,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","TSFTSF"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%tsf,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","SSISSI"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%ssi,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","AGEAGE"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%age,kunit=nsavlu,kdbl=1 ) 
        tznam = t_def( "","","VMPVMP"//ynum,"","T","SCALAR" )
        CALL gltools_wrivai( tznam,tpglt%sit_d(jk,:,:)%vmp,kunit=nsavlu,kdbl=1 ) 
      END DO
    ENDIF
!
  ENDIF
!
  IF ( ydo ) CLOSE(nsavlu)
!
!
!
! 4. Formats
! ===========
!
1010 FORMAT( 1X,A7,E12.5,4X,E12.5,4X,E12.5 )
1020 FORMAT( "Category",I2.2,":" )
!
!
END SUBROUTINE gltools_chkinp
!
! -------------------- END SUBROUTINE gltools_chkinp --------------------
! -----------------------------------------------------------------------
