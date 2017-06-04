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
! ====================== MODULE modi_gltools_chkout =======================
! =======================================================================
!
! Goal:
! -----
!   This module contains a subroutine that prints for every glt_gelato 
! glt_output field :
!   - global minimum
!   - global maximum
!   - global average (no weighing with grid cell surfaces)
!   - values for every field at a specified grid point
!   Note that MPP_SUM, MPP_MIN, MPP_MAX (invoke all procs) cannot be 
! used under condition lp2=.TRUE. HEnce we use the nprinto==2 condition.
!
! Created : 2003/12 (D. Salas y Melia)
! Modified: 2009/08 (D. Salas y Melia) Adapted to Gelato's new interface
! Modified: 2012/11 (D. Salas y Melia) parallelism & super-type
! Modified: 2014/01 (D. Salas y Melia) use standard wrivai for writing
!
! ------------------- BEGIN MODULE modi_gltools_chkout --------------------
!
!THXS_SFX!MODULE modi_gltools_chkout
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_chkout( kdate,tpglt )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  INTEGER, INTENT(in) ::  &
!THXS_SFX!    kdate
!THXS_SFX!  TYPE(t_glt), INTENT(in) ::  &
!THXS_SFX!    tpglt
!THXS_SFX!END SUBROUTINE gltools_chkout
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_chkout
!
! -------------------- END MODULE modi_gltools_chkout ---------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE gltools_chkout ---------------------------
!
! * Subroutine that prints mini, maxi and average of every Gelato glt_output
! field, plus field values at one specified grid point.
!
SUBROUTINE gltools_chkout( kdate,tpglt )
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
  TYPE(t_glt), INTENT(inout) ::  &
    tpglt
!
! .. Local variables
!
  LOGICAL ::  &
        y3d,ydo
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
  REAL, DIMENSION(SIZE(tpglt%ice_atm,1),SIZE(tpglt%dom,1),SIZE(tpglt%dom,2)) ::  &
        zwork3
  TYPE(t_def) ::  &
        tznam
!  
!
! 1. Initialisations
! ===================
!
  IF ( nprinto >= 2 ) THEN
!
! .. List of all fields to be analysed (in a single string variable)
! The user can change this list at will, provided that:
!       - the length of this string (named yallfld) is less than 
!       the declared value 
!       - every word in this string is associated to an action in 
!       the case construct that follows.
!
      IF ( nnflxin==0 ) THEN
          yfld='pfsit palbm ptsfm  &
&           pnsfi pswai pcdfli  &
&           pwatfli ptauxg ptauyg pustar psalf'
        ELSE
          yfld='pfsit palbi ptsfi  &
&           pnsfi pswai pcdfli  &
&           pwatfli ptauxg ptauyg pustar psalf'
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
          '===================== Control Gelato output data ' //  &
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
!
        IF ( TRIM(ylistfld(jf))=='pfsit' ) THEN
            IF ( nnflxin==0 ) THEN
                zwork2(:,:) = tpglt%mix_atm(1,:,:)%fsi
              ELSE
                y3d = .TRUE.
                zwork3(:,:,:) = tpglt%ice_atm(:,:,:)%fsi
            ENDIF
          ELSE IF ( TRIM(ylistfld(jf))=='palbi' ) THEN
            y3d = .TRUE.
            zwork3(:,:,:) = tpglt%ice_atm(:,:,:)%alb
          ELSE IF ( TRIM(ylistfld(jf))=='ptsfi' ) THEN
            y3d = .TRUE.
            zwork3(:,:,:) = tpglt%ice_atm(:,:,:)%tsf
          ELSE IF ( TRIM(ylistfld(jf))=='palbm' ) THEN
            zwork2(:,:) = tpglt%mix_atm(1,:,:)%alb
          ELSE IF ( TRIM(ylistfld(jf))=='ptsfm' ) THEN
            zwork2(:,:) = tpglt%mix_atm(1,:,:)%tsf
          ELSE IF ( TRIM(ylistfld(jf))=='pnsfi' ) THEN
            zwork2(:,:) = tpglt%all_oce(:,:)%nsf
          ELSE IF ( TRIM(ylistfld(jf))=='pswai' ) THEN
            zwork2(:,:) = tpglt%all_oce(:,:)%swa
          ELSE IF ( TRIM(ylistfld(jf))=='pcdfli' ) THEN
            zwork2(:,:) = tpglt%all_oce(:,:)%cdf
          ELSE IF ( TRIM(ylistfld(jf))=='pwatfli' ) THEN
            zwork2(:,:) = tpglt%all_oce(:,:)%wfl
          ELSE IF ( TRIM(ylistfld(jf))=='ptauxg' ) THEN
            zwork2(:,:) = tpglt%all_oce(:,:)%ztx
          ELSE IF ( TRIM(ylistfld(jf))=='ptauyg' ) THEN
            zwork2(:,:) = tpglt%all_oce(:,:)%mty
          ELSE IF ( TRIM(ylistfld(jf))=='pustar' ) THEN
            zwork2(:,:) = tpglt%all_oce(:,:)%ust
          ELSE IF ( TRIM(ylistfld(jf))=='psalf' ) THEN
            zwork2(:,:) = tpglt%all_oce(:,:)%saf
          ELSE
            IF ( lwg ) THEN
              WRITE(noutlu,*) '**** WARNING ****'
              WRITE(noutlu,*) '   In routine imod_tools_chkout'
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
! 3. Save gelato output fields
! =============================
!
  ydo = nsavout==1 .AND. lwg
!
  IF ( ydo ) THEN
!
      IF(lwg) THEN
        WRITE(noutlu,*) 'CHKOUT: Saving Gelato glt_output data'
        WRITE(noutlu,*) '=================================='
        WRITE(noutlu,*) ' '
      ENDIF 
!
! .. Compute file name
      imonth = ( kdate - 10000*( kdate/10000 ) ) / 100
      WRITE( ystep,FMT='(I7)' ) tpglt%ind%cur
      WRITE( yfile,FMT='("/outfld_",I2.2,"_",A)' )  &
        imonth,TRIM( ADJUSTL(ystep) )
      yfile = TRIM(ciopath) // TRIM( ADJUSTL(yfile) )
!
! .. Open and write file
!
    OPEN( UNIT=nsavlu, FILE=yfile, FORM='UNFORMATTED' ) 
  ENDIF
!
  IF ( nsavout==1 ) THEN
    IF (nnflxin /= 0) THEN 
      DO jk=1,nnflxin
        WRITE(ynum,'(I2.2)') jk
        tznam = t_def( "","","SIFRCI"//ynum,"","T","SCALAR" )
         CALL gltools_wrivai( tznam,tpglt%ice_atm(jk,:,:)%fsi,kunit=nsavlu,kdbl=1 )
         tznam = t_def( "","","SIALBI"//ynum,"","T","SCALAR" )
         CALL gltools_wrivai( tznam,tpglt%ice_atm(jk,:,:)%alb,kunit=nsavlu,kdbl=1 )
         tznam = t_def( "","","SITEMI"//ynum,"","T","SCALAR" )
         CALL gltools_wrivai( tznam,tpglt%ice_atm(jk,:,:)%tsf,kunit=nsavlu,kdbl=1 )
      END DO
!
    ELSE
!
      tznam = t_def( "","","SIFRCSIS","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%mix_atm(1,:,:)%fsi,kunit=nsavlu,kdbl=1 ) 
      tznam = t_def( "","","SIALBSIM","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%mix_atm(1,:,:)%alb,kunit=nsavlu,kdbl=1 ) 
      tznam = t_def( "","","SITEMSIW","","T","SCALAR" )
      CALL gltools_wrivai( tznam,tpglt%mix_atm(1,:,:)%tsf,kunit=nsavlu,kdbl=1 ) 
      !
    ENDIF
!
   tznam = t_def( "","","PNSFIXXX","","T","SCALAR" )
   CALL gltools_wrivai( tznam,tpglt%all_oce%nsf,kunit=nsavlu,kdbl=1 ) 
   tznam = t_def( "","","PSWAIXXX","","T","SCALAR" )
   CALL gltools_wrivai( tznam,tpglt%all_oce%swa,kunit=nsavlu,kdbl=1 ) 
   tznam = t_def( "","","PCDFLIXX","","T","SCALAR" )
   CALL gltools_wrivai( tznam,tpglt%all_oce%cdf,kunit=nsavlu,kdbl=1 ) 
   tznam = t_def( "","","PWATFLIX","","T","SCALAR" )
   CALL gltools_wrivai( tznam,tpglt%all_oce%wfl,kunit=nsavlu,kdbl=1 ) 
   tznam = t_def( "","","PTAUXGXX","","U","VECTOR" )
   CALL gltools_wrivai( tznam,tpglt%all_oce%ztx,kunit=nsavlu,kdbl=1 ) 
   tznam = t_def( "","","PTAUYGXX","","V","VECTOR" )
   CALL gltools_wrivai( tznam,tpglt%all_oce%mty,kunit=nsavlu,kdbl=1 ) 
   tznam = t_def( "","","PUSTARXX","","T","SCALAR" )
   CALL gltools_wrivai( tznam,tpglt%all_oce%ust,kunit=nsavlu,kdbl=1 ) 
   tznam = t_def( "","","SALFLUXX","","T","SCALAR" )
   CALL gltools_wrivai( tznam,tpglt%all_oce%saf,kunit=nsavlu,kdbl=1 ) 
  ENDIF
!
  IF ( ydo ) CLOSE(nsavlu)
!
!
!
! 4. End of time step operations
! ===============================
!
  IF(lp1) THEN
    WRITE(noutlu,*)  &
      '                ************************************'
    WRITE(noutlu,*)  &
      '                 END OF gelato TIME STEP Nr =',tpglt%ind%cur
    WRITE(noutlu,*)  &
      '                ************************************'
    CALL FLUSH(noutlu)
  ENDIF
!
!
!
! 5. Formats
! ===========
!
1010 FORMAT( 1X,A7,E12.5,4X,E12.5,4X,E12.5 )
1020 FORMAT( "Category",I2.2,":" )
!
END SUBROUTINE gltools_chkout
!
! -------------------- END SUBROUTINE gltools_chkout --------------------
! -----------------------------------------------------------------------
