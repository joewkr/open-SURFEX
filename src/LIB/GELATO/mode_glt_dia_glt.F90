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
! ========================= MODULE modi_glt_dia_glt =========================
! =======================================================================
!
! 
! * Contains a subroutine that writes model glt_output in Gelato format
!
! --------------------- BEGIN MODULE modi_glt_dia_glt -----------------------

MODULE mode_glt_dia_glt
!INTERFACE
!
! SUBROUTINE wridiag_glt  &
!   ( tpind,tpdom,tpml,tptfl,tpblkw,tpblki,tpsit,tpbud,tpdia,pcumdia )
!   USE modd_types_glt
!   USE modd_glt_param
!   TYPE(t_ind), INTENT(inout) ::  &
!         tpind
!   TYPE(t_dom), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
!         tpdom
!   TYPE(t_mxl), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
!         tpml
!   TYPE(t_tfl), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
!         tptfl 
!   TYPE(t_blk), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
!         tpblkw
!   TYPE(t_blk), DIMENSION(nt,nxglo,nyglo), INTENT(in) ::  &
!         tpblki
!   TYPE(t_sit), DIMENSION(nt,nxglo,nyglo), INTENT(in) ::  &
!         tpsit
!   TYPE(t_bud), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
!         tpbud
!   TYPE(t_dia), DIMENSION(nxglo,nyglo), INTENT(inout) ::  &
!         tpdia
!   REAL, DIMENSION(ndiamax,nxglo,nyglo), INTENT(inout) ::  &
!         pcumdia
! END SUBROUTINE wridiag_glt 
! !
! END INTERFACE

CONTAINS 
!
! ---------------------- END MODULE modi_glt_dia_glt ------------------------
!
!
!
! -----------------------------------------------------------------------
! ------------------------ SUBROUTINE WRIDIAG_GLT -----------------------

! * A subroutine that computes interesting quantities from certain
! icestate variables (statistics) and records them in data files at 
! every time step.

SUBROUTINE wridiag_glt  &
  ( tpind,tpdom,tpml,tptfl,tpblkw,tpblki,tpsit,tpbud,tpdia,pcumdia )
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
  USE modi_gltools_avevai
  USE mode_gltools_wrivais
  USE modi_gltools_outdia
  USE mode_glt_stats
  USE modi_gltools_glterr
  IMPLICIT none
!
  TYPE(t_ind), INTENT(inout) ::  &
        tpind
  TYPE(t_dom), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
        tpdom
  TYPE(t_mxl), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
        tpml
  TYPE(t_tfl), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
        tptfl 
  TYPE(t_blk), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
        tpblkw
  TYPE(t_blk), DIMENSION(nt,nxglo,nyglo), INTENT(in) ::  &
        tpblki
  TYPE(t_sit), DIMENSION(nt,nxglo,nyglo), INTENT(in) ::  &
        tpsit
  TYPE(t_bud), DIMENSION(nxglo,nyglo), INTENT(in) ::  &
        tpbud
  TYPE(t_dia), DIMENSION(nxglo,nyglo), INTENT(inout) ::  &
        tpdia
  REAL, DIMENSION(ndiamax,nxglo,nyglo), INTENT(inout) ::  &
        pcumdia
!
  CHARACTER(8) ::  &
        yword
  CHARACTER(80) ::  &
        yfname,ymess
  INTEGER ::  &
        ji,jt,ii,ij,ii0,ij0
  LOGICAL, DIMENSION(nxglo,nyglo) ::  &
        ynhemis,yshemis
  REAL ::  &
        zai,zaj,zcslat,zdilat,zdilon,zdjlat,zdjlon
  REAL(KIND=4) ::  &
        zehn,zehs,zshn,zshs,zvhn,zvhs,zwhn,zwhs,  &
        zfram,zbering,zncwest,znceast,znorthb
  REAL, DIMENSION(nxglo,nyglo) ::  &
        zfsit,zhsit,zhsnt,zmsnt
  REAL, DIMENSION(nxglo,nyglo) ::  &
        zwork2
  REAL, DIMENSION(nt,nxglo,nyglo) ::  &
        zwork3
  TYPE(t_def) ::  &
        tznam
!
!
#if ! defined in_surfex
!
! 1. Initializations
! ===================
!
! .. Arrays
!
  zwork2(:,:) = 0.
  zwork3(:,:,:) = 0.
! 
! .. Welcome message
!
  IF (lwg) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - SUBROUTINE WRIDIAG_GLT'
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '     --> Write diagnostic files'
  ENDIF
!
! .. Compute total sea ice concentration with threshold, net total sea
! ice concentration, sea ice average thickness
!
  zfsit(:,:) = glt_iceconcm( tpdom,tpsit )
  zhsit(:,:) = glt_avhicem( tpdom,tpsit )
  zhsnt(:,:) = glt_avhsnwm( tpdom,tpsit )      
  zmsnt(:,:) = glt_avmsnwm( tpdom,tpsit )      
!
! .. Time counter
!
  tpind%nts = tpind%nts + 1
!
! .. Set field counters to zero (field index in cumulated diagnostics array)
! Has to be done before writing first 0d and 2d arrays
!
  tpind%i0d = 0
  tpind%i2d = 0
!
! .. For 'specialised averaging' (e.g. ice age or salinity), count time steps
! when there is sea ice
!
  tpdia(:,:)%sic = tpdia(:,:)%sic + zfsit(:,:)
  tpdia(:,:)%sit = tpdia(:,:)%sit + zhsit(:,:)
!
!
!
! 2. Write first set of diagnostics
! ==================================
!
  IF ( ndiap1==1 ) THEN 
!
! >>> Write sea ice u-velocity field [m.s-1]
!
      zwork2(:,:) = tpdia(:,:)%uvl
      yword = 'SIUVLSIT'
      tznam = t_def( " ", " ", yword, " ", "U", "VECTOR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write sea ice v-velocity field [m.s-1]
!
      zwork2(:,:) = tpdia(:,:)%vvl
      yword = 'SIVVLSIT'
      tznam = t_def( " ", " ", yword, " ", "V", "VECTOR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
! 
! >>> Write sea ice average thickness field [m]
!
      zwork2(:,:) = zhsit(:,:)*FLOAT( tpdom(:,:)%tmk ) 
      yword = 'SIHHHSIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write snow average thickness field [m]
!
      zwork2(:,:) = zhsnt(:,:)*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIHHHSNW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write snow average density field [kg.m-3]
!
      zwork2(:,:) = zmsnt(:,:)*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIMMMSNW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write total ice concentration field [0-1]
!
      zwork2(:,:) = zfsit(:,:)*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIFRCSIS'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >> Write fraction of time during which sea ice is present [0-1]
!
      WHERE( zfsit(:,:)>xfsic )
          zwork2(:,:) = 1.
      ENDWHERE
      yword = 'SICETIME'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >> Write average fraction of the marine surface that melts [0-1]
!
      WHERE( ABS( tpsit(:,:,:)%tsf-tice_m ) < epsil1 )
          zwork3(:,:,:) = tpsit(:,:,:)%fsi
      ENDWHERE
      zwork2(:,:) =  &
        SUM( zwork3(:,:,:),DIM=1 )*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIMELTFR'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write average surface temperature field [deg C]
!
      zwork2(:,:) =  &
        ( SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%tsf,DIM=1 ) +  &
        ( 1.-zfsit(:,:) )*tpml(:,:)%tml )*FLOAT( tpdom(:,:)%tmk )
      yword = 'SITEMSMW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write average surface albedo field (all marine surface) [0-1]
!
      zwork2(:,:) =  &
        ( SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%asn,DIM=1 ) +  &
        ( 1.-zfsit(:,:) )*albw )*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIALBSMW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write average surface albedo field (ice only) [0-1]
!
      WHERE( tpdom(:,:)%tmk==1 .AND. zfsit(:,:)>=xiok )
          zwork2(:,:) =  &
            SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%asn, DIM=1 ) / zfsit(:,:)
        ELSEWHERE
          zwork2(:,:) = 0.
      ENDWHERE
      yword = 'SIALBSIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia  &
        ( tpind,tznam,tpdom,zwork2,pcumdia,pwgt=tpdia%sic )
!
! >>> Write average ice salinity [psu]
!
      IF ( nicesal==1 ) THEN
          WHERE( tpdom(:,:)%tmk==1 .AND. zfsit(:,:)>=xiok )
              zwork2(:,:) =  &
                SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%hsi*tpsit(:,:,:)%ssi, DIM=1 )
            ELSEWHERE
              zwork2(:,:) = 0.
          ENDWHERE
          yword = 'SISALSIW'
          tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
          CALL gltools_outdia  &
            ( tpind,tznam,tpdom,zwork2,pcumdia,pwgt=tpdia%sit )
      ENDIF
!
! >>> Write average surface ice age [years]
!
      IF ( niceage==1 ) THEN
          WHERE( tpdom(:,:)%tmk==1 .AND. zfsit(:,:)>=xiok )
              zwork2(:,:) =  &
                SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%age, DIM=1 ) /  &
                  ( xyear2day*xday2sec )
            ELSEWHERE
              zwork2(:,:) = 0.
          ENDWHERE
          yword = 'SIAGESIW'
          tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
          CALL gltools_outdia  &
            ( tpind,tznam,tpdom,zwork2,pcumdia,pwgt=tpdia%sic )
      ENDIF
!
! >>> Write average surface ice age [years]
!
      IF ( nmponds==1 ) THEN
          WHERE( tpdom(:,:)%tmk==1 .AND. zfsit(:,:)>=xiok )
              zwork2(:,:) =  &
                SUM( tpsit(:,:,:)%fsi*tpsit(:,:,:)%vmp, DIM=1 )
            ELSEWHERE
              zwork2(:,:) = 0.
          ENDWHERE
          yword = 'SIVMPSIW'
          tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
          CALL gltools_outdia  &
            ( tpind,tznam,tpdom,zwork2,pcumdia,pwgt=tpdia%sic )
      ENDIF
!
  ENDIF
!
!
! 
! 3. Write second set of diagnostics
! ===================================
!
! .. Note that the outgoing heat flux affecting ice free areas are 
! exactly equal to the incoming heat flux in the same zones.
!
! NOTES 
! ------
!  * If you want to compute a complete energy balance on sea ice,
! you must compare, on the one hand:
!    . SITDENIW (gltools_enthalpy change)
! And, on the other hand:
!    . OIHFLUIW + AIHFLUIW + AISNWFIW + AWHFLUWW + AWSNWFWW - 
! ( IOLFLUIW + IOTFLUIW + LOLFLUIW + LOTFLUIW )
! i.e. 
! ( ocean heat flux + 
!   atmospheric heat flux on ice + energy flux due to snowfalls on ice + 
!   atmospheric heat flux on water + energy flux due to snowfalls on water )
! minus
! ( outgoing short wave + non-solar through leads +
!   outgoing short wave + non-solar through ice )
!  
!  - outgoing energy (solar+non-solar) at the bottom of sea ice)
!
!  * If you want to compute a complete fresh water balance on sea ice, 
! you must compare, on the one hand:
!    ALLFWTOT (all precip-evapo) - LOWFLUIW - IOWFLUIW (outgoing water 
! through leads and under sea ice)
! And, on the other hand:
!    SITDSIIW + SIDDSIIW  (sea ice mass change due to glt_thermo + dynamics) 
! + (SITDSNIW + SIDDSNIW) (snow mass changes due to glt_thermo + dynamics)
! - (SITDSAIW + SIDDSAIW) (salt mass changes due to glt_thermo + dynamics)
! 
!  * The energetic balance due to the (non perfectly conservative) sea 
! ice advection is also available, see SIDDENIW and SIDDLAIW fields.
!
!  * The change in water budget due to dynamics is not implemented yet.
!
  IF ( ndiap2==1 ) THEN 
!  
! >>> Write ocean heat flux - weighed [W.m-2]
!
      zwork2(:,:) = tpdia(:,:)%qoi*FLOAT( tpdom(:,:)%tmk )
      yword = 'OIHFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write equivalent heat flux due to snow melting in the ocean [W.m-2]
!
      zwork2(:,:) = tpbud(:,:)%nli*FLOAT(tpdom(:,:)%tmk)
      yword = 'AWSNWFWW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write equivalent heat flux due to snowfalls on sea ice [W.m-2]
!
      zwork2(:,:) = tpbud(:,:)%nii
      yword = 'AISNWFIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write net heat flux on the ice surface only - weighed [W.m-2]
!     (without the effect of snow)
!
      zwork2(:,:) = tpbud(:,:)%hii-tpbud(:,:)%nii
      yword = 'AIHFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write net heat flux on the water surface only - weighed [W.m-2]
!     (without the effect of snow)
!
      zwork2(:,:) = (tpbud(:,:)%hli-tpbud(:,:)%nli)*FLOAT(tpdom(:,:)%tmk)
      yword = 'AWHFLUWW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write solar energy absorbed by the water surface [W.m-2]
!
      zwork2(:,:) = tpblkw(:,:)%swa*FLOAT( tpdom(:,:)%tmk )
      yword = 'AWSFLUWW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighed sea ice gltools_enthalpy variation due to thermodynamics [W.m-2]
!
      zwork2(:,:) = tpdia(:,:)%the
      yword = 'SITDENIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighed sea ice gltools_enthalpy variation due to advection [W.m-2]
!
      zwork2(:,:) =  &
        ( tpbud(:,:)%enn-tpbud(:,:)%eni ) / dtt - tpdia(:,:)%the
      yword = 'SIDDENIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted net FW flux sent by sea ice to the ocean [W.m-2]
!
      zwork2(:,:) = tptfl(:,:)%wio 
      yword = 'IOWFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted net FW flux sent by leads to the ocean [kg.m-2.s-1]
!
      zwork2(:,:) = tptfl(:,:)%wlo*FLOAT( tpdom(:,:)%tmk )
      yword = 'LOWFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted virtual FW flux sent by sea ice to the ocean [kg.m-2.s-1]
!
      zwork2(:,:) = tptfl(:,:)%cio
      yword = 'IOVFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted salt flux sent by sea ice to the ocean
!
      zwork2(:,:) = tptfl(:,:)%sio
      yword = 'IOSFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted solar heat flux sent by sea ice to the ocean
!
      zwork2(:,:) = tptfl(:,:)%lio
      yword = 'IOLFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted non-solar heat flux sent by sea ice to the ocean
!
      zwork2(:,:) = tptfl(:,:)%tio
      yword = 'IOTFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted solar heat flux sent by leads to the ocean
!
      zwork2(:,:) = tptfl(:,:)%llo*FLOAT( tpdom(:,:)%tmk )
      yword = 'LOLFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted non-solar heat flux sent by leads to the ocean
!
      zwork2(:,:) = tptfl(:,:)%tlo*FLOAT( tpdom(:,:)%tmk )
      yword = 'LOTFLUIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted salt mass change field - glt_thermo only [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%dsa*FLOAT( tpdom(:,:)%tmk )
      yword = 'SITDSAIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted snow mass change field - glt_thermo only [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%dsn*FLOAT( tpdom(:,:)%tmk )
      yword = 'SITDSNIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted sea ice mass change field - glt_thermo only [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%dsi*FLOAT( tpdom(:,:)%tmk )
      yword = 'SITDSIIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted salt mass change field - dynamics only [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%dds*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIDDSAIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted snow mass change field - dynamics only [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%ddn*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIDDSNIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted sea ice mass change field - dynamics only [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%ddi*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIDDSIIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted ice FW content change field - glt_thermo only [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%dwi*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIDMWIIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write total input water to the snow-ice leads system [kg.m-2.s-1]
!
      zwork2(:,:) = tpdia(:,:)%ifw*FLOAT( tpdom(:,:)%tmk )
      yword = 'ALLFWTOT'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write ice production in leads [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%lsi*FLOAT( tpdom(:,:)%tmk )
      yword = 'SILDSIIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted ice top mass balance - positive if melting [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%mrt*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIMRTIIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted ice lateral ablation [ kg.m-2.s-1 ]
!
      zwork2(:,:) = tpdia(:,:)%mrl*FLOAT( tpdom(:,:)%tmk )
      yword = 'SIMRLIIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write weighted ice bottom mass balance [ kg.m-2.s-1 ]
!
      zwork2(:,:) =   &
        ( tpdia(:,:)%dsi-tpdia(:,:)%lsi-tpdia(:,:)%mrt-tpdia(:,:)%mrl )*  &
          FLOAT( tpdom(:,:)%tmk )
      yword = 'SIMRBIIW'
      tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
      CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write spare fields 
!
      IF ( ANY( ABS( tpdia(:,:)%sp1 ) > epsil2 ) ) THEN 
        zwork2(:,:) =   &
          tpdia(:,:)%sp1*FLOAT( tpdom(:,:)%tmk )
        yword = 'FIELDSP1'
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
      ENDIF
!
      IF ( ANY( ABS( tpdia(:,:)%sp2 ) > epsil2 ) ) THEN 
        zwork2(:,:) =   &
          tpdia(:,:)%sp2*FLOAT( tpdom(:,:)%tmk )
        yword = 'FIELDSP2'
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
      ENDIF
!
      DO jt=1,nt
!
! >>> Write sea ice categories concentration fields
!
        zwork2(:,:) = tpsit(jt,:,:)%fsi*FLOAT( tpdom(:,:)%tmk )
        WRITE( yword,FMT='("SIFRCSI",I1.1)' ) jt
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write sea ice categories thickness fields
!
        zwork2(:,:) = tpsit(jt,:,:)%hsi*FLOAT( tpdom(:,:)%tmk )
        WRITE( yword,FMT='("SIHHHSI",I1.1)' ) jt
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write sea ice categories surface temperature
!
        zwork2(:,:) = tpsit(jt,:,:)%tsf*FLOAT( tpdom(:,:)%tmk )
        WRITE( yword,FMT='("SITEMSI",I1.1)' ) jt
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write melt pond volume over each ice category
!
        zwork2(:,:) = tpsit(jt,:,:)%vmp*FLOAT( tpdom(:,:)%tmk )
        WRITE( yword,FMT='("SIVMPSI",I1.1)' ) jt
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write sea ice category albedo
!
        zwork2(:,:) = tpsit(jt,:,:)%asn*FLOAT( tpdom(:,:)%tmk )
        WRITE( yword,FMT='("SIALBSI",I1.1)' ) jt
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write solar energy absorbed by sea ice categories
!
        zwork2(:,:) = tpblki(jt,:,:)%swa*FLOAT( tpdom(:,:)%tmk )
        WRITE( yword,FMT='("AISWASI",I1.1)' ) jt
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
! >>> Write non-solar energy absorbed by sea ice categories
!
        zwork2(:,:) = tpblki(jt,:,:)%nsf*FLOAT( tpdom(:,:)%tmk )
        WRITE( yword,FMT='("SINSFSI",I1.1)' ) jt
        tznam = t_def( " ", " ", yword, " ", "T", "SCALAR" )
        CALL gltools_outdia( tpind,tznam,tpdom,zwork2,pcumdia )
!
      END DO
!
  ENDIF
!
! .. Just a final check, if ndiamax (in gltpar) is too small to hold all the
! data
!
  IF ( tpind%cur==tpind%beg ) THEN
      IF ( tpind%i2d>ndiamax ) THEN 
          WRITE( ymess,  &
            FMT='("Number of 2d diagnostic fields=", &
            &  I3,"> ndiamax=",I3,"\n")' ) tpind%i2d,ndiamax
          CALL gltools_glterr( 'imod_results','Check ndiamax in gltpar', 'STOP' )
      ENDIF 
      IF ( tpind%i0d>ndiamax ) THEN 
          WRITE( ymess,  &
            FMT='("Number of 0d diagnostic fields=", &
            &  I3,"> ndiamax=",I3,"\n")' ) tpind%i0d,ndiamax
          CALL gltools_glterr( 'imod_results','Check ndiamax in gltpar', 'STOP' )
      ENDIF 
  ENDIF
!
!
!
! 3. Sea ice totals for Northern and Southern hemispheres
! ========================================================
!
! 3.1. Compute sea ice totals
! ----------------------------
! 
! The following quantities are computed :
!   - sea ice extent (sum of all grid cells with more than X % ice
!     concentration)
!   - sea ice area
!   - sea ice volume
!
! .. Sea ice extent, north and south
!
  ynhemis(:,:) = ( tpdom(:,:)%lat>0..AND.tpdom(:,:)%tmk==1 )
  yshemis(:,:) = ( tpdom(:,:)%lat<0..AND.tpdom(:,:)%tmk==1 )
  IF ( ndiap3==1 ) THEN
      zehn = SUM(tpdom(:,:)%srf, MASK=(ynhemis.AND.zfsit(:,:)>xfsic)) / 1.E+12
      zehs = SUM(tpdom(:,:)%srf, MASK=(yshemis.AND.zfsit(:,:)>xfsic)) / 1.E+12
  ENDIF 
!
! .. Sea ice area, north and south
!
  zshn = SUM(tpdom(:,:)%srf*zfsit(:,:), MASK=ynhemis) / 1.E+12
  zshs = SUM(tpdom(:,:)%srf*zfsit(:,:), MASK=yshemis) / 1.E+12
!
! .. Sea ice volume, north and south
!
  zvhn = SUM(tpdom(:,:)%srf*zhsit(:,:), MASK=ynhemis) / 1.E+12
  zvhs = SUM(tpdom(:,:)%srf*zhsit(:,:), MASK=yshemis) / 1.E+12
!
! .. Snow volume, north and south
!
  zwhn = SUM(tpdom(:,:)%srf*zhsnt(:,:), MASK=ynhemis) / 1.E+12
  zwhs = SUM(tpdom(:,:)%srf*zhsnt(:,:), MASK=yshemis) / 1.E+12
!
! .. Ice glt_transport through straits (depends on mesh geometry) 
!
  zfram = 0.
  zbering = 0.
  zncwest = 0.
  znceast = 0.
  znorthb = 0.
!
  IF ( cgrdname=='NEMO1' ) THEN
!
! Fram Strait
      ii0 = 268
      ij0 = 271
!
      DO ji=1,10
        ii = ii0+ji-1
        ij = ij0
        zfram = zfram +  &
          iceflx( tpdom,zhsit,tpdia,ii,ij,ii,ij+1 )
      END DO
      zfram = -zfram
!
! Bering Strait
      ii0 = 114
      ij0 = 245
!
      DO ji=1,2
        ii = ii0+ji-1
        ij = ij0
        zbering = zbering +  &
          iceflx( tpdom,zhsit,tpdia,ii,ij,ii,ij+1 )
      END DO
      zbering = -zbering
!
! North Canadian Archipelago (West)
      ii0 = 231
      ij0 = 288
!
      DO ji=1,2
        ii = ii0
        ij = ij0+ji-1
        zncwest = zncwest +  &
          iceflx( tpdom,zhsit,tpdia,ii,ij,ii+1,ij )  
      END DO
      zncwest = -zncwest
!
! Nares Strait (between Ellesmere Land and North Western Greenland)
!  - we compute the ice flux at the northern boundary of this strait (at its 
! Arctic Ocean boundary)
      ii0 = 252
      ij0 = 276
!
      DO ji=1,2
        ii = ii0
        ij = ij0+ji-1
        znceast = znceast +  &
          iceflx( tpdom,zhsit,tpdia,ii,ij,ii+1,ij )  
      END DO
      znceast = -znceast
!
! Barrow Strait (between Prince of Wales I. - south and Bathurst I. - north)
!  - we compute the ice flux 
      ii0 = 282
      ij0 = 273
!
      DO ji=1,14
        ii = ii0+ji-1
        ij = ij0+ji-1
        znorthb = znorthb -  &
          iceflx( tpdom,zhsit,tpdia,ii,ij,ii,ij+1 ) +  &
          iceflx( tpdom,zhsit,tpdia,ii-1,ij,ii,ij )  
      END DO
      znorthb = -znorthb
!
  ENDIF
!
!
! 3.2. Write totals to diagnostic file
! -------------------------------------
!
  IF ( ndiap3==1 ) THEN 
!
! >>> Write north ice extent
!
          WRITE(n0vilu) 'SIEHNSIG'
          WRITE(n0vilu) zehn
! 
! >>> Write south ice extent
!
          WRITE(n0vilu) 'SIEHSSIG'
          WRITE(n0vilu) zehs
! 
! >>> Write north ice area
!
          WRITE(n0vilu) 'SISHNSIG'
          WRITE(n0vilu) zshn
! 
! >>> Write south ice area
!
          WRITE(n0vilu) 'SISHSSIG'
          WRITE(n0vilu) zshs
! 
! >>> Write north ice volume
!
          WRITE(n0vilu) 'SIVHNSIG'
          WRITE(n0vilu) zvhn
!
! >>> Write south ice area
!
          WRITE(n0vilu) 'SIVHSSIG'
          WRITE(n0vilu) zvhs
!  
! >>> Write north snow volume
!
          WRITE(n0vilu) 'SIWHNSIG'
          WRITE(n0vilu) zwhn
!
! >>> Write south snow volume
!
          WRITE(n0vilu) 'SIWHSSIG'
          WRITE(n0vilu) zwhs
! 
! >>> Fram Strait sea ice outflow
!
          WRITE(n0vilu) 'SIFRAMST'
          WRITE(n0vilu) zfram
! 
! >>> Bering Strait sea ice outflow
!
          WRITE(n0vilu) 'SIBERING'
          WRITE(n0vilu) zbering
! 
! >>> North Canadian Archipelago (West) sea ice outflow
!
          WRITE(n0vilu) 'SINCWEST'
          WRITE(n0vilu) zncwest
! 
! >>> North Canadian Archipelago (East) sea ice outflow
!
          WRITE(n0vilu) 'SINCEAST'
          WRITE(n0vilu) znceast
! 
! >>> North Barents Sea sea ice outflow
!
          WRITE(n0vilu) 'SINORTHB'
          WRITE(n0vilu) znorthb
!
  ENDIF
!
!
! 3.3. Print out some important statistics to glt_output file
! --------------------------------------------------------
!
  IF (lwg) THEN
    WRITE(noutlu,*) '                              North        South'
    WRITE(noutlu,1000) zshn,zshs
    IF ( ndiap3==1 ) THEN 
      WRITE(noutlu,1100) zehn,zehs
    ENDIF
    WRITE(noutlu,1200) zwhn,zwhs
    WRITE(noutlu,1300) zvhn,zvhs
    WRITE(noutlu,*) '    Ice flux at Fram : ',zfram
    WRITE(noutlu,*) ' '
  ENDIF
!
!
!
! 4. End of time step operations
! ===============================
!
! .. Formats
!
1000 FORMAT(5X,"Ice surface (SISH.SIG)",2(4X,F9.5))
1100 FORMAT(5X,"Ice extent  (SIEH.SIG)",2(4X,F9.5))
1200 FORMAT(5X,"Snow volume (SIWH.SIG)",2(4X,F9.5))
1300 FORMAT(5X,"Ice volume  (SIVH.SIG)",2(4X,F9.5))
!
  IF (lwg) THEN
    WRITE(noutlu,*)  &
      '                ************************************'
    WRITE(noutlu,*)  &
      '                 END OF glt_gelato TIME STEP Nr =',tpind%cur
    WRITE(noutlu,*)  &
      '                ************************************'
!
! .. Farewell message
!
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - END SUBROUTINE WRIDIAG_GLT'
    WRITE(noutlu,*) ' '
  ENDIF
#else
 WRITE(noutlu,*) ' wri_dia_glt doesn t work in Surfex'
#endif
!
END SUBROUTINE wridiag_glt 
!
! ---------------------- END SUBROUTINE WRIDIAG_GLT -----------------------
! -----------------------------------------------------------------------
END MODULE mode_glt_dia_glt
