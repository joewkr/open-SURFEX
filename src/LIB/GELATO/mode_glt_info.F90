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
! =========================== MODULE mode_glt_info ==========================
! =======================================================================
!
! * This module contains subroutines that perform statistics on 
! different variable types. For example, subroutine glt_info_si can be used 
! to know which are the extremes and averages of sea ice thicknesses,
! concentrations, and so on.
!

! ------------------------ BEGIN MODULE mode_glt_info ----------------------

MODULE mode_glt_info
INTERFACE 

SUBROUTINE glt_info_si(omess,tpdom,tpsit,tpsil)
  USE modd_types_glt
  USE modd_glt_param
  CHARACTER(*) ::                                                              &
        omess
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), OPTIONAL, INTENT(in) ::             &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,nx,ny), OPTIONAL, INTENT(in) ::          &
        tpsil
END SUBROUTINE glt_info_si

END INTERFACE
END MODULE mode_glt_info

! ------------------------ END MODULE mode_glt_info -------------------------



! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE glt_info_si -------------------------
!
! * This subroutine computes sea ice fields maxima, minima, averages, 
! on a domain that is masked by tpdom%tmk.
! * The considered fields are the following :
!       - u,v sea ice velocities
!       - sea ice albedo
!       - sea ice fraction
!       - sea ice thickness
!       - snow layer thickness
!       - heat reservoir in the ice slab
!       - snow layer density
!       - surface temperature.

SUBROUTINE glt_info_si(omess,tpdom,tpsit,tpsil)
!
  USE modd_types_glt
  USE modd_glt_param
  USE modd_glt_const_thm
!
  IMPLICIT NONE

  CHARACTER(*) ::                                                              &
        omess
  TYPE(t_dom), DIMENSION(nx,ny), INTENT(in) ::                          &
        tpdom
  TYPE(t_sit), DIMENSION(nt,nx,ny), OPTIONAL, INTENT(in) ::           &
        tpsit
  TYPE(t_vtp), DIMENSION(nl,nt,nx,ny), OPTIONAL, INTENT(in) ::          &
        tpsil
  INTEGER ::                                                            &
        jk,jl,nbpt
  INTEGER, DIMENSION(2) ::                                              &
        imin,imax
  REAL ::                                                               &
        zfav,zmin,zmax
  REAL, DIMENSION(nx,ny) ::                                             &
        zfsit,zftot


  IF (lp2) THEN
!
! *** Define glt_output logical unit.
!
      WRITE(noutlu,*) ' '
      WRITE(noutlu,*) '                      ****',omess,'****'
      WRITE(noutlu,*) ' '
!
! *** Compute the number of grid points.
!
      nbpt = SUM(tpdom(:,:)%tmk)
!
  ENDIF
!
! *** Performs statistics on sea ice 3D variables.
!
  IF (PRESENT(tpsit)) THEN
!
      IF (lp2) THEN
          zfsit(:,:) = SUM(tpsit(:,:,:)%fsi,DIM=1)
          zmin = MINVAL(zfsit(:,:),MASK=(tpdom(:,:)%tmk==1))
          zmax = MAXVAL(zfsit(:,:),MASK=(tpdom(:,:)%tmk==1))
          imin(:) = MINLOC(zfsit(:,:),MASK=(tpdom(:,:)%tmk==1))
          imax(:) = MAXLOC(zfsit(:,:),MASK=(tpdom(:,:)%tmk==1))
          WRITE(noutlu,1300)  &
            'fsit :',zmin,imin(1),imin(2),zmax,imax(1),imax(2)
          WRITE(noutlu,*) ' ' 
      ENDIF
!
      IF (lp3) THEN
          DO jk = 1,nt
!
! * Snow albedo.
!
            zfav = SUM(tpsit(jk,:,:)%asn,MASK=(tpdom(:,:)%tmk==1)) /  &
              FLOAT(nbpt)
            zmin = MINVAL(tpsit(jk,:,:)%asn,MASK=(tpdom(:,:)%tmk==1))
            zmax = MAXVAL(tpsit(jk,:,:)%asn,MASK=(tpdom(:,:)%tmk==1))
            imin(:) = MINLOC(tpsit(jk,:,:)%asn,MASK=(tpdom(:,:)%tmk==1)) 
            imax(:) = MAXLOC(tpsit(jk,:,:)%asn,MASK=(tpdom(:,:)%tmk==1)) 
!        
            WRITE(noutlu,1400) jk,'asn :',zmin,imin(1),imin(2),  &
              zmax,imax(1),imax(2),zfav
! 
! * Sea ice fraction.
!
            zfav = SUM(tpsit(jk,:,:)%fsi,MASK=(tpdom(:,:)%tmk==1)) /  &
              FLOAT(nbpt)
            zmin = MINVAL(tpsit(jk,:,:)%fsi,MASK=(tpdom(:,:)%tmk==1))
            zmax = MAXVAL(tpsit(jk,:,:)%fsi,MASK=(tpdom(:,:)%tmk==1))
            imin(:) = MINLOC(tpsit(jk,:,:)%fsi,MASK=(tpdom(:,:)%tmk==1)) 
            imax(:) = MAXLOC(tpsit(jk,:,:)%fsi,MASK=(tpdom(:,:)%tmk==1)) 
!        
            WRITE(noutlu,1400) jk,'fsi :',zmin,imin(1),imin(2),  &
              zmax,imax(1),imax(2),zfav
!        
! * Sea ice thickness.
!
            zfav = SUM(tpsit(jk,:,:)%hsi,MASK=(tpdom(:,:)%tmk==1)) /  &
              FLOAT(nbpt)
            zmin = MINVAL(tpsit(jk,:,:)%hsi,MASK=(tpdom(:,:)%tmk==1))
            zmax = MAXVAL(tpsit(jk,:,:)%hsi,MASK=(tpdom(:,:)%tmk==1))
            imin(:) = MINLOC(tpsit(jk,:,:)%hsi,MASK=(tpdom(:,:)%tmk==1)) 
            imax(:) = MAXLOC(tpsit(jk,:,:)%hsi,MASK=(tpdom(:,:)%tmk==1)) 
!        
            WRITE(noutlu,1500) jk,'hsi :',zmin,imin(1),imin(2),  &
              zmax,imax(1),imax(2),zfav
!        
! * Snow thickness.
!
            zfav = SUM(tpsit(jk,:,:)%hsn,MASK=(tpdom(:,:)%tmk==1)) /  &
              FLOAT(nbpt)
            zmin = MINVAL(tpsit(jk,:,:)%hsn,MASK=(tpdom(:,:)%tmk==1))
            zmax = MAXVAL(tpsit(jk,:,:)%hsn,MASK=(tpdom(:,:)%tmk==1))
            imin(:) = MINLOC(tpsit(jk,:,:)%hsn,MASK=(tpdom(:,:)%tmk==1)) 
            imax(:) = MAXLOC(tpsit(jk,:,:)%hsn,MASK=(tpdom(:,:)%tmk==1)) 
!        
            WRITE(noutlu,1500) jk,'hsn :',zmin,imin(1),imin(2),  &
              zmax,imax(1),imax(2),zfav
!        
! * Snow density.
!
            zfav = SUM(tpsit(jk,:,:)%rsn,MASK=(tpdom(:,:)%tmk==1)) /  &
              FLOAT(nbpt)
            zmin = MINVAL(tpsit(jk,:,:)%rsn,MASK=(tpdom(:,:)%tmk==1))
            zmax = MAXVAL(tpsit(jk,:,:)%rsn,MASK=(tpdom(:,:)%tmk==1))
            imin(:) = MINLOC(tpsit(jk,:,:)%rsn,MASK=(tpdom(:,:)%tmk==1)) 
            imax(:) = MAXLOC(tpsit(jk,:,:)%rsn,MASK=(tpdom(:,:)%tmk==1)) 
!        
            WRITE(noutlu,1500) jk,'rsn :',zmin,imin(1),imin(2),  &
              zmax,imax(1),imax(2),zfav
!        
! * Surface temperature.
!
            zfav = SUM(tpsit(jk,:,:)%tsf,MASK=(tpdom(:,:)%tmk==1)) /  &
              FLOAT(nbpt)
            zmin = MINVAL(tpsit(jk,:,:)%tsf,MASK=(tpdom(:,:)%tmk==1))
            zmax = MAXVAL(tpsit(jk,:,:)%tsf,MASK=(tpdom(:,:)%tmk==1))
            imin(:) = MINLOC(tpsit(jk,:,:)%tsf,MASK=(tpdom(:,:)%tmk==1)) 
            imax(:) = MAXLOC(tpsit(jk,:,:)%tsf,MASK=(tpdom(:,:)%tmk==1)) 
!        
            WRITE(noutlu,1800) jk,'tsf :',zmin,imin(1),imin(2),  &
              zmax,imax(1),imax(2),zfav
!        
! * Ice salinity (psu).
!
            IF ( nicesal==1 ) THEN
              zfav = SUM(tpsit(jk,:,:)%ssi,MASK=(tpdom(:,:)%tmk==1)) /  &
                FLOAT(nbpt)
              zmin = MINVAL(tpsit(jk,:,:)%ssi,MASK=(tpdom(:,:)%tmk==1))
              zmax = MAXVAL(tpsit(jk,:,:)%ssi,MASK=(tpdom(:,:)%tmk==1))
              imin(:) = MINLOC(tpsit(jk,:,:)%ssi,MASK=(tpdom(:,:)%tmk==1)) 
              imax(:) = MAXLOC(tpsit(jk,:,:)%ssi,MASK=(tpdom(:,:)%tmk==1)) 
!        
              WRITE(noutlu,1500) jk,'ssi :',zmin,imin(1),imin(2),  &
                zmax,imax(1),imax(2),zfav
            ENDIF
!
! * Ice age (in days).
!
            IF ( niceage==1 ) THEN
              zfav = SUM(tpsit(jk,:,:)%age,MASK=(tpdom(:,:)%tmk==1)) /  &
                xday2sec / FLOAT(nbpt)
              zmin = MINVAL(tpsit(jk,:,:)%age,MASK=(tpdom(:,:)%tmk==1)) /  &
                xday2sec
              zmax = MAXVAL(tpsit(jk,:,:)%age,MASK=(tpdom(:,:)%tmk==1)) /  &
                xday2sec
              imin(:) = MINLOC(tpsit(jk,:,:)%age,MASK=(tpdom(:,:)%tmk==1)) 
              imax(:) = MAXLOC(tpsit(jk,:,:)%age,MASK=(tpdom(:,:)%tmk==1)) 
  !        
              WRITE(noutlu,1600) jk,'age :',zmin,imin(1),imin(2),  &
                zmax,imax(1),imax(2),zfav
            ENDIF
!
! * Melt pond volume (m).
!
            IF ( nmponds==1 ) THEN
              zfav = SUM(tpsit(jk,:,:)%vmp,MASK=(tpdom(:,:)%tmk==1)) /  &
                FLOAT(nbpt)
              zmin = MINVAL(tpsit(jk,:,:)%vmp,MASK=(tpdom(:,:)%tmk==1)) 
              zmax = MAXVAL(tpsit(jk,:,:)%vmp,MASK=(tpdom(:,:)%tmk==1))
              imin(:) = MINLOC(tpsit(jk,:,:)%vmp,MASK=(tpdom(:,:)%tmk==1)) 
              imax(:) = MAXLOC(tpsit(jk,:,:)%vmp,MASK=(tpdom(:,:)%tmk==1)) 
  !        
              WRITE(noutlu,1600) jk,'vmp :',zmin,imin(1),imin(2),  &
                zmax,imax(1),imax(2),zfav
            ENDIF 
            WRITE(noutlu,*) ' '
          END DO
      ENDIF
  ENDIF
!
! *** Performs statistics on sea ice 4D variables.
!
  IF (PRESENT(tpsil)) THEN
!
      IF (lp3) THEN
          DO jk = 1,nt
            DO jl=1,nl
!
! * Vertical gltools_enthalpy profile
!
              zfav = SUM(tpsil(jl,jk,:,:)%ent,MASK=(tpdom(:,:)%tmk==1)) /  &
                FLOAT(nbpt)
              zmin = MINVAL(tpsil(jl,jk,:,:)%ent,MASK=(tpdom(:,:)%tmk==1))
              zmax = MAXVAL(tpsil(jl,jk,:,:)%ent,MASK=(tpdom(:,:)%tmk==1))
              imin(:) = MINLOC(tpsil(jl,jk,:,:)%ent,MASK=(tpdom(:,:)%tmk==1)) 
              imax(:) = MAXLOC(tpsil(jl,jk,:,:)%ent,MASK=(tpdom(:,:)%tmk==1)) 
  !        
              WRITE(noutlu,1700) jk,jl,'ent :',zmin,imin(1),imin(2),  &
                zmax,imax(1),imax(2),zfav
            END DO
            WRITE(noutlu,*) ' '
          END DO
      ENDIF
  ENDIF
!
! *** Formats
!
1300 FORMAT( A6,  &
  ' min=',F7.4,1X,'at (',I3,',',I3,') max=',  &
  F7.4,1X,'at (',I3,',',I3,')' )
1400 FORMAT( 'th',I2.2,1X,A5,  &
  ' min=',F7.4,1X,'at (',I3,',',I3,') max=',  &
  F7.4,1X,'at (',I3,',',I3,') ave=',F7.4 )
1500 FORMAT( 'th',I2.2,1X,A5,  &
  ' min=',F8.4,1X,'at (',I3,',',I3,') max=',  &
  F8.4,1X,'at (',I3,',',I3,') ave=',F8.4 )
1600 FORMAT( 'th',I2.2,1X,A5,  &
  ' min=',F8.2,1X,'at (',I3,',',I3,') max=',  &
  F8.2,1X,'at (',I3,',',I3,') ave=',F8.2 )
1700 FORMAT( 'th',I2.2,1X,'lev',I2.2,1X,A5,  &
  ' min=',E12.5,1X,'at (',I3,',',I3,') max=',  &
  E12.5,1X,'at (',I3,',',I3,') ave=',E12.5 )
1800 FORMAT( 'th',I2.2,1X,A5,  &
  ' min=',F8.3,1X,'at (',I3,',',I3,') max=',  &
  F8.3,1X,'at (',I3,',',I3,') ave=',F8.3 )
!
! *** End of routine
!
END SUBROUTINE glt_info_si 
!
! ------------------------ END SUBROUTINE glt_info_si -----------------------
! -----------------------------------------------------------------------
