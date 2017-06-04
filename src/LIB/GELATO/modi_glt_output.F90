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
! ======================= MODULE modi_glt_output ========================
! =======================================================================
!
! 
! * This module contains subroutines that manage icestate glt_output trace, 
! i.e.
!       - opening
!       - closing
! 

   
! ---------------------- BEGIN MODULE modi_glt_output -----------------------

!THXS_SFX!MODULE modi_glt_output
!THXS_SFX!INTERFACE
!THXS_SFX!
!THXS_SFX!SUBROUTINE opnout
!THXS_SFX!END SUBROUTINE opnout
!THXS_SFX!
!THXS_SFX!SUBROUTINE clsout
!THXS_SFX!END SUBROUTINE clsout
!THXS_SFX!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_output

! ----------------------- END MODULE modi_glt_output ------------------------


! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE OPNOUT ---------------------------
!
! - Opens GELATO glt_output file (this file is called 'gltout'), and prints
! a kind introductory welcome message.
! - Also opens specific files for gltools_timers glt_output
!
SUBROUTINE opnout
!
  USE modd_glt_param
!
  IMPLICIT NONE
!
  CHARACTER(20) :: &
       yfile
!
! 
!
! 1. Initialize Gelato glt_output file
! =====================================
!
  IF (lwg) THEN
    WRITE(*,*) 'Opening logical unit = ',noutlu,' for output'
    IF ( noutlu/=6 ) THEN
      WRITE( yfile,'(A,I3.3)') 'gltout_',gelato_myrank
      OPEN(UNIT=noutlu,FILE=yfile,FORM='FORMATTED')
    ENDIF
!
    WRITE(noutlu,*)  &
      '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    WRITE(noutlu,*)  &
      '          MERCI D''UTILISER LE MODELE DE GLACE DE MER GELATO !'
    WRITE(noutlu,*)  &
      '       ! GRACIAS POR UTILIZAR EL MODELO DE HIELO DE MAR GELATO !'
    WRITE(noutlu,*)  &
      '             THANK YOU FOR USING GELATO SEA ICE MODEL !'
    WRITE(noutlu,*)  &
      '            TAKK FOR AT DU BRUKER GELATO SJ0 IS MODELL !'
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '                            . ,             '
    WRITE(noutlu,*) '                             *    ,         '
    WRITE(noutlu,*) '                        ` *~.|,~* ''         ' 
    WRITE(noutlu,*) '                        ''  ,~*~~* `     _   '
    WRITE(noutlu,*) '                         ,* / \`* ''    //   '
    WRITE(noutlu,*) '                          ,* ; \,O.   //    '
    WRITE(noutlu,*) '                              ,(:::)=//     '
    WRITE(noutlu,*) '                             (  `~(###)     '
    WRITE(noutlu,*) '                              %---''`"y      '
    WRITE(noutlu,*) '                               \    /       ' 
    WRITE(noutlu,*) '                                \  /        '
    WRITE(noutlu,*) '                               __)(__  hjw  '
    WRITE(noutlu,*) '                              ''------`      '
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*)  &
      '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
!
!
!
! 2. Initialize timers output file
! =================================
!
    IF ( ntimlu/=6 ) THEN
      WRITE( yfile,'(A,I3.3)') 'timers_',gelato_myrank
      OPEN(UNIT=ntimlu,FILE=yfile,FORM='FORMATTED')
    ENDIF
!
  ENDIF
!
END SUBROUTINE opnout
!
! ----------------------- END SUBROUTINE OPNOUT -------------------------
! -----------------------------------------------------------------------


! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE CLSOUT --------------------------
!
! * Closes GELATO glt_output file, and gltools_timers file

SUBROUTINE clsout
!
  USE modd_glt_param
!
  IMPLICIT NONE
!
!
  IF (lwg) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) '                     * *******************'
    WRITE(noutlu,*) '                     * END OF GELATO RUN *'
    WRITE(noutlu,*) '                     * *******************'
    WRITE(noutlu,*) ' '
!
    IF ( noutlu/=6 ) CLOSE(noutlu)
    IF ( ntimlu/=6 ) CLOSE(ntimlu)
  ENDIF
!
END SUBROUTINE clsout
!
! ------------------------ END SUBROUTINE CLSOUT ------------------------
! -----------------------------------------------------------------------
