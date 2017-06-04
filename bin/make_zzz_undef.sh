#MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
#MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
#MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
#MNH_LIC for details. version 1.
#
#   Creation d'un fichier contenu les déclarations
#   de subroutine non défini
#
#   Creer d'abord le fichier "error" par
#
#   make MESONH > error 2>&1
if [ ! -f 'error' ]
then
cat << 'EOF'
ATTENTION LE FICHIER "error" n'existe pas
Creer d abord le fichier "error" par
make MESONH > error 2>&1 
EOF
exit 1
fi
BIDON=zzz_undef.f
rm -f  $BIDON
for routine in $(grep "undefined reference" error  | awk '{print $NF}' | tr -d "\`'" | sort -u )
do
ROUTINE=`echo $routine | sed -e 's/_$//g'  | sed -e 's/_$//g '`
echo ROUTINE=$ROUTINE
cat <<EOF >> $BIDON
        subroutine $ROUTINE
        print *, "ROUTINE NON DEFINI = ###$ROUTINE####"
        call abort
        end subroutine $ROUTINE
EOF
done
