#!/usr/bin/env perl
#SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
#SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
#SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
#SFX_LIC for details. version 1.
##************************************************************************
## File     : splr.pl
## Author   : zRJ rimvydasjas(at)gmail(dot)com, HIRLAM(LHMS),
##            part of HARMONIE project custom building toolchain
## Date     : somewhere around 2k13
##
## Purpose  : fortran source preparation:
##              module dependencies/interfaces
##              source spliting to fortran UNITS
##              optional FPP stage
##
## About    : tool ported to SURFEX project framework
##            to mimic functionality of spl/spll into single perl script
##            without external deps (cpp,gawk,sed,etc) ant temp files
##
## Remark   : *should* be quite safe, is quite fast, fork/edit freely
##            if you like it and have suggestions/bugs/new features
##            feel free writing an informal email to me
##
## Input    : file list
##            recommend smth like:
##            find dir -name "*.F90" -type f |xargs perl splr.pl [FLAGS]
##
## Version  : pre v0 alpha build 3
##
## Bugs     : fortran does not have bugs unlike some other ++ languages;)
##
## TODO     : first to reduce STUDY dependencies(speed)
##
## Warning  : for internal use only,
##            PROVIDED AS IS. I take no responsibility whatsoever!
##
##************************************************************************

use File::Basename qw(dirname basename);

use lib dirname($0);
use Fortran90_stuff;

use strict;
use warnings;
use Data::Dumper;

&splr_setup();

#warn "ARGV=\n".Dumper(@ARGV);

{
  our ${splr_moddir};
  our ${splr_outdir};
  our ${splr_dofpp};
  our ${splr_domodi};
  our ${splr_dodeps};
  our ${splr_dosplit};
  our ${splr_dowrap};

  my (@files);
  (@files) = @ARGV;

#RJ: only needed for modi generation
  if(${splr_domodi}) {
    &setup_parse();
    our ${f90s_FORGIVE_ME}=1;
  }

  for my $fname (@files) {
    next unless ($fname=~/\.(?:F90|F95|F03|F08|f90|f95|f03|f08)$/);
    unless ( -f $fname){
        warn "Warning[splr]: file $fname does not exists\n";
        next;
    }
##    print "Working on file $fname \n";

    my $DIRNAME="";
    my $BASENAME="";
    my $BASENAME2="";
    my $SUFFIX2=".F90";

    if(${splr_outdir} eq "") {
      $DIRNAME=dirname($fname);
    }
    else {
      $DIRNAME=${splr_outdir};
    }
    $BASENAME=basename($fname);
    $BASENAME2=$BASENAME;
    $BASENAME2=~ s/(\.[^\.]+)$//;
    if(defined($1)) {
      $SUFFIX2=$1;
    }

    my $slurp_org;
    &slurpfile(\$fname,\$slurp_org);

#RJ: pre FPP stage, REVERTABLE
    if(${splr_dofpp}) {
      &splr_fpp(\$slurp_org);
    }

    my $safe_modi=1;
    my $safe_split=1;
    my $safe_wrap=1;
#RJ: if file unit contains MODULE MODI_XXX or MODULE MODE_XXX do not attempt to wrap into modi
#RJ  usually they have hardcoded INTERFACE statements inlined
    if($slurp_org=~/(?:^|[\n])[ ]*+MODULE[ \n]++MOD[IE]_([\w]++)/i) {
      ${safe_wrap}=0;
    }
#RJ: if current file has something like ^MODULE MODI_... don't bother, mainly surfex->av_pgd
 #   if($slurp_org=~/(?:^|[\n])[ ]*+MODULE[ \n]++MODI_([\w]++)/i) {
 #     warn "Warning[splr]: module modi_$1 detected in ".$BASENAME.", not making modis\n";
 #     ${safe_modi}=0;
 #   }

#RJ: if file has cpp prototyping better to avoid (mainly xrd/{lfi,fa})
    if($slurp_org=~/(?:^(?:[\n\t ]++|[\!][^\n]*+)*+[\#])/ii) {
      warn "Warning[splr]: cpp prototyping detected in ".$BASENAME.", not splitting\n";
      ${safe_split}=0;
    }

#RJ: if path contains LIB/XRDnn/{FA,LFI} better try not to create modis, intel ifort compiler bug for 'FA=>FACOM_DEFAULT'
    ${safe_modi}=0 if($fname=~/\bLIB[\/]XRD40[\/](?:FA|LFI)[\/]/i);

#RJ-later
#RJ: something is wery wrong if read_surf.F90 is plitted and compiled as separate units...
#RJ: disabled for now not to mask out bug in SODA_OI2 test nc outputs
#if($fname=~/[\/]read_surf[\.][fF]90/){
#  ${splr_dosplit}=0 ;
#  warn "zRJ [WARNING] not splitting: $fname=";
#}
    my @splitted_units;
    if(${splr_dosplit} && ${safe_split}) {
      &slurp_split(\$slurp_org,\$fname,\@splitted_units);
    }
    else {
      push(@splitted_units,$slurp_org);
    }

    my $slurp_deps="";
    if(${splr_dodeps}) {
      $slurp_deps="#===========  ".$BASENAME." dependence ==========================\n";
    }

    foreach my $ii (0..$#splitted_units) {
      my $try_wrap=0;

#RJ: ensure that there is a newline at the end, just in case of uses other than mine
      $splitted_units[$ii].="\n" unless $splitted_units[$ii]=~/[\n]$/;

      my $cur_name="";
      my $cur_uname="";
      if($splitted_units[$ii]=~/(?:^|[\n])[ \t]*+[^\!\n]*\b(subroutine|function|module|program)\b[ \t]+([\w]++)\b/i) {
        my $utype=$1;
        my $uname=$2;

        if(${splr_dowrap} && ${safe_wrap} && ${safe_modi} && ${safe_split} && $utype=~/^(?:subroutine|function)\b$/i) {
          $try_wrap=1;
#RJ: avoid wrapping goto_smth units, recursive dependencies in (mode_modeln_teb_handler & mode_modeln_surfex_handler)
          ${try_wrap}=0 if($uname=~/^goto_/i);
##RJ: do not wrap if supplied file path contains LIB directory (I'm lazy...)
#          ${try_wrap}=0 if($fname=~/\bLIB[\/]/);
#RJ: for simplicity try wrapping only if supplied file path has some specific directory (Yeah, I'm lazy...)
#          ${try_wrap}=0 unless($fname=~/\b(?:ASSIM|FORC|OFFLIN|SURFEX|TOPD|TRIP)[\/]/);
          ${try_wrap}=0 unless($fname=~/\b(?:ASSIM|FORC|OFFLIN|SURFEX)[\/]/);
        }

        if(${try_wrap}) {
          $cur_name="spll_modi_".lc($uname).$SUFFIX2;
          $cur_uname=lc($uname);
          my $theader="MODULE MODI_".uc($uname)."\nIMPLICIT NONE\nCONTAINS\n";
          my $tfooter="\nEND MODULE MODI_".uc($uname)."\n";
          $splitted_units[$ii]=$theader.$splitted_units[$ii].$tfooter;
        }
        else {
          $cur_name="spll_".lc($uname).$SUFFIX2;
          $cur_uname=lc($uname);
        }
      }
      else {
        $cur_name=$BASENAME2.$SUFFIX2;
        $cur_uname=$BASENAME2;
#        if($ii > 0) {
          $cur_name="spll_".$BASENAME2."__SPLIT$ii".$SUFFIX2;
          $cur_uname=$BASENAME2."__SPLIT$ii";
#        }
      }

      if(${splr_dodeps}) {
        &splr_dep(\$cur_name,\$splitted_units[$ii],\$slurp_deps);
      }

      my $fnoutf=$DIRNAME."/".$cur_name;

      my $remake=1;
      if (-f $fnoutf) {
        my $slurpold="";
        &slurpfile(\$fnoutf,\$slurpold);
        $remake=0 if ($slurpold eq $splitted_units[$ii]);
      }
      if ($remake) {
        &slurp2file(\$fnoutf,\$splitted_units[$ii]);
      }
      else {
        print "SPILLED $fnoutf UNCHANGED \n"
      }

      if(${splr_domodi} && ${safe_modi} && ${try_wrap}==0) {
        my $MODDIR="";
        if(${splr_moddir} ne "" ) {
          $MODDIR=${splr_moddir};
        }
        else {
          $MODDIR=$DIRNAME;
        }
        $ENV{LOC_INTFBDIR} = $MODDIR;
        $ENV{INTFBDIR} = $MODDIR;
        &splr_modi(\$cur_name,\$splitted_units[$ii],\$slurp_deps);
      }
    }

    if(${splr_dodeps}) {
      my $fndeps=$DIRNAME."/".$BASENAME2.".D";
      &slurp2file(\$fndeps,\$slurp_deps);
#RJ: touch the dep file just in case
      utime(undef,undef,$fndeps);
    }
  }
}

sub splr_help {
  warn "Usage: perl splr.pl [OPTION...] FILE...\n";
  warn "\n";
  warn "  --deps            write filename.D dependencies for makefiles\n";
  warn "                      (simplified spll style)\n";
  warn "  --modi            create modi interfaces where possible\n";
  warn "                      (Fortran90_stuff.pm implementation)\n";
  warn "  --split           try presplit sources to separate units first\n";
  warn "  --wrap            wrap functions/subroutines to modis for enforcing\n";
  warn "\n";
  warn "  --mdir=dir        output directory for generated modi interfaces\n";
  warn "  --odir=dir        output directory for all generated files\n";
  warn "\n";
  warn "  --fpp             run simple fortran preprocessor\n";
  warn "          -D name   activate define name\n";
  warn "          -U name   deactivate define name\n";
  warn "          -I dir    consider extra dir in \$VPATH for includes\n";
  warn "\n";
  warn "  -?, -h, --help    give this help list\n";
  exit 1;
}

sub splr_setup {

  use Getopt::Long;

  our $splr_dohelp=0;
  our $splr_dofpp=0;
  our $splr_dodeps=0;
  our $splr_domodi=0;
  our $splr_dosplit=0;
  our $splr_dowrap=0;
  our $splr_moddir="";
  our $splr_outdir="";

  my @splr_incs=();
  my @splr_defs=();
  my @splr_undefs=();
  our $splr_dodef="";
  our $splr_doundef="";

  Getopt::Long::Configure('no_ignore_case','bundling_override','pass_through');
  GetOptions( "D=s" => \@{splr_defs},
              "U=s" => \@{splr_undefs},
              "I=s" => \@{splr_incs},
              "mdir=s" => \${splr_moddir},
              "odir=s" => \${splr_outdir},
              "fpp" => \${splr_dofpp},
              "deps" => \${splr_dodeps},
              "modi" => \${splr_domodi},
              "wrap" => \${splr_dowrap},
              "split" => \${splr_dosplit},
              "help|h|?" => \${splr_dohelp},
              ) or die("Bailout[splr]: failed to parse flags!");

  if(${splr_dohelp}) {
    &splr_help();
  }

  if(${splr_dofpp}) {
    if(@{splr_incs}) {
      $ENV{VPATH}=join(':',@{splr_incs});
    }
    if(@{splr_defs}) {
      ${splr_dodef}=join('|',@{splr_defs});
    }
    else {
      ${splr_dodef}="zRJ";
    }
    if(@{splr_undefs}) {
      ${splr_doundef}=join('|',@{splr_undefs});
#RJ: prefer defs over undefs
      ${splr_doundef}=~s/\b(?:${splr_dodef})\b(?:[|])?+//g;
    }
    else {
      ${splr_doundef}="Zrj";
    }
  }
}

sub splr_fpp {

  use Fortran90_stuff qw(slurp_inc slurp_fpp);

  my($slurp)=@_;
  our ${splr_dodef};
  our ${splr_doundef};

#RJ: first inline, then apply defs
  if($ENV{VPATH}) {
    &slurp_inc($slurp);
  }

#RJ: apply masking defs, defs are prefered by undefs in setup
  &slurp_fpp($slurp,\${splr_dodef},\${splr_doundef});

}

sub splr_dep {

  my($fname,$slurp,$slurp_deps) = @_;

  my $EXTERNAL="ISO_C_BINDING|ISO_FORTRAN_ENV|OMP_LIB|OMP_LIB_KINDS";
  $EXTERNAL.= "|IEEE_EXCEPTIONS|IEEE_ARITHMETIC|IEEE_FEATURES";
  $EXTERNAL.= "|IFCORE|NETCDF|GRIB_API|HDF|HDF5|MPI|MPI_F08";

  my $spllfile=$$fname;
  my $spllobj=$$fname;
  $spllobj=~s/\.[^\.]++$/.o/;

  $$slurp_deps.="#---------------------- splitted ".$spllfile." dependence -----------\n";

  my $mod_deps='';
  my $modules='';
#RJ: currently unused, but anyway
  my $inc_deps='';

  {
    local $/;
    my $dump="\n".$$slurp."\n";

    $dump=~s/[\n][ \t]*+module[ \:\t]++([\w]+)/$modules.=' '.$1;""/ieg;
    $dump=~s/[\n][ \t]*+use[ \:\t]++([\w]+)/$mod_deps.=' '.$1;""/ieg;
    $dump=~s/[\n][ \t\#]*+include[ ]*+[\"\'][ ]*+([^\s\'\"]++)[ ]*+[\"\']/$inc_deps.=' '.$1;""/ieg;

    $modules=~s/^[ ]++//;
    $mod_deps=~s/^[ ]++//;
    $inc_deps=~s/^[ ]++//;
    $modules=lc($modules);
    $mod_deps=lc($mod_deps);

    my %hm =();
    my @am =();
    @am = grep { ! $hm{$_}++ } split(' ',$mod_deps);
    $mod_deps=join(' ',@am);

    my %hi =();
    my @ai =();
    @ai = grep { ! $hi{$_}++ } split(' ',$inc_deps);
    $inc_deps=join(' ',@ai);
  }

  if($modules ne '') {
    $modules=~s/\b([\w]++)\b/$1.mod/ig;
    $$slurp_deps.=$modules." : ".$spllobj."\n";
  }

  if($mod_deps ne '') {
#RJ: filter out some external stuff, to avoid excessive VPATH abuse
    $mod_deps=~s/\b(?:$EXTERNAL)\b[ ]*+//ig;
    $mod_deps=~s/\b([\w]++)\b/$1.mod/ig;
    $$slurp_deps.=$spllobj." : ".$spllfile." ".$mod_deps."\n";
  }
 # warn "MOD_DEP=$mod_deps\n" unless ($mod_deps eq "");
 # warn "INC_DEP=$inc_deps\n" unless ($inc_deps eq "");
}

sub splr_modi {

  my($fname,$slurp,$slurp_deps) = @_;

  use Fortran90_stuff qw( slurpfile slurp_fpp slurp_split
                        slurp2array array2slurp slurp2file
                        setup_parse expcont study $study_called cont_lines
                        writefile readfile );

  our ${study_called};

  our ${splr_dodeps};

#RJ: if current file has something like ^MODULE MODI_... don't bother, mainly surfex->av_pgd
    if($$slurp=~/(?:^|[\n])[ ]*+MODULE[ \n]++MODI_([\w]++)/i) {
      warn "Warning[mkintfbl]: module modi_$1 detected in ".$$fname.", skipping\n";
      return;
    }

    my $locintfbldir=$ENV{LOC_INTFBDIR} or die "LOC_INTFBDIR not defined ";
    my $intfbldir=$ENV{INTFBDIR} or die "INTFBDIR not defined ";

    my @splitted_units;
    &slurp_split($slurp,$fname,\@splitted_units);

ttt:    foreach my $ii (0..$#splitted_units) {
      my $cur_fname=$$fname;
      if($ii>0) {
        $cur_fname=~s/(\.[fF][0-9][0-9])$/__SPLIT$ii$1/;
      }
      my (%prog_info);
      my @statements=();
      my @lines;
      &slurp2array(\$splitted_units[$ii],\@lines);

      &expcont(\@lines,\@statements);
      $study_called=0;
#RJ:some safety
      {
        local $@;
        eval{&study(\@statements,\%prog_info);};
        if($@) {
          warn "Warning[mkintfbl]: study crashed- ".$cur_fname."\n";
  die;
          next ttt;
        }
      }
###      print Dumper(\%prog_info);

      my (@interface_block,@line_hash,$ftemp);
      $ftemp=$$fname;
      $ftemp=~s/^.*[\/]//;
#RJ:some safety
      {
        local $@;
        eval{&hm_create_interface_module (\@statements,\@interface_block,\$ftemp);};
        if($@) {
          warn "Warning[mkintfbl]: create_intfb crashed- ".$cur_fname."\n";
  die;
          next;
        }
      }
      unless(@interface_block) {
        next;
      }
      &cont_lines (\@interface_block,\@lines,\@line_hash);

      my $int_block_fname = lc($ftemp);
#RJ       unless ($int_block_fname =~ /^[\w]/) {
      unless ($int_block_fname =~ /^[a-zA-Z]/) {
        warn "Warning[mkintfbl]: get unit name failed - ".$int_block_fname."\n";
        next;
      }
      my $int_fname="spll_modi_".$int_block_fname.".f90";
      $int_block_fname=$int_fname;
      my $ofname = "$intfbldir/$int_block_fname";
      my $remake = 1;
      my $slurpout="";
      &array2slurp(\@lines,\$slurpout);
      if (-f $ofname) {
            my $slurpold="";
            &slurpfile(\$ofname,\$slurpold);
            $remake=0 if ($slurpold eq $slurpout);
      }
      if ($remake) {
          print "WRITE INTERFACE BLOCK $int_block_fname \n";
            $int_block_fname = "$locintfbldir/$int_block_fname";
            print "$int_block_fname \n";
            &slurp2file(\$int_block_fname,\$slurpout);
      }
      else {
        print "INTERFACE BLOCK $int_block_fname UNCHANGED \n"
      }
      if(${splr_dodeps}) {
        &splr_dep(\$int_fname,\$slurpout,$slurp_deps);
      }
    }

}

sub hm_create_interface_module {

use Fortran90_stuff qw( create_intfb $f90s_ALLOW_DIRTY_MOD_REGEX
                        $f90s_INTFB_GENERIC $f90s_INTFB_INC_IMPNONE
                        $f90s_PREFER_DOUBLE_CONT );

# Create a "minimal" interface block for subroutines
  my($statements,$interface_blocks,$foldname) = @_;
  our(${f90s_PREFER_DOUBLE_CONT});
  our(${f90s_ALLOW_DIRTY_MOD_REGEX});
  our(${f90s_INTFB_GENERIC});
  our(${f90s_INTFB_INC_IMPNONE});
  @$interface_blocks=();

  my($href);
  my $parse=0;
  my @intz=();
  my @intfb=();
  my $cur_fname='';

  ${f90s_PREFER_DOUBLE_CONT}=0;

#RJ: choose interface simple vs generic
  ${f90s_INTFB_GENERIC}=0;
  ${f90s_INTFB_INC_IMPNONE}=1;

#RJ-later
#RJ: if file name contains lonlat_rot DO NOT attempt to do generic intefaces, must be fixed first
  my $TT=$$foldname;
##  warn $TT;
  if($TT=~/(?:lonlat_rot|gltools_newice_r|glt_constrain_r)/i ){
    ${f90s_INTFB_GENERIC}=0;
  warn "zRJ [Warning]: not making generic modi for $$foldname";
  }

#RJ: attempt to clean up interface dependencies, most can be avoided by code fixes,except third one
#RJ: if left empty - includes all without only
  if(${f90s_ALLOW_DIRTY_MOD_REGEX} eq '') {
    ${f90s_ALLOW_DIRTY_MOD_REGEX}= 'PARKIND1|LFI_PRECISION|YOMCMA';  #common
    ${f90s_ALLOW_DIRTY_MOD_REGEX}.='|SPECTRAL_COLUMNS_MIX|GRIDPOINT_FIELDS_MIX|SPECTRAL_FIELDS_MOD';
    ${f90s_ALLOW_DIRTY_MOD_REGEX}.='|CONTROL_VECTORS|RANDOM_NUMBERS_MIX|JB_CONTROL_VECTORS_MOD'; #as usual
    ${f90s_ALLOW_DIRTY_MOD_REGEX}.='|MODD_BUDGET|MODD_CH_INIT_JVALUES'; #from mpa
    ${f90s_ALLOW_DIRTY_MOD_REGEX}.='|MODD_FMDECLAR|MODD_TYPE_DATE_SURF|MODD_TYPE_SNOW|MODD_TYPE_EFUTIL'; #from surfex
    ${f90s_ALLOW_DIRTY_MOD_REGEX}.='|YOMCMEMTYPES|YOAMSU|MOD_KFGRID'; #from sat
    ${f90s_ALLOW_DIRTY_MOD_REGEX}.='|ODB_MODULE|ISO_C_BINDING|EXTR_MODULE_1C|RAD_BIAS_1C_UTI'; #from odb
    ${f90s_ALLOW_DIRTY_MOD_REGEX}.='|MODD_TYPE|MODD_GLT_PARAM'; #from gelato
  }

# Gather information needed to create interface block for routine
  foreach $href (@$statements) {
    next if($href->{content} eq 'comment');
    next if($href->{exec});
    next if($href->{in_contain});
    next if($href->{content} eq 'in_interface');

#RJ: ignore program and module, but if commented out, should extract externally visible ones if present
    if(($href->{content} eq 'PROGRAM') || ($href->{content} eq 'MODULE')) {
      warn "Warning[INTFB]: skipping ".$href->{statement};
      @$interface_blocks=();
      return;
    }
    if(($href->{content} eq 'SUBROUTINE') || ($href->{content} eq 'FUNCTION')) {
      if($parse != 0) {
        warn "Warning[INTFB]: unexpected ".$href->{statement};
        @$interface_blocks=();
        return;
      }
      if($href->{content} eq 'SUBROUTINE') {
        $parse=1;
      }
      elsif($href->{content} eq 'FUNCTION') {
        $parse=2;
      }
    }
    if($parse != 0) {
#RJ: remove existing pre/post inserts if any
      delete $href->{pre_insert} if(exists $href->{pre_insert});
      delete $href->{post_insert} if(exists $href->{post_insert});
      push(@intz,$href);
      if((($href->{content} eq 'ENDSUBROUTINE') && ($parse == 1)) || (($href->{content} eq 'ENDFUNCTION') && ($parse == 2))) {
        $parse=0;
        &create_intfb(\@intz,\@intfb,\$cur_fname);
        
#RJ: zero out for safety
        @intz=();

#RJ: modi interface overrides
        {
        my $myhref;
        for $myhref (@intfb) {
          if(($myhref->{content} eq 'SUBROUTINE') || ($myhref->{content} eq 'FUNCTION')) {
            my $fname2=$$foldname;
            $fname2=~s/\.f90/.D/;
#RJ: for reproducibility
#            $myhref->{pre_insert} = "!depfile:".$fname2."\nMODULE MODI_".uc($cur_fname)."\n".$myhref->{pre_insert};
#RJ: pre V8 builds use auto_modi tag?
#            $myhref->{pre_insert} = "!depfile:".$fname2."\nMODULE MODI_"."$cur_fname"."\n".$myhref->{pre_insert};
            $myhref->{pre_insert} = "!auto_modi:".$fname2."\nMODULE MODI_"."$cur_fname"."\n".$myhref->{pre_insert};
          }
          if(($myhref->{content} eq 'ENDSUBROUTINE') || ($myhref->{content} eq 'ENDFUNCTION')) {
            $myhref->{post_insert}.="END MODULE MODI_"."$cur_fname"."\n";
          }
        }
        }
#RJ: output interface
        push(@$interface_blocks,@intfb);
#RJ: consider only first interface block, comment out to include rest if present
        last;
      }
    }
  }
  if($parse != 0) {
    warn "Warning[INTFB]: failed to find end unit ".$cur_fname."\n";
    @$interface_blocks=();
    return;
  }
  $$foldname=$cur_fname;
}
