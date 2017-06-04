#!/usr/bin/env perl
#SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
#SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
#SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
#SFX_LIC for details. version 1.
use strict;
use warnings;
use Fortran90_stuff_interface;
use Data::Dumper;

$Data::Dumper::Indent = 1;
{
  my (@files);
  (@files) = @ARGV;
  &setup_parse();
  my $locintfbldir=$ENV{LOC_INTFBDIR} or die "LOC_INTFBDIR not defined ";
  my $intfbldir=$ENV{INTFBDIR} or die "INTFBDIR not defined ";
  our $study_called;
  for (@files) {
    my (@interface_block);
    my (@line_hash);
    chomp;
 # Read in lines from file
    my $fname = $_;
    print "Working on file $fname \n";
    my @lines = &readfile($fname);
    my @statements=();
    my %prog_info=();
    &expcont(\@lines,\@statements);
    $study_called=0;
    &study(\@statements,\%prog_info);
    print Dumper(\%prog_info);
    unless($prog_info{is_module}) {
      $fname=~s#.*/(.+)$#$1#;
      &create_interface_block(\@statements,\@interface_block,$fname);
      &cont_lines(\@interface_block,\@lines,\@line_hash);
      my $int_block_fname=$fname;
      $int_block_fname=~s/\.F90/.intfb.h/;
      #$int_block_fname=~s#.*/(.+)$#$1#;
      my $ofname=$intfbldir.'/'.$int_block_fname;
      my $remake=1;
      if ( -f $ofname ) {
	my @oldlines=&readfile($ofname);
	$remake=0 if(&eq_array(\@oldlines, \@lines));
	print "INTERFACE BLOCK $int_block_fname UNCHANGED \n" unless ($remake);
      }
      if($remake) {
	print "WRITE INTERFACE BLOCK $int_block_fname \n";
	$int_block_fname=$locintfbldir.'/modi_'.$int_block_fname;
	#$int_block_fname=$locintfbldir.'/_'.$int_block_fname;
	print "$int_block_fname \n";
	&writefile($int_block_fname,\@lines);
      }
    }
  }
}
sub eq_array {
    my ($ra, $rb) = @_;
    return 0 unless $#$ra == $#$rb;
    for my $i (0..$#$ra) {
	return 0 unless $ra->[$i] eq $rb->[$i];
    }
    return 1;
}

