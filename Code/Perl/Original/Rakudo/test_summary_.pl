#! perl

# Copyright (C) 2004-2010, The Perl Foundation.
# $Id$

##  The "make spectest" target tells us how many tests we failed
##  (hopefully zero!), but doesn't say how many were actually passed.
##  This script runs the spectest tests and summarizes
##  passed, failed, todoed, skipped, executed and planned test results.
##
##  Usage:
##     tools/test_summary.pl [testlist]
##
##  If supplied, C<testlist> identifies an alternate list of tests
##  to use (e.g., t/localtest.data).

use strict;
use warnings;
use Time::Local;
use Time::HiRes;

my $benchmark;
# Comment out the next line to skip benchmarking; see docs below
$benchmark = Simple::Relative::Benchmarking::begin();    # defined below

# Build the list of test scripts to run in @tfiles
my $testlist = $ARGV[0] || 't/spectest.data';
my $fh;
open($fh, '<', $testlist) || die "Can't read $testlist: $!";
my (@tfiles, %tname); # @tfiles lists all test file names before fudging
while (<$fh>) {
    /^ *#/ && next;
    my ($specfile) = split ' ', $_;
    next unless $specfile;
    push @tfiles, "t/spec/$specfile";
}
close $fh or die $!;

# Fudge any Rakudo specific tests by running the fudgeall script
{
    my $cmd = join ' ', $^X, 't/spec/fudgeall', 'rakudo', @tfiles;
    # Fudgeall prints the name of each test script, but changes the name
    # ending to .rakudo instead of .t if tests were fudged.
    print "$cmd\n";
    @tfiles = split ' ', `$cmd`; # execute fudgeall, collect test names
}

# Put test names in %tname, with the 't/spec/' removed from the start
# and truncated to 49 characters. Keep track of the maximum name length.
@tfiles = sort @tfiles;
my $max = 0;
for my $tfile (@tfiles) {
    my $tname = $tfile;
    $tname =~ s{^t/spec/}{};
    $tname = substr($tname, 0, 49);
    if (length($tname) > $max) {
        $max = length($tname);
    }
    $tname{$tfile} = $tname;
}

# Prepare arrays and hashes to gather and accumulate test statistics
my @col = qw(pass fail todo skip plan spec);
my @syn = qw(S02 S03 S04 S05 S06 S07 S09 S10 S11 S12 S13 S14 S16 S17 S19 S28 S29 S32 int);
my %syn; # number of test scripts per Synopsis
my %sum; # total pass/fail/todo/skip/test/plan per Synposis
my $syn;
for $syn (@syn) {
    $syn{$syn} = 0;
    for my $col (@col) {
        $sum{"$syn-$col"} = 0;
    }
}
$syn = ''; # to reliably trigger the display of column headings

# Execute all test scripts, aggregate the results, display the failures
$| = 1;
my ( @fail, @plan_hint );
my %plan_per_file;
for my $tfile (@tfiles) {
    my $th;
    open($th, '<', $tfile) || die "Can't read $tfile: $!\n";
    my ($pass,$fail,$todo,$skip,$plan,$abort,$bonus) = (0,0,0,0,0,0,0);
    my $no_plan = 0; # planless may be fine, but bad for statistics
    # http://www.shadowcat.co.uk/blog/matt-s-trout/a-cunning-no_plan/
    while (<$th>) {                # extract the number of tests planned
        if (/^\s*plan\D*(\d+)/) { $plan = $1; last; }
        elsif (/^\s*plan\s+\*;/) { $no_plan = 1; last; }
    }
    close $th or die $!;
    my $tname = $tname{$tfile};
    # Repeat the column headings at the start of each Synopsis
    if ( $syn ne substr($tname, 0, 3) ) {
        $syn  =  substr($tname, 0, 3);
        printf( "%s  pass fail todo skip plan\n", ' ' x $max );
        unless ( exists $syn{$syn} ) {
            push @fail, "note: test_summary.pl \@syn does not have $syn";
        }
    }
    $syn{$syn}++;
    printf "%s%s..", $tname, '.' x ($max - length($tname));
    my $cmd = "./perl6 $tfile";
    # Run the test, collecting all stdout in @results
    my @results = split "\n", qx{$cmd};
    my (%skip, %todopass, %todofail);
    my ($time1, $time2, $testnumber, $test_comment ) = ( 0, 0, 0, '' );
    my @times = (); my @comments = ();
    for (@results) {
        # Pass over the optional line containing "1..$planned"
        if    (/^1\.\.(\d+)/)      { $plan = $1 if $1 > 0; next; }
        # Handle lines containing timestamps
        if    (/^# t=(\d+\.\d+)/)  {
            # Calculate the per test execution time
            $time2 = $time1;
            $time1 = $1;
            my $microseconds = int( ($time1 - $time2) * 1_000_000 );
            if ( $testnumber > 0 ) {
                $times[$testnumber] = $microseconds;
                $comments[$testnumber] = $test_comment;
                $testnumber = 0;
            }
            next;
        }
        # Ignore lines not beginning with "ok $$test" or "not ok $test"
        next unless /^(not )?ok +(\d+)/;
        if    (/#\s*SKIP\s*(.*)/i) { $skip++; $skip{$1}++; }
        elsif (/#\s*TODO\s*(.*)/i) { $todo++;
            my $reason = $1;
            if (/^ok /) { $todopass{$reason}++ }
            else        { $todofail{$reason}++ }
        }
        elsif (/^not ok +(.*)/)    { $fail++; push @fail, "$tname $1"; }
        elsif (/^ok +(\d+) - (.*)$/) {
            $pass++; $testnumber = $1; $test_comment = $2;
        }
    }
    my $test = $pass + $fail + $todo + $skip;
    if ($plan > $test) {
        $abort = $plan - $test;
        $fail += $abort;
        push @fail, "$tname aborted $abort test(s)";
    }
    elsif ($plan < $test) {
        $bonus = $test - $plan;
        push @fail, "$tname passed $bonus unplanned test(s)";
    }
    if ($no_plan) {
        push @plan_hint, "'plan *;' could become 'plan $plan;' in $tname";
    }
    printf "%4d %4d %4d %4d %4d\n",
        $pass, $fail, $todo, $skip, $plan;
    $sum{'pass'} += $pass;  $sum{"$syn-pass"} += $pass;
    $sum{'fail'} += $fail;  $sum{"$syn-fail"} += $fail;
    $sum{'todo'} += $todo;  $sum{"$syn-todo"} += $todo;
    $sum{'skip'} += $skip;  $sum{"$syn-skip"} += $skip;
    $sum{'plan'} += $plan;  $sum{"$syn-plan"} += $plan;
    {
        my $f = $tfile;
        $f =~ s/\.rakudo$/.t/;
        $plan_per_file{$f} = $plan;
    }
    for (keys %skip) {
        printf "   %3d skipped: %s\n", $skip{$_}, $_;
    }
    for (keys %todofail) {
        printf "   %3d todo   : %s\n", $todofail{$_}, $_;
    }
    for (keys %todopass) {
        printf "   %3d todo PASSED: %s\n", $todopass{$_}, $_;
    }
    if ($abort) {
        printf "   %3d tests aborted (missing ok/not ok)\n", $abort;
    }
    if ($bonus) {
        printf "   %3d tests more than planned were run\n", $bonus;
    }
    defined $benchmark && $benchmark->log_script_times($tfile,\@times,\@comments);
} # for my $tfile (@tfiles)
defined $benchmark && $benchmark->end(); # finish simple relative benchmarking

# Calculate plan totals from test scripts grouped by Synopsis and overall.
# This ignores any test list and processes all unfudged files in t/spec/.
# Implementing 'no_plan' or 'plan *' in test scripts makes this total
# inaccurate.
for my $syn (sort keys %syn) {
    my $ackcmd = "ack ^plan t/spec/$syn* -wH"; # some systems use ack-grep
    my @results = `$ackcmd`;       # gets an array of all the plan lines
    my $spec = 0;
    for (@results) {
        my ($fn, undef, $rest) = split /:/, $_;
        if (exists $plan_per_file{$fn}) {
            $spec += $plan_per_file{$fn}
        } else {
            # unreliable because some tests use expressions
            $spec += $1 if $rest =~ /^\s*plan\s+(\d+)/;
        }
    }
    $sum{"$syn-spec"} = $spec;
    $sum{'spec'} += $spec;
}

if (@plan_hint) {
    print "----------------\n";
    foreach (@plan_hint) {
        print "    $_\n";
    }
}

# Show test totals grouped by Synopsys, followed by overall totals
print "----------------\n";
my $sumfmt = qq(%-11.11s %6s,%6s,%6s,%6s,%6s,%6s\n);
printf $sumfmt, qq{"Synopsis",}, map { qq{"$_"} } @col;
for my $syn (sort keys %syn) {
    printf $sumfmt, qq{"$syn",}, map { $sum{"$syn-$_"} } @col;
}
my $total = scalar(@tfiles).' regression files';
printf $sumfmt, qq{"total",}, map { $sum{$_} } @col;
print "----------------\n";

# Optionally show the statistics that can be manually appended to
# docs/spectest-progress.csv
if ($ENV{'REV'}) {
    my @gmt = gmtime;
    my $testdate = sprintf '"%4d-%02d-%02d %02d:%02d"', $gmt[5]+1900,
        $gmt[4]+1, $gmt[3], $gmt[2], $gmt[1];
    my $filecount = scalar(@tfiles);
    my $passpercent = 100 * $sum{'pass'} / $sum{'spec'};
    print join(',', $ENV{'REV'}, (map { $sum{$_} } @col),
        $filecount), "\n";
    printf "spectest-progress.csv update: " .
        "%d files, %d (%.1f%% of %d) pass, %d fail\n",
        $filecount, $sum{'pass'}, $passpercent, $sum{'spec'}, $sum{'fail'};
}

# List descriptions of the tests that failed
if (@fail) {
    print "Failure summary:\n";
    foreach (@fail) {
        print "$_\n";
    }
}
else {
    print "No failures!\n";
}

# End of main program

#-------------------- Simple Relative Benchmarking ---------------------

package Simple::Relative::Benchmarking;

sub begin {       # this constructor starts simple relative benchmarking
    my $timings = shift || 5;    # number of timings to keep (default 5)
    my $self = {};
    my @test_history;
    $self->{'Timings'} = $timings;
    $self->{'Last_test_loaded'} = '';

    if ( open( $self->{'file_in'}, '<', 'docs/test_summary.times') ) {
        my $file_in = $self->{'file_in'};
        my $line = <$file_in>; chomp $line;
        if ( $line =~ m/{"test_.+":\[/i ) { # should be Test_history
            $line = <$file_in>; chomp $line;
            while ( $line =~ m/\s\s(.+\d\d\d\d-\d\d-\d\d.\d\d:\d\d:\d\d.+)/ ) {
                my $history_line = $1;
                $history_line =~ s/,$//; # trim possible trailing comma
                push @test_history, $history_line;
                $line = <$file_in>; chomp $line;
            } # ends on the ' ],' line after the test_history
            $line = <$file_in>; chomp $line;
            # if ( $line =~ m/ "test_microseconds":{/i ) {
            #     warn "begin reached 'test_microseconds'\n";
            # }
        }
    }
    open( $self->{'file_out'}, '>', 'docs/test_summary.times.tmp') or die "cannot create docs/test_summary.times.tmp: $!";
    my $parrot_version = qx{./perl6 -e'print \$*VM<config><revision>'};
    my $rakudo_version = qx{git log --oneline --max-count=1 .}; chomp $rakudo_version;
    $rakudo_version =~ s/\\/\\\\/g; # escape all backslashes
    $rakudo_version =~ s/\"/\\\"/g; # escape all double quotes
    my $file_out = $self->{'file_out'};
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
        gmtime(time());
    push @test_history, sprintf("[\"%4d-%02d-%02d %02d:%02d:%02d\",%d,\"%s\"]",
        $year+1900, $mon+1, $mday, $hour, $min, $sec,
        $parrot_version, $rakudo_version );
    # Delete the oldest test test_history if there are too many.
    while ( @test_history > $self->{'Timings'} ) { shift @test_history; }
    print $file_out qq!{"test_history":[\n!;
    print $file_out "  " . join(",\n  ",@test_history) . "\n ],\n";
    print $file_out qq! "test_microseconds":{!;
    # tell Test.pm to output per-test timestamps
    $ENV{'PERL6_TEST_TIMES'} = 'true';
    return bless $self;
}

# track simple relative benchmarking
sub log_script_times {
    my $self      = shift;
    my $test_name = shift;
    my $ref_times = shift;
    my $ref_comments = shift;
    my (@times) = @$ref_times;
    my (@comments) = @$ref_comments;
    shift @times;     # offset by 1: the first result becomes $times[0];
    shift @comments;
    for ( my $i=0; $i<=@times; $i++ ) {
        if ( not defined $comments[$i] ) { $comments[$i] = ''; }
        $comments[$i] =~ s/\\/\\\\/g; # escape all backslashes
        $comments[$i] =~ s/\"/\\\"/g; # escape all double quotes
    }
    my ( $line );
    my $file_in  = $self->{'file_in'};
    my $file_out = $self->{'file_out'};
    $test_name =~ s{^t/spec/}{};   # eg 'S02-literals/numeric.t'
    my $test_separator;
    if ( $self->{'Last_test_loaded'} eq '' ) {
        $test_separator = "\n";
    }
    else {
        $test_separator = ",\n";
    }
    while ( not eof($file_in) and $self->{'Last_test_loaded'} lt $test_name ) {
        $line = <$file_in>; chomp $line;
        if ( $line =~ m/^\s\s"(.+)":.$/ ) {
            $self->{'Last_test_loaded'} = $1;
        }
    }
    my @logged_results;
    if ( not eof($file_in) and $self->{'Last_test_loaded'} eq $test_name ) {
        my $line = <$file_in>; chomp $line;
        while ( not eof($file_in) and $line =~ m/^\s\s\s\[(\d+),\[(.+?)\],?/ ) {
            my $test_number = $1;
            my @timings = split /,/ , $2;
            $logged_results[$test_number-1] = [ @timings ];
            $line = <$file_in>; chomp $line;
        }
    }
    my $microseconds = [];
    my $testcount = @times;
    for ( my $test_number=0; $test_number<$testcount; $test_number++) {
        unless ( defined($times[$test_number]) ) { $times[$test_number] = 0; }
        my ( @times_in_file );
        if ( defined @{$logged_results[$test_number]} ) {
            @times_in_file = ( @{$logged_results[$test_number]} );
        }
        push @times_in_file, $times[$test_number];
        if ( not defined( $times_in_file[0] ) ) { shift @times_in_file; }
        # Delete the oldest test timings if there are too many.
        while ( @times_in_file > $self->{'Timings'} ) { shift @times_in_file; }
        $$microseconds[$test_number] = [ @times_in_file ];
    }
    my $test_number = 1; # start from number 1 again
    print $file_out
        $test_separator .
        qq'  "$test_name":[\n' .
        join(",\n", map {'   ['.$test_number++.',['.join(',',@$_).'],"'.$comments[$test_number-2].'"]'} @$microseconds) .
        qq'\n  ]';
}

sub end {
    my $self = shift;
    my $file_in  = $self->{'file_in'};
    my $file_out = $self->{'file_out'};
    print $file_out "\n }\n}\n";
    close $file_out or warn $!;
    close $file_in or warn $!;
    unlink 'docs/test_summary.times';
    rename 'docs/test_summary.times.tmp', 'docs/test_summary.times';
}

package main;
