use strict;
use warnings;
use Time::Local;
use Time::HiRes;

my $benchmark;
$benchmark = Simple::Relative::Benchmarking::begin();

my $testlist = $ARGV[0] || 't/spectest.data';
my $fh;
open($fh, '<', $testlist) || die "Can't read $testlist: $!";
my (@tfiles, %tname);
while (<$fh>) {
    /^ *#/ && next;
    my ($specfile) = split ' ', $_;
    next unless $specfile;
    push @tfiles, "t/spec/$specfile";
}
close $fh or die $!;

{
    my $cmd = join ' ', $^X, 't/spec/fudgeall', 'rakudo', @tfiles;
    print "$cmd\n";
    @tfiles = split ' ', `$cmd`;
}

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

my @col = qw(pass fail todo skip plan spec);
my @syn = qw(S02 S03 S04 S05 S06 S07 S09 S10 S11 S12 S13 S14 S16 S17 S19 S28 S29 S32 int);
my %syn;
my %sum;
my $syn;
for $syn (@syn) {
    $syn{$syn} = 0;
    for my $col (@col) {
        $sum{"$syn-$col"} = 0;
    }
}
$syn = '';

$| = 1;
my ( @fail, @plan_hint );
my %plan_per_file;
for my $tfile (@tfiles) {
    my $th;
    open($th, '<', $tfile) || die "Can't read $tfile: $!\n";
    my ($pass,$fail,$todo,$skip,$plan,$abort,$bonus) = (0,0,0,0,0,0,0);
    my $no_plan = 0;
    while (<$th>) {
        if (/^\s*plan\D*(\d+)/) { $plan = $1; last; }
        elsif (/^\s*plan\s+\*;/) { $no_plan = 1; last; }
    }
    close $th or die $!;
    my $tname = $tname{$tfile};
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
    my @results = split "\n", qx{$cmd};
    my (%skip, %todopass, %todofail);
    my ($time1, $time2, $testnumber, $test_comment ) = ( 0, 0, 0, '' );
    my @times = (); my @comments = ();
    for (@results) {
        if    (/^1\.\.(\d+)/)      { $plan = $1 if $1 > 0; next; }
        if    (/^# t=(\d+\.\d+)/)  {
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
}
defined $benchmark && $benchmark->end();

for my $syn (sort keys %syn) {
    my $ackcmd = "ack ^plan t/spec/$syn* -wH";
    my @results = `$ackcmd`;
    my $spec = 0;
    for (@results) {
        my ($fn, undef, $rest) = split /:/, $_;
        if (exists $plan_per_file{$fn}) {
            $spec += $plan_per_file{$fn}
        } else {
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

print "----------------\n";
my $sumfmt = qq(%-11.11s %6s,%6s,%6s,%6s,%6s,%6s\n);
printf $sumfmt, qq{"Synopsis",}, map { qq{"$_"} } @col;
for my $syn (sort keys %syn) {
    printf $sumfmt, qq{"$syn",}, map { $sum{"$syn-$_"} } @col;
}
my $total = scalar(@tfiles).' regression files';
printf $sumfmt, qq{"total",}, map { $sum{$_} } @col;
print "----------------\n";

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

if (@fail) {
    print "Failure summary:\n";
    foreach (@fail) {
        print "$_\n";
    }
}
else {
    print "No failures!\n";
}

package Simple::Relative::Benchmarking;

sub begin {
    my $timings = shift || 5;
    my $self = {};
    my @test_history;
    $self->{'Timings'} = $timings;
    $self->{'Last_test_loaded'} = '';

    if ( open( $self->{'file_in'}, '<', 'docs/test_summary.times') ) {
        my $file_in = $self->{'file_in'};
        my $line = <$file_in>; chomp $line;
        if ( $line =~ m/{"test_.+":\[/i ) {
            $line = <$file_in>; chomp $line;
            while ( $line =~ m/\s\s(.+\d\d\d\d-\d\d-\d\d.\d\d:\d\d:\d\d.+)/ ) {
                my $history_line = $1;
                $history_line =~ s/,$//;
                push @test_history, $history_line;
                $line = <$file_in>; chomp $line;
            }
            $line = <$file_in>; chomp $line;
        }
    }
    open( $self->{'file_out'}, '>', 'docs/test_summary.times.tmp') or die "cannot create docs/test_summary.times.tmp: $!";
    my $parrot_version = qx{./perl6 -e'print \$*VM<config><revision>'};
    my $rakudo_version = qx{git log --oneline --max-count=1 .}; chomp $rakudo_version;
    $rakudo_version =~ s/\\/\\\\/g;
    $rakudo_version =~ s/\"/\\\"/g;
    my $file_out = $self->{'file_out'};
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
        gmtime(time());
    push @test_history, sprintf("[\"%4d-%02d-%02d %02d:%02d:%02d\",%d,\"%s\"]",
        $year+1900, $mon+1, $mday, $hour, $min, $sec,
        $parrot_version, $rakudo_version );
    while ( @test_history > $self->{'Timings'} ) { shift @test_history; }
    print $file_out qq!{"test_history":[\n!;
    print $file_out "  " . join(",\n  ",@test_history) . "\n ],\n";
    print $file_out qq! "test_microseconds":{!;
    $ENV{'PERL6_TEST_TIMES'} = 'true';
    return bless $self;
}

sub log_script_times {
    my $self      = shift;
    my $test_name = shift;
    my $ref_times = shift;
    my $ref_comments = shift;
    my (@times) = @$ref_times;
    my (@comments) = @$ref_comments;
    shift @times;
    shift @comments;
    for ( my $i=0; $i<=@times; $i++ ) {
        if ( not defined $comments[$i] ) { $comments[$i] = ''; }
        $comments[$i] =~ s/\\/\\\\/g;
        $comments[$i] =~ s/\"/\\\"/g;
    }
    my ( $line );
    my $file_in  = $self->{'file_in'};
    my $file_out = $self->{'file_out'};
    $test_name =~ s{^t/spec/}{};
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
        while ( @times_in_file > $self->{'Timings'} ) { shift @times_in_file; }
        $$microseconds[$test_number] = [ @times_in_file ];
    }
    my $test_number = 1;
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
