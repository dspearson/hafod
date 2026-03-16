#!/usr/bin/perl
# run-inline.pl -- Run all benchmarks in-process (no startup overhead)
# Runs each runtime once, measures operations within the process.
use strict;
use warnings;
use File::Basename;
use File::Spec;
use Cwd qw(abs_path);

my $BENCHDIR = abs_path(dirname(__FILE__));
my $PROJDIR  = abs_path("$BENCHDIR/..");
my $SRCDIR   = "$PROJDIR/src";
my $SCSH_DIR = "/tmp/scsh-master";
my $SCSH     = "$SCSH_DIR/go";

my $HAFOD_SO = "$PROJDIR/bin/hafod.so";
my $CHEZ = $ENV{HAFOD_SCHEME}
    // (grep { -x $_ } map { "$_/petite" } split(/:/, $ENV{PATH} // ''))[0]
    // 'scheme';
my $have_native = -f $HAFOD_SO;
my $HAFOD = $ENV{HAFOD}
    // ($have_native
        ? qq{$CHEZ --libdirs "$SRCDIR" --program "$HAFOD_SO"}
        : qq{"$PROJDIR/bin/hafod"});

my $have_scsh = -x $SCSH;
my $ITERATIONS = $ENV{BENCH_ITERS} // 3;

sub parse_bench_output {
    my ($output) = @_;
    my %results;
    for my $line (split /\n/, $output) {
        if ($line =~ /^BENCH\s+(\S+)\s+([\d.]+)/) {
            $results{$1} = $2 + 0;
        }
    }
    return %results;
}

sub run_n_times {
    my ($cmd, $n) = @_;
    my %all;
    for my $iter (1..$n) {
        printf STDERR "\r  \e[2mrun %d/%d...\e[0m", $iter, $n;
        my $output = `$cmd 2>/dev/null`;
        my %r = parse_bench_output($output);
        for my $k (keys %r) {
            push @{$all{$k}}, $r{$k};
        }
    }
    printf STDERR "\r%30s\r", "";
    # Return best (min) for each benchmark
    my %best;
    for my $k (keys %all) {
        my @sorted = sort { $a <=> $b } @{$all{$k}};
        $best{$k} = $sorted[0];
    }
    return %best;
}

# Header
print "\n";
print "\e[1m";
print "  ╔══════════════════════════════════════════════════════════════════════════╗\n";
print "  ║                    hafod in-process benchmark suite                      ║\n";
print "  ╚══════════════════════════════════════════════════════════════════════════╝\n";
print "\e[0m\n";
printf "  hafod:       %s\n", $HAFOD;
printf "  scsh:        %s\n", $have_scsh ? $SCSH : "(not found)";
printf "  iterations:  %d (best-of)\n", $ITERATIONS;
print "  \e[2mstartup excluded — timing is per-operation only\e[0m\n\n";

# Run hafod
printf "  \e[1mRunning hafod benchmarks...\e[0m\n";
my %hafod = run_n_times(qq{$HAFOD -s "$BENCHDIR/all-hafod.ss"}, $ITERATIONS);

# Run scsh
my %scsh;
if ($have_scsh) {
    printf "  \e[1mRunning scsh benchmarks...\e[0m\n";
    %scsh = run_n_times(qq{cd "$SCSH_DIR" && "$SCSH" -s "$BENCHDIR/all-scsh.scm"}, $ITERATIONS);
}

# Display results
print "\n";
my @names = qw(fork-exec pipeline string-io regex file-ops computation
               env-ops glob readline field-split awk regex-subst
               redir temp-file with-cwd);

if ($have_scsh) {
    printf "  \e[1;4mhafod vs scsh (in-process, startup excluded)\e[0m\n\n";
    printf "  %-22s  %10s  %10s  %8s\n", "Benchmark", "hafod", "scsh", "Ratio";
    printf "  %s  %s  %s  %s\n", '-' x 22, '-' x 10, '-' x 10, '-' x 8;
} else {
    printf "  \e[1;4mhafod (in-process)\e[0m\n\n";
    printf "  %-22s  %10s\n", "Benchmark", "hafod";
    printf "  %s  %s\n", '-' x 22, '-' x 10;
}

my @ratios;
for my $name (@names) {
    my $h = $hafod{$name};
    next unless defined $h;

    if ($have_scsh && defined $scsh{$name}) {
        my $s = $scsh{$name};
        my $ratio = ($s > 0.001) ? $h / $s : 0;
        push @ratios, $ratio if $ratio > 0;

        my $bar = '';
        if ($ratio > 0) {
            my $log = log($ratio) / log(2);
            my $len = int(abs($log) * 4 + 0.5);
            $len = 1 if $len < 1;
            $len = 14 if $len > 14;
            $bar = ($ratio < 1.0)
                ? "\e[32m" . ('<' x $len) . "\e[0m"
                : "\e[31m" . ('>' x $len) . "\e[0m";
        }

        printf "  %-22s  %9.1f ms  %9.1f ms  %6.2fx %s\n",
            $name, $h, $s, $ratio, $bar;
    } else {
        printf "  %-22s  %9.1f ms\n", $name, $h;
    }
}

if ($have_scsh && @ratios) {
    my $log_sum = 0;
    $log_sum += log($_) for @ratios;
    my $geo_mean = exp($log_sum / scalar @ratios);

    printf "\n  %s\n", '-' x 60;
    printf "  %-22s  %21s  %6.2fx %s\n",
        "geometric mean", "",
        $geo_mean,
        ($geo_mean < 1)
            ? "\e[32m" . ('<' x int(abs(log($geo_mean)/log(2)) * 4 + 0.5)) . "\e[0m"
            : "\e[31m" . ('>' x int(abs(log($geo_mean)/log(2)) * 4 + 0.5)) . "\e[0m";
    print "\n  \e[2m< hafod faster    > scsh faster    (ratio = hafod/scsh)\e[0m\n";
}
print "\n  Done.\n\n";
