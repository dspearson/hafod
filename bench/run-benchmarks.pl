#!/usr/bin/perl
# run-benchmarks.pl -- Unified benchmark suite: hafod vs scsh
use strict;
use warnings;
use File::Basename;
use File::Spec;
use Cwd qw(abs_path);
use Time::HiRes qw(time);
use Getopt::Long;
use POSIX qw(strftime);

my $BENCHDIR = abs_path(dirname(__FILE__));
my $PROJDIR  = abs_path("$BENCHDIR/..");
my $SRCDIR   = "$PROJDIR/src";
my $SCSH_DIR = "/tmp/scsh-master";
my $SCSH     = "$SCSH_DIR/go";
my $ITERATIONS = 7;

my $save_results = 0;
GetOptions('save' => \$save_results);

my $RESULTS_CSV = "$BENCHDIR/results/benchmarks.csv";

# Use compiled native program directly, bypassing shell wrapper.
# Override with HAFOD env var to test a specific binary (e.g. hafod-standalone).
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
my $is_tty = -t STDERR;

# Descriptions for thread-only benchmarks
my %thread_desc = (
    '09-threads'        => 'spawn+join 10k threads',
    '10-channel'        => 'channel 50k msgs (buf=64)',
    '11-thread-preempt' => '100 threads x 10k work',
    '12-thread-ring'    => '50-thread ring x 5k passes',
);

# Collected results for --save
my @saved_rows;

sub run_timed {
    my ($cmd, $n) = @_;
    my @times;
    for (1 .. $n) {
        my $t0 = time();
        system("$cmd >/dev/null 2>&1");
        my $t1 = time();
        push @times, $t1 - $t0;
    }
    @times = sort { $a <=> $b } @times;
    my $best   = $times[0];
    my $median = $times[int(@times / 2)];
    return ($best, $median);
}

sub progress {
    return unless $is_tty;
    printf STDERR "\r  \e[2mrunning %-26s\e[0m", "$_[0]...";
}

sub clear_progress {
    return unless $is_tty;
    printf STDERR "\r%40s\r", "";
}

sub ratio_bar {
    my ($ratio) = @_;
    return '' if $ratio <= 0;
    my $log = log($ratio) / log(2);
    my $len = int(abs($log) * 4 + 0.5);
    $len = 1 if $len < 1;
    $len = 12 if $len > 12;
    if ($ratio < 1.0) {
        return "\e[32m" . ('<' x $len) . "\e[0m";
    } else {
        return "\e[31m" . ('>' x $len) . "\e[0m";
    }
}

sub fmt_time {
    my ($t) = @_;
    return sprintf("%6.3f s", $t);
}

sub save_row {
    my ($name, $best, $median, $mode) = @_;
    return unless $save_results;
    push @saved_rows, {
        name   => $name,
        best   => $best,
        median => $median,
        mode   => $mode,
    };
}

# Gather benchmarks
my (@compare_benches, @thread_benches);
opendir(my $dh, $BENCHDIR) or die "Cannot open $BENCHDIR: $!";
for my $f (sort readdir $dh) {
    next unless $f =~ /^(\d+-[^.]+)-hafod\.ss$/;
    my $name = $1;
    if (-f "$BENCHDIR/${name}-scsh.scm") {
        push @compare_benches, $name;
    } else {
        push @thread_benches, $name;
    }
}
closedir $dh;

# Header
print "\n";
print "\e[1m";
print "  ╔══════════════════════════════════════════════════════════════════════════╗\n";
print "  ║                          hafod benchmark suite                           ║\n";
print "  ╚══════════════════════════════════════════════════════════════════════════╝\n";
print "\e[0m\n";
printf "  hafod:       %s\n", $HAFOD;
printf "  scsh:        %s\n", $have_scsh ? $SCSH : "(not found -- skipping comparisons)";
printf "  iterations:  %d (best-of / median)\n\n", $ITERATIONS;

# Part 1: hafod vs scsh
if (@compare_benches) {
    my $hdr = $have_scsh ? "hafod vs scsh" : "hafod (scsh not available)";
    printf "  \e[1;4m%s\e[0m\n\n", $hdr;

    if ($have_scsh) {
        printf "  %-22s  %9s %9s   %9s %9s   %7s\n",
            "Benchmark", "hafod", "(med)", "scsh", "(med)", "Ratio";
        printf "  %s  %s %s   %s %s   %s\n",
            '-' x 22, '-' x 9, '-' x 9, '-' x 9, '-' x 9, '-' x 7;
    } else {
        printf "  %-22s  %9s  %9s\n", "Benchmark", "best", "median";
        printf "  %s  %s  %s\n", '-' x 22, '-' x 9, '-' x 9;
    }

    my @ratios;

    for my $name (@compare_benches) {
        my $hafod_script = "$BENCHDIR/${name}-hafod.ss";
        my $scsh_script  = "$BENCHDIR/${name}-scsh.scm";

        progress($name);
        my ($h_best, $h_med) = run_timed(qq{$HAFOD -s "$hafod_script"}, $ITERATIONS);
        save_row($name, $h_best, $h_med, 'hafod');

        if ($have_scsh) {
            my ($s_best, $s_med) = run_timed(qq{cd "$SCSH_DIR" && "$SCSH" -s "$scsh_script"}, $ITERATIONS);
            clear_progress();
            save_row($name, $s_best, $s_med, 'scsh');

            my $ratio = ($s_best > 0) ? $h_best / $s_best : 0;
            push @ratios, $ratio if $ratio > 0;
            my $bar = ratio_bar($ratio);

            printf "  %-22s  %9s %9s   %9s %9s   %6.2fx %s\n",
                $name,
                fmt_time($h_best), fmt_time($h_med),
                fmt_time($s_best), fmt_time($s_med),
                $ratio, $bar;
        } else {
            clear_progress();
            printf "  %-22s  %9s  %9s\n",
                $name, fmt_time($h_best), fmt_time($h_med);
        }
    }

    if ($have_scsh && @ratios) {
        # Geometric mean of ratios
        my $log_sum = 0;
        $log_sum += log($_) for @ratios;
        my $geo_mean = exp($log_sum / scalar @ratios);

        printf "\n  %s\n", '-' x 72;
        printf "  %-22s  %39s   %6.2fx %s\n",
            "geometric mean", "", $geo_mean, ratio_bar($geo_mean);
        print "\n  \e[2m< hafod faster    > scsh faster    (ratio = hafod/scsh)\e[0m\n";
    }
    print "\n";
}

# Part 2: hafod-only (thread benchmarks)
if (@thread_benches) {
    printf "  \e[1;4mhafod green threads (no scsh equivalent)\e[0m\n\n";
    printf "  %-22s  %9s  %9s   %s\n", "Benchmark", "best", "median", "Description";
    printf "  %s  %s  %s   %s\n", '-' x 22, '-' x 9, '-' x 9, '-' x 28;

    for my $name (@thread_benches) {
        my $script = "$BENCHDIR/${name}-hafod.ss";
        my $desc = $thread_desc{$name} // '';

        progress($name);
        my ($best, $med) = run_timed(qq{$HAFOD -s "$script"}, $ITERATIONS);
        clear_progress();
        save_row($name, $best, $med, 'hafod');

        printf "  %-22s  %9s  %9s   %s\n",
            $name, fmt_time($best), fmt_time($med), $desc;
    }
    print "\n";
}

# Save results to CSV if --save
if ($save_results && @saved_rows) {
    my $date = strftime('%Y-%m-%d', localtime);
    my $need_header = ! -f $RESULTS_CSV;

    open(my $fh, '>>', $RESULTS_CSV) or die "Cannot open $RESULTS_CSV: $!";
    if ($need_header) {
        print $fh "date,benchmark,best_s,median_s,mode\n";
    }
    for my $row (@saved_rows) {
        printf $fh "%s,%s,%.3f,%.3f,%s\n",
            $date, $row->{name}, $row->{best}, $row->{median}, $row->{mode};
    }
    close $fh;
    print "  Results saved to bench/results/benchmarks.csv\n";
}

print "  Done. ($ITERATIONS iterations per benchmark)\n\n";
