#!/usr/bin/perl
# run-benchmarks.pl -- Unified benchmark suite: hafod vs scsh
use strict;
use warnings;
use File::Basename;
use File::Spec;
use Cwd qw(abs_path);
use Time::HiRes qw(time);

my $BENCHDIR = abs_path(dirname(__FILE__));
my $PROJDIR  = abs_path("$BENCHDIR/..");
my $HAFOD    = "$PROJDIR/bin/hafod";
my $SCSH_DIR = "/tmp/scsh-master";
my $SCSH     = "$SCSH_DIR/go";
my $ITERATIONS = 7;

my $have_scsh = -x $SCSH;
my $is_tty = -t STDERR;

# Descriptions for thread-only benchmarks
my %thread_desc = (
    '09-threads'        => 'spawn+join 10k threads',
    '10-channel'        => 'channel 50k msgs (buf=64)',
    '11-thread-preempt' => '100 threads x 10k work',
    '12-thread-ring'    => '50-thread ring x 5k passes',
);

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
        my ($h_best, $h_med) = run_timed(qq{"$HAFOD" -s "$hafod_script"}, $ITERATIONS);

        if ($have_scsh) {
            my ($s_best, $s_med) = run_timed(qq{cd "$SCSH_DIR" && "$SCSH" -s "$scsh_script"}, $ITERATIONS);
            clear_progress();

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
        my ($best, $med) = run_timed(qq{"$HAFOD" -s "$script"}, $ITERATIONS);
        clear_progress();

        printf "  %-22s  %9s  %9s   %s\n",
            $name, fmt_time($best), fmt_time($med), $desc;
    }
    print "\n";
}

print "  Done. ($ITERATIONS iterations per benchmark)\n\n";
