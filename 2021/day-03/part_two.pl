#!/usr/bin/env perl

# use warnings;
use strict;
use lib "./";
use common;
use Data::Dumper;

sub tally_digit_column {
    my $numbers = $_[0];
    my $digit = $_[1];

    my @ones;
    my @zeroes;

    for my $num (@$numbers) {
        if ($num->[$digit] eq "1") {
            push(@ones, $num);
        } else {
            push(@zeroes, $num);
        }
    }

    return (\@ones, \@zeroes);
}

sub part_two {
    my $numbers = $_[0];
    my $digits = @{$numbers->[0]};

    my $highest = $numbers;
    my $lowest = $numbers;

    for my $idx (0..$digits) {
        if (scalar(@$highest) != 1) {
            my @results = tally_digit_column($highest, $idx);
            my $ones = @results[0];
            my $zeroes = @results[1];
            if (scalar(@$ones) >= scalar(@$zeroes)) {
                $highest = $ones;
            } else {
                $highest = $zeroes;
            }
        }

        if (scalar(@$lowest) != 1) {
            my @results = tally_digit_column($lowest, $idx);
            my $ones = @results[0];
            my $zeroes = @results[1];
            if (scalar(@$zeroes) <= scalar(@$ones)) {
                $lowest = $zeroes;
            } else {
                $lowest = $ones;
            }
        }
    }

    my $oxygen_rating = oct("0b" . join("", @{$highest->[0]}));
    my $scrubber_rating = oct("0b" . join("", @{$lowest->[0]}));

    return $oxygen_rating * $scrubber_rating;
}

my @numbers = common::read_file($ARGV[0]);
my $result = part_two(\@numbers);
print $result, "\n";
