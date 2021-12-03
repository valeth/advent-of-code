#!/usr/bin/env perl

# use warnings;
use strict;
use lib "./";
use common;
use Data::Dumper;

sub part_one {
    my $numbers = $_[0];
    my $count = @$numbers;
    my $digits = @{$numbers->[0]};
    my @ones = ((0) x $digits);

    for (@$numbers) {
        my @row = @$_;
        for my $idx (0 .. $#row) {
            @ones[$idx] += 1 if @row[$idx] == "1";
        }
    }

    my @gamma_rate = ();
    my @epsilon_rate = ();

    for (@ones) {
        my $zeroes = $count - $_;
        if ($zeroes < $_) {
            push(@gamma_rate, 1);
            push(@epsilon_rate, 0);
        } else {
            push(@gamma_rate, 0);
            push(@epsilon_rate, 1);
        }
    }
    my $gamma_rate = oct("0b" . join("", @gamma_rate));
    my $epsilon_rate = oct("0b" . join("", @epsilon_rate));

    return $gamma_rate * $epsilon_rate;
}

my @numbers = common::read_file($ARGV[0]);
my $result = part_one(\@numbers);
print $result, "\n";
