package common;

use strict;

sub read_file {
    my $input_path = $_[0];

    open(FH, "<", $input_path) or die $!;

    my @lines;

    while(<FH>) {
        my $line = $_;
        $line =~ s/\s+$//; # rtrim
        my @chars = split('', $line);
        push(@lines, \@chars);
    }

    close(FH);

    return @lines;
}

1; # module needs to return a "true value", because reasons
