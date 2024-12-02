#!/usr/bin/perl

my $line = 0;
my $first, $mid, $last;
my $street, $city, $state, $zip;
my $phone, $email;

while(<>) {
    chomp;

    if ($line == 0) {
        /^(\w+) (.*?) (\w+)/;
        $first = $1;
        $mid = $2;
        $last = $3;
    }

    if ($line == 1) {
        $street = $_;
    }

    if ($line == 2) {
        /^(.*), (..) (\d{5})/;
        $city = $1;
        $state = $2;
        $zip = $3;
    }

    if ($line == 14) {
        $phone = $_;
    }

    if ($line == 26) {
        $email = $_;


        print "$last,$first,$phone,$email,$street,$city,$state,$zip\n";
        exit;
        $line = 0;
    }

    $line = $line + 1;

}

