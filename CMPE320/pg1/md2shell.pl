#!/usr/bin/perl
use strict;

my sub issingle($) 
{
    my ($string) = @_;

    my $state = 0;
    my $idx = 0;
    my $return_val = 1;
    my $BACK_TICK = '`';

LOOP:
    while (($idx < length($string)) && ($state >= 0)) {

        my $ch = substr($string,$idx, 1);
        #print "ch=$ch, state=$state\n";

        $idx++;
        if ($state == 0) {
            if ($ch eq $BACK_TICK) {
                $state = 1;
            }
            else {
                $return_val = 0;
                last LOOP;     
            }
        } # end case 0
        elsif ($state == 1) {
            if ($ch eq $BACK_TICK) {
                $return_val = 0;
                last LOOP;
            }
            else {
                $state = 2;
            }
        }
        elsif($state == 2) {
            if ($ch eq $BACK_TICK) {
                if ($idx >= length($string))  {
                    last LOOP;
                } 
                else {
                    $state = 3;
                }
            }
            else {
                $state = 2;
            }
        }
        elsif($state == 3) {
            $return_val = 0;
            last LOOP;
        }
    } # end while

    return $return_val;
} # end sub


my sub ismulti($)
{
    my ($string) = @_;

    my $MULTI_TICK = '```';
    my $MULTI_TILDE = '~~~';

    if ($string eq $MULTI_TICK) {
        return 1;
    }

    if ($string eq $MULTI_TILDE) { 
        return 1;
    }

    return 0;
}

# main

my $multi = 0;
my $qa = 0;

LOOP:
while (<>) {
    chomp;

    if ($_ eq "## Questions and Answers") {
        $qa = 1;
        print "#!/bin/bash\n";
    }
    elsif ($qa == 0)  {
        next LOOP;
    }

    if (issingle($_)) {

        # found a simple shell escape
        # write it to output file, strip out ``
        my $line = substr($_, 1, length($_)-2);

        print "$line\n";
    }
    
    elsif (ismulti($_)) {
        $multi = ($multi == 0) ? 1 : 0;
    }

    elsif ($multi == 1) {
        s/^\s+//g;

        print "$_\n";
    }

    elsif (/^(\d+\.)/) {
        print "# " . substr($_, 0, 32) . "\n";
    }
}