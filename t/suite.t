#!perl

# run the Test::Perinci::CmdLine test suite

use 5.010;
use strict;
use warnings;

use Test::More 0.98;
use Test::Perinci::CmdLine qw(pericmd_ok);

pericmd_ok(class => 'Perinci::CmdLine::Lite');
done_testing;
