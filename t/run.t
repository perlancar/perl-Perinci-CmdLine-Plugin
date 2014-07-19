#!perl

use 5.010;
use strict;
use warnings;

use FindBin '$Bin';
use lib "$Bin/lib";

use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_complete test_run);

$Test::Perinci::CmdLine::CLASS = 'Perinci::CmdLine::Lite';
our $url_prefix = "/Test/Perinci/CmdLine/Examples/";

test_run(
    args      => {url=>"${url_prefix}f_hello"},
    argv      => [],
    exit_code => 0,
    output_re => qr/\AHello, world!\n\z/,
);

DONE_TESTING:
done_testing;
