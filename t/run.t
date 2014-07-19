#!perl

use 5.010;
use strict;
use warnings;

use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_complete test_run);

$Test::Perinci::CmdLine::CLASS = 'Perinci::CmdLine::Lite';

our %SPEC;

$SPEC{f_hello} = {
    v => 1.1,
    summary => 'Summary for f_hello',
    description => <<'_',

Description for f_hello

_
};
sub f_hello {
    my %args = @_;
    [200, "OK", "Hello, world!"];
}

test_run(
    args      => {url=>'/main/f_hello'},
    argv      => [],
    exit_code => 0,
    output_re => qr/\AHello, world!\n\z/,
);

DONE_TESTING:
done_testing;
