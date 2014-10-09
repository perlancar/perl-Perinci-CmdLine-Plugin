#!perl

# test that function and metadata embedded directly in script work.

use 5.010;
use strict;
use warnings;

use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_complete test_run);

$Test::Perinci::CmdLine::CLASS = 'Perinci::CmdLine::Lite';

our %SPEC;

$SPEC{hello} = {
    v => 1.1,
    args => {
        bar => {schema=>'str'},
    },
};
sub hello {
    [200, "OK", "Hello, world!"];
}

test_run(
    name      => 'run works',
    args      => {url=>'/main/hello'},
    argv      => [],
    exit_code => 0,
    output_re => qr/\AHello, world!\n\z/,
);

test_complete(
    args       => {url=>'/main/hello'},
    comp_line0 => 'cmd --bar^',
    result     => ['--bar'],
);

DONE_TESTING:
done_testing;
