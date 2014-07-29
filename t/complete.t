#!perl

use 5.010;
use strict;
use warnings;

use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_complete);

$Test::Perinci::CmdLine::CLASS = 'Perinci::CmdLine::Lite';

test_complete(
    args       => {url=>'/Perinci/Examples/test_completion'},
    comp_line0 => 'cmd --s1 ap^',
    result     => ['apple', 'apricot'],
);

DONE_TESTING:
done_testing;
