#!perl

use 5.010;
use strict;
use warnings;

use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_complete);

test_complete(
    args       => {url=>'/Perinci/Examples/test_completion'},
    comp_line0 => 'cmd --s1 ap^',
    result     => ['apple', 'apricot'],
);

test_complete(
    name       => 'completing positional argument, with subcommands',
    args       => {subcommands=>{
        sc1 => {url=>'/Perinci/Examples/test_completion'},
    }},
    comp_line0 => 'cmd sc1 9^',
    result     => [9, 90..99],
);

# XXX test complete for cli with subcommands + PROG_OPT

DONE_TESTING:
done_testing;
