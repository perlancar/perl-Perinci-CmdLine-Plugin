#!perl

# test that function and metadata embedded directly in script work.

use 5.010;
use strict;
use warnings;

use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_complete test_run);

our %SPEC;

$SPEC{hello} = {
    v => 1.1,
    args => {
        bar => {
            schema => 'str',
        },
        baz => {
            schema => 'hash',
            meta => {
                v => 1.1,
                args => {
                    qux => {schema => 'str'},
                },
            },
        },
    },
};
sub hello {
    my %args = @_;
    my $greet_word = $args{baz}{qux} // "Hello";
    [200, "OK", "$greet_word, world!"];
}

test_run(
    name      => 'run works',
    args      => {url=>'/main/hello'},
    argv      => [qw/--baz-qux Ola/],
    exit_code => 0,
    output_re => qr/\AOla, world!\n\z/,
);

test_complete(
    args       => {url=>'/main/hello'},
    comp_line0 => 'cmd --bar^',
    result     => ['--bar'],
);

DONE_TESTING:
done_testing;
