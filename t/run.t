#!perl

use 5.010;
use strict;
use warnings;

use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_complete test_run);

$Test::Perinci::CmdLine::CLASS = 'Perinci::CmdLine::Lite';

test_run(
    name      => 'help action',
    args      => {url=>'/Perinci/Examples/noop'},
    argv      => [qw/--help/],
    exit_code => 0,
    output_re => qr/- Do nothing.+^Common options:/ms,
);

test_run(
    name      => 'version action',
    args      => {url=>'/Perinci/Examples/noop'},
    argv      => [qw/--version/],
    exit_code => 0,
    output_re => qr/version \Q$Perinci::Examples::VERSION\E/,
);

test_run(
    name      => 'subcommands action',
    args      => {subcommands => {
        noop => {url=>'/Perinci/Examples/noop'},
        dies => {url=>'/Perinci/Examples/dies'},
    }},
    argv      => [qw/--subcommands/],
    exit_code => 0,
    output_re => qr/^Available subcommands:\s+dies\s+noop/ms,
);

subtest '--format option' => sub {
    test_run(
        name      => '--json',
        args      => {url => '/Perinci/Examples/sum'},
        argv      => [qw/1 2 3 --json/],
        exit_code => 0,
        output_re => qr/^\[200,"OK",6\]/,
    );
    subtest 'text-pretty' => sub {
        test_run(
            name      => 'undef',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-pretty undef/],
            exit_code => 0,
            output_re => qr/\A\z/,
        );
        test_run(
            name      => 'scalar',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-pretty scalar/],
            exit_code => 0,
            output_re => qr/\ASample data\n\z/,
        );
        test_run(
            name      => 'empty array',
            args      => {url => '/Perinci/Examples/gen_array'},
            argv      => [qw/--format=text-pretty --len 0/],
            exit_code => 0,
            output_re => qr/\A\z/,
        );
        test_run(
            name      => 'aos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-pretty aos/],
            exit_code => 0,
            output_re => qr/\A
                            \+-+\+\n
                            \|\s*one\s*\|\n/x,
        );
        test_run(
            name      => 'aoaos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-pretty aoaos/],
            exit_code => 0,
            output_re => qr/\A
                            \+-+\+-+\+ .+\n
                            \|\s*This\s*\|\s*is\s*\| .+\n/x,
        );
        test_run(
            name      => 'aohos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-pretty aohos/],
            exit_code => 0,
            output_re => qr/\A
                            \+-+\+-+\+ .+\n
                            \|\s*field1\s*\|\s*field2\s*\| .+\n/x,
        );
        test_run(
            name      => 'hos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-pretty hos/],
            exit_code => 0,
            output_re => qr/\A
                            \+-+\+-+\+ \n
                            \|\s*key\s*\|\s*value\s*\| \n/x,
        );
        test_run(
            name      => 'hohos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-pretty hohos/],
            exit_code => 0,
            output_re => qr/\A
                            \[\s*200,\s*"OK"/x,
        );
    }; # text-pretty

    subtest 'text-simple' => sub {
        test_run(
            name      => 'undef',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-simple undef/],
            exit_code => 0,
            output_re => qr/\A\z/,
        );
        test_run(
            name      => 'scalar',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-simple scalar/],
            exit_code => 0,
            output_re => qr/\ASample data\n\z/,
        );
        test_run(
            name      => 'empty array',
            args      => {url => '/Perinci/Examples/gen_array'},
            argv      => [qw/--format=text-simple --len 0/],
            exit_code => 0,
            output_re => qr/\A\z/,
        );
        test_run(
            name      => 'aos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-simple aos/],
            exit_code => 0,
            output_re => qr/\Aone\ntwo\n/x,
        );
        test_run(
            name      => 'aoaos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-simple aoaos/],
            exit_code => 0,
            output_re => qr/\AThis\tis\tthe\tfirst\trow\n
                            This\tis\tthe\tsecond\trow\n/x,
        );
        test_run(
            name      => 'aohos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-simple aohos/],
            exit_code => 0,
            output_re => qr/\A11\t12\t\n21\t\t23\n/x,
        );
        test_run(
            name      => 'hos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-simple hos/],
            exit_code => 0,
            output_re => qr/\A
                            key\t1\nkey2\t2\n/x,
        );
        test_run(
            name      => 'hohos',
            args      => {url => '/Perinci/Examples/gen_sample_data'},
            argv      => [qw/--format=text-simple hohos/],
            exit_code => 0,
            output_re => qr/\A
                            \[\s*200,\s*"OK"/x,
        );
    }; # text-simple
};

subtest 'call action' => sub {
    test_run(
        name      => 'single command',
        args      => {url=>'/Perinci/Examples/sum'},
        argv      => [qw/1 2 3/],
        exit_code => 0,
        output_re => qr/6/,
    );
    test_run(
        name      => 'single command',
        args      => {url=>'/Perinci/Examples/sum'},
        argv      => [qw/1 2 3/],
        exit_code => 0,
        output_re => qr/6/,
    );
};

DONE_TESTING:
done_testing;
