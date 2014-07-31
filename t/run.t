#!perl

use 5.010;
use strict;
use warnings;

use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_complete test_run);

$Test::Perinci::CmdLine::CLASS = 'Perinci::CmdLine::Lite';

subtest 'help action' => sub {
    test_run(
        name      => 'help action',
        args      => {url=>'/Perinci/Examples/noop'},
        argv      => [qw/--help/],
        exit_code => 0,
        output_re => qr/- Do nothing.+^Common options:/ms,
    );
};

subtest 'version action' => sub {
    test_run(
        name      => 'version action',
        args      => {url=>'/Perinci/Examples/noop'},
        argv      => [qw/--version/],
        exit_code => 0,
        output_re => qr/version \Q$Perinci::Examples::VERSION\E/,
    );
};

subtest 'subcommands action' => sub {
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
};

subtest 'output formats' => sub {
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
        name      => 'multiple subcommands (subcommand not specified -> help)',
        args      => {url => '/Perinci/Examples/',
                      subcommands => {
                          s => {url=>'/Perinci/Examples/sum'},
                          m => {url=>'/Perinci/Examples/merge_hash'},
                      }
                  },
        argv      => [qw//],
        exit_code => 0,
        output_re => qr/Common options/,
    );
    test_run(
        name      => 'multiple subcommands (subc specified via first cli arg)',
        args      => {url => '/Perinci/Examples/',
                      subcommands => {
                          s => {url=>'/Perinci/Examples/sum'},
                          m => {url=>'/Perinci/Examples/merge_hash'},
                      }
                  },
        argv      => [qw/s --array 2 --array 3 --array 4/],
        exit_code => 0,
        output_re => qr/9/,
    );
    test_run(
        name      => 'multiple subcommands (subc specified via '.
            'default_subcommand)',
        args      => {url => '/Perinci/Examples/',
                      subcommands => {
                          s => {url=>'/Perinci/Examples/sum'},
                          m => {url=>'/Perinci/Examples/merge_hash'},
                      },
                      default_subcommand => 's',
                  },
        argv      => [qw/--array 2 --array 3 --array 4/],
        exit_code => 0,
        output_re => qr/9/s,
    );
    test_run(
        name      => 'multiple subcommands (subc specified via --cmd)',
        args      => {url => '/Perinci/Examples/',
                      subcommands => {
                          s => {url=>'/Perinci/Examples/sum'},
                          m => {url=>'/Perinci/Examples/merge_hash'},
                      },
                      default_subcommand => 's',
                  },
        argv      => ['--cmd', 'm',
                      '--h1-json', '{"a":11,"b":12}',
                      '--h2-json', '{"a":21,"c":23}'],
        exit_code => 0,
        output_re => qr/a[^\n]+21.+b[^\n]+12.+c[^\n]+23/s,
    );

    test_run(
        name      => 'args_as array',
        args      => {url=>'/Perinci/Examples/test_args_as_array'},
        argv      => [qw/--a0 zero --a1 one --a2 two --format text-simple/],
        exit_code => 0,
        output_re => qr/^zero\none\ntwo/,
    );
    test_run(
        name      => 'args_as arrayref',
        args      => {url=>'/Perinci/Examples/test_args_as_arrayref'},
        argv      => [qw/--a0 zero --a1 one --a2 two --format text-simple/],
        exit_code => 0,
        output_re => qr/^zero\none\ntwo/,
    );
    test_run(
        name      => 'args_as hashref',
        args      => {url=>'/Perinci/Examples/test_args_as_hashref'},
        argv      => [qw/--a0 zero --a1 one --format text-simple/],
        exit_code => 0,
        output_re => qr/^a0\s+zero\na1\s+one/,
    );

    test_run(
        name      => 'result_naked',
        args      => {url=>'/Perinci/Examples/test_result_naked'},
        argv      => [qw/--a0 zero --a1 one/],
        exit_code => 0,
        output_re => qr/a0[^\n]+zero.+a1[^\n]+one/s,
    );
};

DONE_TESTING:
done_testing;
