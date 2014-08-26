#!perl

use 5.010;
use strict;
use warnings;

use File::Slurp::Tiny qw(write_file);
use File::Temp qw(tempdir);
use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_run);

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
    test_run(
        name      => 'unknown subcommand = error',
        args      => {subcommands => {
            noop => {url=>'/Perinci/Examples/noop'},
            dies => {url=>'/Perinci/Examples/dies'},
        }},
        argv      => [qw/foo/],
        exit_code => 200,
    );
    test_run(
        name      => 'default_subcommand',
        args      => {subcommands => {
            noop => {url=>'/Perinci/Examples/noop'},
            dies => {url=>'/Perinci/Examples/dies'},
        },
                      default_subcommand=>'noop'},
        argv      => [qw//],
        exit_code => 0,
    );
    test_run(
        name      => 'default_subcommand 2',
        args      => {subcommands => {
            noop => {url=>'/Perinci/Examples/noop'},
            dies => {url=>'/Perinci/Examples/dies'},
        },
                      default_subcommand=>'dies'},
        argv      => [qw/--cmd noop/],
        exit_code => 0,
    );
};

subtest 'output formats' => sub {
    test_run(
        name      => '--json',
        args      => {url => '/Perinci/Examples/sum'},
        argv      => [qw/1 2 3 --json/],
        exit_code => 0,
        output_re => qr/^\[200,"OK",6,\{\}\]/,
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

subtest 'result metadata' => sub {
    subtest 'cmdline.exit_code' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/exit_code'},
            argv      => [qw//],
            status    => 200,
            exit_code => 7,
        );
    };
    subtest 'cmdline.result' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/result'},
            argv      => [qw//],
            output_re => qr/false/,
        );
    };
    subtest 'cmdline.default_format' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/default_format'},
            argv      => [qw//],
            output_re => qr/null/,
        );
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/default_format'},
            argv      => [qw/--format text/],
            output_re => qr/\A\z/,
        );
    };
    subtest 'cmdline.skip_format' => sub {
        test_run(
            args      => {url=>'/Perinci/Examples/CmdLineResMeta/skip_format'},
            argv      => [qw//],
            output_re => qr/ARRAY\(0x/,
        );
    };
};

subtest 'config' => sub {
    my $dir = tempdir(CLEANUP=>1);
    write_file("$dir/prog.conf", <<'_');
arg=101
[subcommand1]
arg=102
[subcommand2]
arg=103
[profile=profile1]
arg=111
[subcommand1 profile=profile1]
arg=121
_
    write_file("$dir/prog2.conf", <<'_');
arg=104
_
    test_run(
        name => 'config_dirs',
        args => {
            url=>'/Perinci/Examples/noop',
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [],
        output_re => qr/101/,
    );
    test_run(
        name => 'config_filename',
        args => {
            url=>'/Perinci/Examples/noop',
            program_name=>'prog',
            config_filename=>'prog2.conf',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [],
        output_re => qr/104/,
    );
    test_run(
        name => '--noconfig',
        args => {
            url=>'/Perinci/Examples/noop',
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/--noconfig/],
        output_re => qr/^$/,
    );
    test_run(
        name => '--config-path',
        args => {
            url=>'/Perinci/Examples/noop',
            program_name=>'prog',
            read_config=>1,
            #config_dirs=>[$dir],
        },
        argv => ['--config-path', "$dir/prog.conf"],
        output_re => qr/^101$/,
    );
    test_run(
        name => '--config-profile',
        args => {
            url=>'/Perinci/Examples/noop',
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/--config-profile=profile1/],
        output_re => qr/111/,
    );
    test_run(
        name => 'subcommand',
        args => {
            subcommands => {
                subcommand1=>{url=>'/Perinci/Examples/noop'},
            },
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/subcommand1/],
        output_re => qr/102/,
    );
    test_run(
        name => 'subcommand + --config-profile',
        args => {
            subcommands => {
                subcommand1=>{url=>'/Perinci/Examples/noop'},
            },
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/--config-profile=profile1 subcommand1/],
        output_re => qr/121/,
    );
};

DONE_TESTING:
done_testing;
