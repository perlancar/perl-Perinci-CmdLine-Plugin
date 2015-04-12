#!perl

use 5.010;
use strict;
use warnings;

use File::Slurp::Tiny qw(write_file);
use File::Temp qw(tempdir tempfile);
use Perinci::CmdLine::Lite;
use Test::More 0.98;
use Test::Perinci::CmdLine qw(test_run);

require Perinci::Examples;

subtest 'help action' => sub {
    test_run(
        args      => {url=>'/Perinci/Examples/noop'},
        argv      => [qw/--help/],
        exit_code => 0,
        output_re => qr/- Do nothing.+^Other options:/ms,
    );
    test_run(
        name      => 'has subcommands',
        args      => {
            url => '/Perinci/Examples/',
            subcommands=>{
                sc1=>{url=>'/Perinci/Examples/gen_array'},
            },
        },
        argv      => [qw/--help/],
        exit_code => 0,
        output_re => qr/--subcommands/ms,
    );
    test_run(
        name      => 'help on subcommand',
        args      => {subcommands=>{
            sc1=>{url=>'/Perinci/Examples/gen_array'},
        }},
        argv      => [qw/sc1 --help/],
        exit_code => 0,
        output_re => qr/ sc1 -.+--len/ms,
    );
};

subtest 'version action' => sub {
    test_run(
        args      => {url=>'/Perinci/Examples/noop'},
        argv      => [qw/--version/],
        exit_code => 0,
        output_re => qr/version \Q$Perinci::Examples::VERSION\E/,
    );
};

subtest 'subcommands action' => sub {
    test_run(
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
        output_re => qr/^\[\s*200,\s*"OK",\s*6,\s*\{\}\s*\]/s,
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
            output_re => qr/\Aone\ntwo\n/x,
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
                            \[\s*"?200"?,\s*"OK"/sx,
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
                            \[\s*"?200"?,\s*"OK"/sx,
        );
    }; # text-simple
};

subtest 'call action' => sub {
    # test regression 2015-04-12
    test_run(
        name      => 'missing required args -> error',
        args      => {url=>'/Perinci/Examples/sum'},
        argv      => [],
        exit_code => 100,
    );

    test_run(
        name      => 'single command',
        args      => {url=>'/Perinci/Examples/sum'},
        argv      => [qw/1 2 3/],
        exit_code => 0,
        output_re => qr/6/,
    );
    test_run(
        name      => 'default property of arg spec is observed',
        args      => {url=>'/Perinci/Examples/gen_hash'},
        argv      => [qw//],
        exit_code => 0,
        posttest  => sub {
            my ($argv, $stdout, $stderr, $res) = @_;
            is(~~keys(%{$res->[2]}), 10, "default number of pairs");
        },
    );
    test_run(
        name      => 'schema default of arg is observed',
        args      => {url=>'/Perinci/Examples/err'},
        argv      => [qw//],
        exit_code => 200,
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
        output_re => qr/^Other options/m,
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

subtest 'cmdline_src' => sub {
    my $prefix = "/Perinci/Examples/CmdLineSrc";
    test_run(
        name   => 'unknown value',
        args   => {url=>"$prefix/cmdline_src_unknown"},
        argv   => [],
        status => 531,
    );
    test_run(
        name   => 'arg type not str/array',
        args   => {url=>"$prefix/cmdline_src_invalid_arg_type"},
        argv   => [],
        status => 531,
    );
    test_run(
        name   => 'multiple stdin',
        args   => {url=>"$prefix/cmdline_src_multi_stdin"},
        argv   => [qw/a b/],
        status => 500,
    );

    # file
    {
        my ($fh, $filename)   = tempfile();
        my ($fh2, $filename2) = tempfile();
        write_file($filename , 'foo');
        write_file($filename2, "bar\nbaz");
        test_run(
            name => 'file 1',
            args => {url=>"$prefix/cmdline_src_file"},
            argv => ['--a1', $filename],
            exit_code => 0,
            output_re => qr/a1=foo/,
        );
        test_run(
            name => 'file 1 (special hint arguments passed)',
            args => {url=>"$prefix/cmdline_src_file"},
            argv => ['--json', '--a1', $filename],
            exit_code => 0,
            output_re => qr/
                               "-cmdline_src_a1"\s*:\s*"file"
                               .+
                               "-cmdline_srcfilenames_a1"\s*:\s*\[
                           /sx,
        );
        test_run(
            name => 'file 2',
            args => {url=>"$prefix/cmdline_src_file"},
            argv => ['--a1', $filename, '--a2', $filename2],
            exit_code => 0,
            output_re => qr/a1=foo\na2=\[bar\n,baz\]/,
        );
        test_run(
            name => 'file 2 (special hint arguments passed)',
            args => {url=>"$prefix/cmdline_src_file"},
            argv => ['--json', '--a1', $filename, '--a2', $filename2],
            exit_code => 0,
            output_re => qr/
                               "-cmdline_src_a1"\s*:\s*"file"
                               .+
                               "-cmdline_src_a2"\s*:\s*"file"
                               .+
                               "-cmdline_srcfilenames_a1"\s*:\s*\[
                               .+
                               "-cmdline_srcfilenames_a2"\s*:\s*\[
                           /sx,
        );
        test_run(
            name   => 'file not found',
            args   => {url=>"$prefix/cmdline_src_file"},
            argv   => ['--a1', $filename . "/x"],
            status => 500,
        );
        test_run(
            name   => 'file, missing required arg',
            args   => {url=>"$prefix/cmdline_src_file"},
            argv   => ['--a2', $filename],
            status => 400,
        );
    }

    # stdin_or_file
    {
        my ($fh, $filename)   = tempfile();
        my ($fh2, $filename2) = tempfile();
        write_file($filename , 'foo');
        write_file($filename2, "bar\nbaz");
        test_run(
            name => 'stdin_or_file file',
            args => {url=>"$prefix/cmdline_src_stdin_or_file_str"},
            argv => [$filename],
            exit_code => 0,
            output_re => qr/a1=foo$/,
        );
        test_run(
            name => 'stdin_or_file file (extra argument)',
            args => {url=>"$prefix/cmdline_src_stdin_or_file_str"},
            argv => [$filename, $filename],
            exit_code => 0,
            output_re => qr/a1=foo$/,
        );
        test_run(
            name => 'stdin_or_file file (special hint arguments passed)',
            args => {url=>"$prefix/cmdline_src_stdin_or_file_str"},
            argv => ['--json', $filename],
            exit_code => 0,
            output_re => qr/
                               "-cmdline_src_a1"\s*:\s*"stdin_or_file"
                               .+
                               "-cmdline_srcfilenames_a1"\s*:\s*\[
                           /sx,
        );
        test_run(
            name   => 'stdin_or_files file not found',
            args   => {url=>"$prefix/cmdline_src_stdin_or_file_str"},
            argv   => [$filename . "/x"],
            status => 500,
        );

        # i don't know why these tests don't work, they should though. and if
        # tested via a cmdline script like
        # examples/cmdline_src-stdin_or_file-{str,array} they work fine.
        if (0) {
            open $fh, '<', $filename2;
            local *STDIN = $fh;
            local @ARGV;
            test_run(
                name => 'stdin_or_file stdin str',
                args => {url=>"$prefix/cmdline_src_stdin_or_file_str"},
                argv => [],
                exit_code => 0,
                output_re => qr/a1=bar\nbaz$/,
            );
            # XXX test special hint arguments passed
        }
        if (0) {
            open $fh, '<', $filename2;
            local *STDIN = $fh;
            local @ARGV;
            test_run(
                name => 'stdin_or_file stdin str',
                args => {url=>"$prefix/cmdline_src_stdin_or_file_array"},
                argv => [],
                exit_code => 0,
                output_re => qr/a1=\[bar\n,baz\]/,
            );
            # XXX test special hint arguments passed
        }
    }

    # stdin_or_files
    {
        my ($fh, $filename)   = tempfile();
        my ($fh2, $filename2) = tempfile();
        write_file($filename , 'foo');
        write_file($filename2, "bar\nbaz");
        test_run(
            name => 'stdin_or_files file',
            args => {url=>"$prefix/cmdline_src_stdin_or_files_array"},
            argv => [$filename, $filename2],
            exit_code => 0,
            output_re => qr/a1=\[foo,bar\n,baz\]$/,
        );
        test_run(
            name => 'stdin_or_files file (special hint arguments passed)',
            args => {url=>"$prefix/cmdline_src_stdin_or_files_str"},
            argv => ['--json', $filename],
            exit_code => 0,
            output_re => qr/
                               "-cmdline_src_a1"\s*:\s*"stdin_or_files"
                               .+
                               "-cmdline_srcfilenames_a1"\s*:\s*\[
                           /sx,
        );
        test_run(
            name   => 'stdin_or_files file not found',
            args   => {url=>"$prefix/cmdline_src_stdin_or_files_str"},
            argv   => [$filename . "/x"],
            status => 500,
        );

        # i don't know why these tests don't work, they should though. and if
        # tested via a cmdline script like
        # examples/cmdline_src-stdin_or_files-{str,array} they work fine.
        if (0) {
            open $fh, '<', $filename2;
            local *STDIN = $fh;
            local @ARGV;
            test_run(
                name => 'stdin_or_files stdin str',
                args => {url=>"$prefix/cmdline_src_stdin_or_files_str"},
                argv => [],
                exit_code => 0,
                output_re => qr/a1=bar\nbaz$/,
            );
            # XXX test special hint arguments passed
        }
        if (0) {
            open $fh, '<', $filename2;
            local *STDIN = $fh;
            local @ARGV;
            test_run(
                name => 'stdin_or_files stdin str',
                args => {url=>"$prefix/cmdline_src_stdin_or_files_array"},
                argv => [],
                exit_code => 0,
                output_re => qr/a1=\[bar\n,baz\]/,
            );
            # XXX test special hint arguments passed
        }
    }

    # stdin
    {
        my ($fh, $filename) = tempfile();
        write_file($filename, "bar\nbaz");

        local *STDIN;

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin str',
            args => {url=>"$prefix/cmdline_src_stdin_str"},
            argv => [],
            exit_code => 0,
            output_re => qr/a1=bar\nbaz/,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin str (special hint arguments passed)',
            args => {url=>"$prefix/cmdline_src_stdin_str"},
            argv => ['--json'],
            exit_code => 0,
            output_re => qr/
                               "-cmdline_src_a1"\s*:\s*"stdin"
                           /sx,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin array',
            args => {url=>"$prefix/cmdline_src_stdin_array"},
            argv => [],
            exit_code => 0,
            output_re => qr/a1=\[bar\n,baz\]/,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin + arg set to "-"',
            args => {url=>"$prefix/cmdline_src_stdin_str"},
            argv => [qw/--a1 -/],
            exit_code => 0,
            output_re => qr/a1=bar\nbaz/,
        );

        test_run(
            name   => 'stdin + arg set to non "-"',
            args   => {url=>"$prefix/cmdline_src_stdin_str"},
            argv   => [qw/--a1 x/],
            status => 400,
        );
    }

    # stdin_line
    {
        my ($fh, $filename) = tempfile();
        write_file($filename, "foo\n");

        local *STDIN;

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin_line + from stdin',
            args => {url=>"$prefix/cmdline_src_stdin_line"},
            argv => ['--a2', 'bar'],
            exit_code => 0,
            output_re => qr/a1=foo\na2=bar/,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin_line + from stdin (special hint arguments passed)',
            args => {url=>"$prefix/cmdline_src_stdin_line"},
            argv => ['--json', '--a2', 'bar'],
            exit_code => 0,
            output_re => qr/
                               "-cmdline_src_a1"\s*:\s*"stdin_line"
                           /sx,
        );

        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'stdin_line + from cmdline',
            args => {url=>"$prefix/cmdline_src_stdin_line"},
            argv => ['--a2', 'bar', '--a1', 'qux'],
            exit_code => 0,
            output_re => qr/a1=qux\na2=bar/,
        );

        write_file($filename, "foo\nbar\n");
        open $fh, '<', $filename;
        *STDIN = $fh;
        test_run(
            name => 'multi stdin_line',
            args => {url=>"$prefix/cmdline_src_multi_stdin_line"},
            argv => ['--a3', 'baz'],
            exit_code => 0,
            output_re => qr/a1=foo\na2=bar\na3=baz/,
        );
    }

    done_testing;
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

subtest 'env' => sub {
    # XXX turned off via read_env attribute

    {
        local $ENV{SUM_NUMS_OPT} = '1 2';
        test_run(
            name      => 'turned off via --no-env',
            args      => {program_name => 'sum-nums',
                          url => '/Perinci/Examples/sum'},
            argv      => [qw/--no-env --json 3/],
            exit_code => 0,
            output_re => qr/^\[\s*200,\s*"OK",\s*3,\s*\{\}\s*\]/s,
        );
    }

    {
        local $ENV{SUM_NUMS_OPT} = '--json 1 2';
        test_run(
            name      => '--json',
            args      => {program_name => 'sum-nums',
                          url => '/Perinci/Examples/sum'},
            argv      => [qw/3/],
            exit_code => 0,
            output_re => qr/^\[\s*200,\s*"OK",\s*6,\s*\{\}\s*\]/s,
        );
    }

    {
        local $ENV{foo_opt} = '--json 1 2';
        test_run(
            name      => '--json',
            args      => {program_name => 'sum-nums',
                          env_name => 'foo_opt',
                          url => '/Perinci/Examples/sum'},
            argv      => [qw/3/],
            exit_code => 0,
            output_re => qr/^\[\s*200,\s*"OK",\s*6,\s*\{\}\s*\]/s,
        );
    }
};

subtest 'config' => sub {
    my $dir = tempdir(CLEANUP=>1);
    my $url = "/Perinci/Examples/noop2";
    write_file("$dir/prog.conf", <<'_');
a=101
b=201
[subcommand1]
a=102
c=201
[subcommand2]
a=103
[profile=profile1]
a=111
d=201
[subcommand1 profile=profile1]
a=121
_
    write_file("$dir/prog2.conf", <<'_');
a=104
_
    test_run(
        name => 'config_dirs',
        args => {
            url=>$url,
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [],
        output_re => qr/^a=101\nb=201\nc=\nd=\ne=$/,
    );
    test_run(
        name => 'config_filename',
        args => {
            url=>$url,
            program_name=>'prog',
            config_filename=>'prog2.conf',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [],
        output_re => qr/^a=104\nb=\nc=\nd=\ne=$/,
    );
    test_run(
        name => '--no-config',
        args => {
            url=>$url,
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/--no-config/],
        output_re => qr/^a=\nb=\nc=\nd=\ne=$/,
    );
    test_run(
        name => '--config-path',
        args => {
            url=>$url,
            program_name=>'prog',
            read_config=>1,
            #config_dirs=>[$dir],
        },
        argv => ['--config-path', "$dir/prog.conf"],
        output_re => qr/^a=101\nb=201\nc=\nd=\ne=$/,
    );
    test_run(
        name => '--config-profile',
        args => {
            url=>$url,
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/--config-profile=profile1/],
        output_re => qr/a=111\nb=201\nc=\nd=201\ne=$/,
    );
    test_run(
        name => 'unknown config profile -> error',
        args => {
            url=>$url,
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/--config-profile=foo/],
        status => 412,
    );
    test_run(
        name => 'unknown config profile but does not read config -> ok',
        args => {
            url=>$url,
            program_name=>'foo',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/--config-profile=bar/],
        output_re => qr/^a=\nb=\nc=\nd=\ne=$/,
    );
    {
        test_run(
            name => 'unknown config profile but set ignore_missing_config_profile_section -> ok',
            hook_before_read_config_file => sub {
                my ($self, $r) = @_;
                $r->{ignore_missing_config_profile_section} = 1;
            },
            args => {
                url=>$url,
                program_name=>'prog',
                read_config=>1,
                config_dirs=>[$dir],
            },
            argv => [qw/--config-profile=bar/],
            output_re => qr/^a=101\nb=201\nc=\nd=\ne=$/,
        );
    }
    test_run(
        name => 'subcommand',
        args => {
            subcommands => {
                subcommand1=>{url=>$url},
            },
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/subcommand1/],
        output_re => qr/^a=102\nb=201\nc=201\nd=\ne=$/,
    );
    test_run(
        name => 'subcommand + --config-profile',
        args => {
            subcommands => {
                subcommand1=>{url=>$url},
            },
            program_name=>'prog',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw/--config-profile=profile1 subcommand1/],
        output_re => qr/^a=121\nb=201\nc=201\nd=201\ne=$/,
    );

    write_file("$dir/sum.conf", <<'_');
array=0
_
    test_run(
        name => 'array-ify if argument is array',
        args => {
            url=>'/Perinci/Examples/sum',
            program_name=>'sum',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [qw//],
        exit_code => 0,
        output_re => qr/^0$/,
    );
    # TODO array-ify common option

    write_file("$dir/prog3.conf", <<'_');
format=json
naked_res=1
a.arg=101
_
    test_run(
        name => 'can also set common option',
        args => {
            url=>$url,
            program_name=>'prog3',
            read_config=>1,
            config_dirs=>[$dir],
        },
        argv => [],
        exit_code => 0,
        output_re => qr/^"a=101\\nb=\\nc=\\nd=\\ne="/,
    );
};

subtest "stream input (simple types)" => sub {
    my ($fh, $filename) = tempfile();
    write_file($filename, "one\ntwo three\nfour\n");
    test_run(
        args => {
            url => '/Perinci/Examples/Stream/wc',
        },
        argv => [$filename],
        exit_code => 0,
        output_re => qr/
                           ^chars \s+ 19\n
                           ^lines \s+ 3\n
                           ^words \s+ 4\n
                       /mx,
    );
};

subtest "stream input (json stream)" => sub {
    my ($fh, $filename) = tempfile();
    write_file($filename, qq({}\n{"a":1}\n{"b":2,"c":3}\n{"d":4}\n));
    test_run(
        args => {
            url => '/Perinci/Examples/Stream/wc_keys',
        },
        argv => [$filename],
        exit_code => 0,
        output_re => qr/
                           ^keys \s+ 4\n
                       /mx,
    );

    write_file($filename, qq({}\n{\n));
    test_run(
        name => 'records need to be json',
        args => {
            url => '/Perinci/Examples/Stream/wc_keys',
        },
        argv => [$filename],
        exit_code => 200,
    );
};

subtest "stream output (simple types)" => sub {
    my ($fh, $filename) = tempfile();
    write_file($filename, qq(1\n3\n5\n));
    test_run(
        args => {
            url => '/Perinci/Examples/Stream/square_input',
        },
        argv => [$filename],
        exit_code => 0,
        output_re => qr/
                           ^1\n
                           ^9\n
                           ^25\n
                       /mx,
    );
};

subtest "stream output (json stream)" => sub {
    test_run(
        args => {
            url => '/Perinci/Examples/Stream/hash_stream',
        },
        argv => [qw/-n 3/],
        exit_code => 0,
        output_re => qr/
                           ^\Q{"num":1}\E\n
                           ^\Q{"num":2}\E\n
                           ^\Q{"num":3}\E\n
                       /mx,
    );
};

# XXX test logging

DONE_TESTING:
done_testing;
