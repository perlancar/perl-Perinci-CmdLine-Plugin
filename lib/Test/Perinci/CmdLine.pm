package Test::Perinci::CmdLine;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;

use Capture::Tiny qw(capture);
use Test::More 0.98;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(test_run test_complete);

our $CLASS = "Perinci::CmdLine";

sub test_run {
    my %args = @_;

    my $cmd = $CLASS->new(%{$args{args}}, exit=>0);

    local @ARGV = @{$args{argv} // []};
    my ($stdout, $stderr);
    my $res;
    eval {
        ($stdout, $stderr) = capture {
            $res = $cmd->run;
        };
    };
    my $eval_err = $@;
    my $exit_code = $res->[3]{'x.perinci.cmdline.base.exit_code'};

    my $name = "test_run: " . ($args{name} // join(" ", @{$args{argv} // []}));

    subtest $name => sub {
        if ($args{dies}) {
            ok($eval_err || ref($eval_err), "dies");
            return;
        } else {
            ok(!$eval_err, "doesn't die") or diag("dies: $eval_err");
        }

        if (defined $args{exit_code}) {
            is($exit_code, $args{exit_code}, "exit code");
        }

        if ($args{status}) {
            is($res->[0], $args{status}, "status");
        }

        if ($args{output_re}) {
            like($stdout // "", $args{output_re}, "output_re")
                or diag("output is <" . ($stdout // "") . ">");
        }

        if ($args{posttest}) {
            $args{posttest}->(\@ARGV, $stdout, $stderr, $res);
        }
    };
}

sub test_complete {
    my (%args) = @_;

    my $cmd = $CLASS->new(%{$args{args}}, exit=>0);

    local @ARGV = @{$args{argv} // []};

    # $args{comp_line0} contains comp_line with '^' indicating where comp_point
    # should be, the caret will be stripped. this is more convenient than
    # counting comp_point manually.
    my $comp_line  = $args{comp_line0};
    defined ($comp_line) or die "BUG: comp_line0 not defined";
    my $comp_point = index($comp_line, '^');
    $comp_point >= 0 or
        die "BUG: comp_line0 should contain ^ to indicate where comp_point is";
    $comp_point =~ s/\^//;

    local $ENV{COMP_LINE}  = $comp_line;
    local $ENV{COMP_POINT} = $comp_point;

    my ($stdout, $stderr);
    my $res;
    ($stdout, $stderr) = capture {
        $res = $cmd->run;
    };
    my $exit_code = $res->[3]{'x.perinci.cmdline.base.exit_code'};

    my $name = "test_complete: " . ($args{name} // $args{comp_line0});
    subtest $name => sub {
        is($exit_code, 0, "exit code = 0");
        is($stdout // "", join("", map {"$_\n"} @{$args{result}}), "result");
    };
}

1;
# ABSTRACT: Test library for Perinci::CmdLine{,::Lite}

=head1 FUNCTIONS

=head2 test_run(%args)

=head2 test_complete(%args)
