package Perinci::CmdLine::Plugin::Run::Normal;

# put pragmas + Log::ger here
use strict;
use warnings;
use Log::ger;
use parent 'Perinci::CmdLine::PluginBase';

# put other modules alphabetically here
use IO::Interactive qw(is_interactive);

# put global variables alphabetically here
# AUTHORITY
# DATE
# DIST
# VERSION

sub meta {
    return {
        summary => 'Normal run',
        description => <<'_',

This plugin is included by default, run at the `run` event for a normal run.

_
        conf => {
        },
        prio => 50, # normal
        tags => ['category:run-handler'],
    };
}

sub on_run {
    my ($self, $r) = @_;

    my $co = $r->{common_opts};

    # set default from common options
    $r->{naked_res} = $co->{naked_res}{default} if $co->{naked_res};
    $r->{format}    = $co->{format}{default} if $co->{format};

    # EXPERIMENTAL, set default format to json if we are running in a pipeline
    # and the right side of the pipe is the 'td' program
    {
        last if is_interactive(*STDOUT) || $r->{format};
        last unless eval { require Pipe::Find; 1 };
        my $pipeinfo = Pipe::Find::get_stdout_pipe_process();
        last unless $pipeinfo;
        last unless $pipeinfo->{exe} =~ m![/\\]td\z! ||
            $pipeinfo->{cmdline} =~ m!\A([^\0]*[/\\])?perl\0([^\0]*[/\\])?td\0!;
        $r->{format} = 'json';
    }

    $r->{format} //= $self->cmdline->default_format;

    if ($self->cmdline->read_config) {
        # note that we will be reading config file
        $r->{read_config} = 1;
    }

    if ($self->cmdline->read_env) {
        # note that we will be reading env for default options
        $r->{read_env} = 1;
    }

    eval {
        log_trace("[pericmd] Running hook_before_parse_argv ...");
        #$self->cmdline->hook_before_parse_argv($r);

        my $parse_res = $self->cmdline->parse_argv($r);
        if ($parse_res->[0] == 501) {
            # we'll need to send ARGV to the server, because it's impossible to
            # get args from ARGV (e.g. there's a cmdline_alias with CODE, which
            # has been transformed into string when crossing network boundary)
            $r->{send_argv} = 1;
        } elsif ($parse_res->[0] != 200) {
            die $parse_res;
        }
        $r->{parse_argv_res} = $parse_res;
        $r->{args} = $parse_res->[2] // {};

        # set defaults
        $r->{action} //= 'call';

        # init logging
        if ($self->cmdline->log) {
            require Log::ger::App;
            my $default_level = do {
                my $dry_run = $r->{dry_run} // $self->cmdline->default_dry_run;
                $dry_run ? 'info' : 'warn';
            };
            Log::ger::App->import(
                level => $r->{log_level} // $self->cmdline->log_level // $default_level,
                name  => $self->cmdline->program_name,
            );
        }

        log_trace("[pericmd] Running hook_after_parse_argv ...");
        #$self->cmdline->hook_after_parse_argv($r);

        $self->cmdline->parse_cmdline_src($r);

        #log_trace("TMP: parse_res: %s", $parse_res);

        my $missing = $parse_res->[3]{"func.missing_args"};
        die [400, "Missing required argument(s): ".join(", ", @$missing)]
            if $missing && @$missing;

        my $scd = $r->{subcommand_data};
        if ($scd->{pass_cmdline_object} // $self->cmdline->pass_cmdline_object) {
            $r->{args}{-cmdline} = $self->cmdline;
            $r->{args}{-cmdline_r} = $r;
        }

        $self->cmdline->_plugin_run_event(
            name => 'action',
            req_handler => 1,
        );
    };

    my $err = $@;
    if ($err || !$r->{res}) {
        if ($err) {
            $err = [500, "Died: $err"] unless ref($err) eq 'ARRAY';
            if (%Devel::Confess::) {
                no warnings 'once';
                require Scalar::Util;
                my $id = Scalar::Util::refaddr($err);
                my $stack_trace = $Devel::Confess::MESSAGES{$id};
                $err->[1] .= "\n$stack_trace" if $stack_trace;
            }
            $err->[1] =~ s/\n+$//;
            $r->{res} = $err;
        } else {
            $r->{res} = [500, "Bug: no response produced"];
        }
    } elsif (ref($r->{res}) ne 'ARRAY') {
        log_trace("[pericmd] res=%s", $r->{res}); #2
        $r->{res} = [500, "Bug in program: result not an array"];
    }

    if (!$r->{res}[0] || $r->{res}[0] < 200 || $r->{res}[0] > 555) {
        $r->{res}[3]{'x.orig_status'} = $r->{res}[0];
        $r->{res}[0] = 555;
    }

    $r->{res}[3]{title} //= join(
        " ",
        $self->cmdline->program_name,
        @{ $r->{orig_argv} // \@ARGV },
    );

    $r->{format} //= $r->{res}[3]{'cmdline.default_format'};
    $r->{format} //= $r->{meta}{'cmdline.default_format'};
    my $restore_orig_result;
    my $orig_result;
    if (exists $r->{res}[3]{'cmdline.result.noninteractive'} && !is_interactive(*STDOUT)) {
        $restore_orig_result = 1;
        $orig_result = $r->{res}[2];
        $r->{res}[2] = $r->{res}[3]{'cmdline.result.noninteractive'};
    } elsif (exists $r->{res}[3]{'cmdline.result.interactive'} && is_interactive(*STDOUT)) {
        $restore_orig_result = 1;
        $orig_result = $r->{res}[2];
        $r->{res}[2] = $r->{res}[3]{'cmdline.result.interactive'};
    } elsif (exists $r->{res}[3]{'cmdline.result'}) {
        $restore_orig_result = 1;
        $orig_result = $r->{res}[2];
        $r->{res}[2] = $r->{res}[3]{'cmdline.result'};
    }

  FORMAT:
    $self->cmdline->_format($r);

    if ($restore_orig_result) {
        $r->{res}[2] = $orig_result;
    }

    my $exitcode;
    if ($r->{res}[3] && defined($r->{res}[3]{'cmdline.exit_code'})) {
        $exitcode = $r->{res}[3]{'cmdline.exit_code'};
    } else {
        $exitcode = $self->cmdline->status2exitcode($r->{res}[0]);
    }
    if ($self->cmdline->exit) {
        log_trace("[pericmd] exit(%s)", $exitcode);
        exit $exitcode;
    } else {
        # so this can be tested
        $r->{res}[3]{'x.perinci.cmdline.base.exit_code'} = $exitcode;
    }

    $self->cmdline->_unsetup_progress_output;

    [201];
}

1;
# ABSTRACT:

=for Pod::Coverage ^(.+)$

=head1 DESCRIPTION

A C<Run::> plugin is the main plugin that runs at the C<run> event, which is
fired by Perinci::CmdLine's C<run()> method.

Multiple C<Run::*> plugins can be registered at the C<run> event, but only one
will actually run because they return C<201> code which instruct
Perinci::CmdLine to end the event early.

The C<Run::Normal> plugin (this plugin) is the plugin run at normal mode. It
calls the designated Riap function with arguments from user's command-line
arguments (and/or configuration file, and/or environment variable), and then
display the return value. However, instead of calling the function, there are
also other alternative actions that can be performed instead like C<help>,
C<meta>, etc.
