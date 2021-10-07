package Perinci::CmdLine::Plugin::Run::DebugCompletion;

# put pragmas + Log::ger here
use strict;
use warnings;
use Log::ger;
use parent 'Perinci::CmdLine::PluginBase';

# put other modules alphabetically here

# put global variables alphabetically here
# AUTHORITY
# DATE
# DIST
# VERSION

sub meta {
    return {
        summary => 'Debug completion',
        description => <<'_',

This plugin will make your script log tab completion information to a log file
then exit instead of doing a normal run. It can be used to debug tab completion
issues. An example of log line it produces:

    [/path/to/your-script] [prog PROGNAME] [pid 12345] [uid 1000] COMP_LINE=<your-script > (%d char(s)) COMP_POINT=<%s>\n",

This plugin is not included by default. To activate this plugin from the
command-line of bash shell:

    % PERINCI_CMDLINE_PLUGINS="-Run::DebugCompletion" your-script ...

By default it logs to `/tmp/pericmd-completion.log`. To customize the log file
location:

    % PERINCI_CMDLINE_PLUGINS="-Run::DebugCompletion,log_file,/path/to/log.file" your-script ...

This plugin runs at the C<run> event at a very high priority (1) then skips
all the other run handlers (return 201 status).

_
        conf => {
            log_file => {
                summary => 'Location of log file',
                schema => 'filename*',
                description => <<'_',

If not specified, will use `/tmp/pericmd-completion.log`.

_
            },
        },
        prio => 1, # very high
        tags => ['category:run-handler', 'category:debugging'],
    };
}

sub on_run {
    my ($self, $r) = @_;

    my $log_file = $self->{log_file};
    unless (defined $log_file) {
        require File::Spec;
        my $tmpdir = File::Spec->tmpdir;
        $log_file = File::Spec->catfile($tmpdir, "pericmd-completion.log");
    }

  LOG: {
        open my $fh, ">>", $log_file
            or do { warn "Can't open completion log file, skipped: $!"; last };
        print $fh sprintf(
            "[%s] [prog %s] [pid %d] [uid %d] COMP_LINE=<%s> (%d char(s)) COMP_POINT=<%s>\n",
            scalar(localtime),
            $0,
            $$,
            $>,
            $ENV{COMP_LINE},
            length($ENV{COMP_LINE}),
            $ENV{COMP_POINT},
        );
        print $fh join("", map {"  $_=$ENV{$_}\n"} sort keys %ENV);
        close $fh;
    }

    [201, "OK"]; # skip the rest of the event handlers
}

1;
# ABSTRACT:

=for Pod::Coverage ^(.+)$

=cut
