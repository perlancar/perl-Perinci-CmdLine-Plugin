package Perinci::CmdLine::Plugin::Run::DumpObject;

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
        summary => 'Dump object mode',
        description => <<'_',

This plugin is included by default at a high priority and activated if the
PERINCI_CMDLINE_DUMP_OBJECT environmnent is true.

_
        conf => {
        },
        prio => 10, # high
        tags => ['category:run-handler', 'category:debugging'],
    };
}

sub on_run {
    my ($self, $r) = @_;

    return [100] unless $ENV{PERINCI_CMDLINE_DUMP_OBJECT};

    require Data::Dump;

    local $r->{in_dump_object} = 1;

    # check whether subcommand is defined. try to search from --cmd, first
    # command-line argument, or default_subcommand.
    $self->cmdline->hook_before_parse_argv($r);
    $self->cmdline->_parse_argv1($r);

    if ($r->{read_env}) {
        my $env_words = $self->cmdline->_read_env($r);
        unshift @ARGV, @$env_words;
    }

    my $scd = $r->{subcommand_data};
    # we do get_meta() currently because some common option like dry_run is
    # added in hook_after_get_meta().
    my $meta = $self->cmdline->get_meta($r, $scd->{url} // $self->cmdline->{url});

    # additional information, because scripts often put their metadata in 'main'
    # package
    {
        no warnings 'once';
        $self->cmdline->{'x.main.spec'} = \%main::SPEC;
    }

    my $label = $ENV{PERINCI_CMDLINE_DUMP_OBJECT};
    my $dump = join(
        "",
        "# BEGIN DUMP $label\n",
        Data::Dump::dump($self->cmdline), "\n",
        "# END DUMP $label\n",
    );

    $r->{res} = [
        200, "OK", $dump,
        {
            stream => 0,
            "cmdline.skip_format" => 1,
        },
    ];

    $self->cmdline->_format($r);

    [201, "OK"]; # skip the rest of the event handlers
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

The C<Run::DumpObject> plugin handler first check if the
PERINCI_CMDLINE_DUMP_OBJECT variable is set to a true value (containing some
label e.g. foo). If not, then the handler declines.

The handler then dumps the main Perinci::CmdLine object and exits.

This mode can be used by tools like L<shcompgen> to extract the command-line
options.


=head1 ENVIRONMENT

=head2 PERINCI_CMDLINE_DUMP_OBJECT
