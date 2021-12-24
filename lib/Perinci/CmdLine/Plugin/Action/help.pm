package Perinci::CmdLine::Plugin::Action::help;

use 5.010001;
use strict;
use warnings;
use Log::ger;
use parent 'Perinci::CmdLine::PluginBase';

# AUTHORITY
# DATE
# DIST
# VERSION

sub meta {
    return {
        summary => 'Handle the "help" action',
        conf => {
        },
        prio => 50, # normal
        tags => ['category:action-handler'],
    };
}

sub on_action {
    require Perinci::CmdLine::Help;

    my ($self, $r) = @_;

    my @help;
    my $scn    = $r->{subcommand_name};
    my $scd    = $r->{subcommand_data};

    my $meta = $self->cmdline->get_meta($r, $scd->{url} // $self->cmdline->{url});

    # XXX use 'delete local' when we bump minimal perl to 5.12
    my $common_opts = { %{$self->cmdline->common_opts} };

    # hide usage '--subcommands' if we have subcommands but user has specified a
    # subcommand to use
    my $has_sc_no_sc = $self->cmdline->subcommands &&
        !length($r->{subcommand_name} // '');
    delete $common_opts->{subcommands} if $self->cmdline->subcommands && !$has_sc_no_sc;

    my $res = Perinci::CmdLine::Help::gen_help(
        program_name => $self->cmdline->get_program_and_subcommand_name($r),
        program_summary => ($scd ? $scd->{summary}:undef ) // $meta->{summary},
        program_description => $scd ? $scd->{description} : undef,
        meta => $meta,
        meta_is_normalized => 1,
        subcommands => $has_sc_no_sc ? $self->cmdline->list_subcommands : undef,
        common_opts => $common_opts,
        per_arg_json => $self->cmdline->per_arg_json,
        per_arg_yaml => $self->cmdline->per_arg_yaml,
    );

    $res->[3]{"cmdline.skip_format"} = 1;

    $r->{res} = $res;

    [200];
}

1;
# ABSTRACT:

=head1 SEE ALSO
