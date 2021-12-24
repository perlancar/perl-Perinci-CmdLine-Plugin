package Perinci::CmdLine::Plugin::FormatResult::Default;

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
        summary => 'The default result formatter plugin',
        description => <<'_',

This plugin uses <pm:Perinci::Result::Format::Lite> to format result.

This plugin is included by default.

A `format_result` plugin should store the formatted result in the `fres` result
stash key for use by other plugins.

_
        conf => {
        },
        prio => 50, # normal
        tags => ['category:result-formatter'],
    };
}

sub on_format_result {
    require Perinci::Result::Format::Lite;

    my ($self, $r) = @_;

    my $fmt = $r->{format} // 'text';

    if ($fmt eq 'html+datatables') {
        $fmt = 'text-pretty';
        $ENV{VIEW_RESULT} //= 1;
        no warnings 'once';
        $Perinci::CmdLine::Plugin::tempfile_opt_suffix = '.html';
        $ENV{FORMAT_PRETTY_TABLE_BACKEND} //= 'Text::Table::HTML::DataTables';
    } elsif ($fmt eq 'termtable') {
        $fmt = 'text-pretty';
        no warnings 'once';
        $ENV{FORMAT_PRETTY_TABLE_BACKEND} //= 'Term::TablePrint';
    }

    my $fres = Perinci::Result::Format::Lite::format(
        $r->{res}, $fmt, $r->{naked_res}, $self->cmdline->{use_cleanser});

    # ux: prefix error message with program name
    if ($fmt =~ /text/ && $r->{res}[0] =~ /\A[45]/ && defined($r->{res}[1])) {
        $fres = $self->cmdline->program_name . ": $fres";
    }

    $r->{fres} = $fres;

    [200];
}

1;
# ABSTRACT:

=head1 SEE ALSO
