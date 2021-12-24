package Perinci::CmdLine::Plugin::DisplayResult::Default;

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
        summary => 'The default result displayer plugin',
        description => <<'_',

_
        conf => {
        },
        prio => 50, # normal
        tags => ['category:result-displayer'],
    };
}

sub on_display_result {
    require Data::Sah::Util::Type;

    my ($self, $r) = @_;

    my $res  = $r->{res};
    my $meta = $r->{meta};
    my $fres = $r->{fres};
    my $resmeta = $res->[3] // {};
    my $handle = $r->{output_handle};

    my $layer;
  SELECT_LAYER:
    {
        if ($resmeta->{'x.hint.result_binary'}) {
            # XXX only when format is text?
            $layer = ":bytes"; last;
        }

        if ($ENV{UTF8} ||
                defined($r->{subcommand_data} && $r->{subcommand_data}{use_utf8}) ||
                $self->cmdline->use_utf8) {
            $layer = ":encoding(utf8)"; last;
        }

        if ($self->cmdline->use_locale) {
            $layer = ":locale"; last;
        }

    }
    binmode($handle, $layer) if $layer;

    my $sch = $meta->{result}{schema} // $resmeta->{schema};
    my $type = Data::Sah::Util::Type::get_type($sch) // '';

    if ($resmeta->{stream} // $meta->{result}{stream}) {
        my $x = $res->[2];
        if (ref($x) eq 'CODE') {
            if (Data::Sah::Util::Type::is_simple($sch)) {
                while (defined(my $l = $x->())) {
                    print $l;
                    print "\n" unless $type eq 'buf';
                }
            } else {
                require JSON::MaybeXS;
                state $json = JSON::MaybeXS->new->allow_nonref;
                if ($self->cmdline->use_cleanser) {
                    while (defined(my $rec = $x->())) {
                        print $json->encode(
                            $self->cmdline->cleanser->clone_and_clean($rec)), "\n";
                    }
                } else {
                    while (defined(my $rec = $x->())) {
                        print $json->encode($rec), "\n";
                    }
                }
            }
        } else {
            die "Result is a stream but no coderef provided";
        }
    } else {
        # do preprocessing based on content_type. should probably be moved
        # elsewhere later.
      PREPROCESS_RESULT: {
            last unless defined $r->{viewer};

            my $ct = $resmeta->{content_type} // '';
            if ($ct eq 'text/x-org') {
                $fres = "# -*- mode: org -*-\n" . $fres;
            }
        }

        print $handle $fres;
        if (defined $r->{viewer}) {
            require ShellQuote::Any::Tiny;
            my $cmd = $r->{viewer} ." ". ShellQuote::Any::Tiny::shell_quote($r->{viewer_temp_path});
            system $cmd;
        }
    }

    [200];
}

1;
# ABSTRACT:

=head1 SEE ALSO
