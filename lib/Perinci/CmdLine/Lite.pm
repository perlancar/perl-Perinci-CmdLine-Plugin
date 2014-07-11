package Perinci::CmdLine::Lite;

use 5.010001;


# DATE
# VERSION

use Mo;
extends 'Perinci::CmdLine::Base';

# when base class has errors, we need to use this to get meaningful error
#use parent 'Perinci::CmdLine::Base';

# compared to pericmd, i want to avoid using internal attributes like
# $self->{_format}, $self->{_res}, etc.

sub BUILD {
    my ($self, $args) = @_;

    if (!$self->{actions}) {
        $self->{actions} = {
            call => {},
            version => {},
            subcommands => {},
            help => {},
        };
    }

    if (!$self->{common_opts}) {
        my $co = {
            version => {
                getopt  => 'version|v',
                summary => 'Show program version',
                handler => sub { $self->run_version; exit 0 },
            },
            help => {
                getopt  => 'help|h|?',
                summary => 'Show help message',
                handler => sub { $self->run_help; exit 0 },
            },
            format => {
                getopt  => 'format=s',
                summary => 'Set output format (text/text-simple/text-pretty/json/json-pretty)',
                handler => sub { $self->{format} = $_[1] },
            },
            json => {
                getopt  => 'json',
                summary => 'Set output format to json',
                handler => sub { $self->{format} = 'json' },
            },
        };
        if ($self->subcommands) {
            $co->{subcommands} = {
                getopt  => 'json',
                summary => 'Set output format to json',
                handler => sub { $self->run_subcommands; exit 0 },
            };
        }
        $self->{common_opts} = $co;
    }

    $self->{formats} //= [qw/text text-simple text-pretty json/];
}

sub format_result {
    my ($self, $res, $format, $meta) = @_;
    if ($format =~ /\Atext(-simple|-pretty)?\z/) {
        my $is_pretty = $format eq 'text-pretty' ? 1 :
            $format eq 'text-simple' ? 0 : (-t STDOUT);
        no warnings 'uninitialized';
        if ($res->[0] != 200) {
            return "ERROR $res->[0]: $res->[1]\n";
        } else {
            require Data::Check::Structure;
            my $data = $res->[2];
            my $max = 5;
            if (!ref($data)) {
                $data //= "";
                $data .= "\n" unless $data =~ /\n\z/;
                return $data;
            } elsif (Data::Check::Structure::is_aos($data, {max=>$max})) {
                if ($is_pretty) {
                    require Text::Table::Tiny;
                    $data = [map {[$_]} @$data];
                    return Text::Table::Tiny::table(rows=>$data) . "\n";
                } else {
                    return join("", map {"$_\n"} @$data);
                }
            } elsif (Data::Check::Structure::is_aoaos($data, {max=>$max})) {
                if ($is_pretty) {
                    require Text::Table::Tiny;
                    return Text::Table::Tiny::table(rows=>$data) . "\n";
                } else {
                    return join("", map {join("\t", @$_)."\n"} @$data);
                }
            } elsif (Data::Check::Structure::is_hos($data, {max=>$max})) {
                if ($is_pretty) {
                    require Text::Table::Tiny;
                    $data = [map {[$_, $data->{$_}]} sort keys %$data];
                    unshift @$data, ["key", "value"];
                    return Text::Table::Tiny::table(rows=>$data) . "\n";
                } else {
                    return join("", map {"$_\t$data->{$_}\n"} sort keys %$data);
                }
            } elsif (Data::Check::Structure::is_aohos($data, {max=>$max})) {
                # collect all mentioned fields
                my %fieldnames;
                for my $row (@$data) {
                    $fieldnames{$_}++ for keys %$row;
                }
                my @fieldnames = sort keys %fieldnames;
                my $newdata = [];
                for my $row (@$data) {
                    push @$newdata, [map {$row->{$_}} @fieldnames];
                }
                if ($is_pretty) {
                    unshift @$newdata, \@fieldnames;
                    require Text::Table::Tiny;
                    return Text::Table::Tiny::table(rows=>$newdata) . "\n";
                } else {
                    return join("", map {join("\t", @$_)."\n"} @$newdata);
                }
            } else {
                $format = 'json-pretty';
            }
        }
    }

    warn "Unknown format '$format', fallback to json-pretty"
        unless $format =~ /\Ajson(-pretty)?\z/;
    state $cleanser = do {
        require Data::Clean::JSON;
        Data::Clean::JSON->get_cleanser;
    };
    $cleanser->clean_in_place($res);
    state $json = do {
        require JSON;
        JSON->new->allow_nonref;
    };
    if ($format eq 'json') {
        return $json->encode($res);
    } else {
        return $json->pretty->encode($res);
    }
}

sub display_result {
    my ($self, $res, $fres) = @_;
    print $fres;
}

sub run_subcommands {
    my ($self) = @_;

    if (!$self->subcommands) {
        say "There are no subcommands.";
        return 0;
    }

    say "Available subcommands:";
    my $subcommands = $self->list_subcommands;
    for (@$subcommands) {
        say "  $_->{name} $_->{url}";
    }
    0;
}

sub _get_meta {
    my ($self, $url) = @_;
    $url =~ m!\A(?:pl:)?/(\w+(?:/\w+)*)/(\w+)\z!
        or die [500, "Unsupported/bad URL '$url'"];
    my ($mod, $func) = ($1, $2);
    require "$mod.pm";
    $mod =~ s!/!::!g;
    no strict 'refs';
    require Perinci::Sub::Normalize;
    Perinci::Sub::Normalize::normalize_function_metadata(
        ${"$mod\::SPEC"}{$func});
}

# XXX
sub run_version {
    my ($self) = @_;

    my $url = $self->{_subcommand} && $self->{_subcommand}{url} ?
        $self->{_subcommand}{url} : $self->url;
    my $res = $self->_pa->request(meta => $url);
    my ($ver, $date);
    if ($res->[0] == 200) {
        $ver = $res->[2]{entity_v} // "?";
        $date = $res->[2]{entity_date};
    } else {
        #$log->warnf("Can't request 'meta' action on %s: %d - %s",
        #            $url, $res->[0], $res->[1]);
        $ver = '?';
        $date = undef;
    }

    say __x(
        "{program} version {version}",
        program => $self->_color('program_name',
                                 $self->_program_and_subcommand_name),
        version => $self->_color('emphasis', $ver)) .
            ($date ? " ($date)" : "");
    {
        no strict 'refs';
        say "  ", __x(
            "{program} version {version}",
            program => $self->_color('emphasis', "Perinci::CmdLine"),
            version => $self->_color('emphasis',
                                     $Perinci::CmdLine::VERSION || "dev"))
            . ($Perinci::CmdLine::DATE ? " ($Perinci::CmdLine::DATE)" : "");
    }

    0;
}

sub hook_after_get_meta {
    require Perinci::Object;

    my ($self, $meta) = @_;

    if (Perinci::Object::risub($meta)->can_dry_run) {
        $self->common_opts->{dry_run} = {
            getopt  => 'dry-run',
            summary => "Run in simulation mode (also via DRY_RUN=1)",
            handler => sub {
                $self->{_dry_run} = 1;
                #$ENV{VERBOSE} = 1;
            },
        };
    }
}

# XXX
sub run_help {
    my ($self) = @_;

    say "Help message";
    0;
}

sub run_call {
    my ($self) = @_;
    my $sc = $self->{_subcommand};
    my %fargs = %{$self->{_args} // {}};
    $fargs{-cmdline} = $self if $sc->{pass_cmdline_object} //
        $self->pass_cmdline_object;

    my $tx_id;

    my $dry_run = $self->{_dry_run};
    my $using_tx = !$dry_run && $self->undo && ($sc->{undo} // 1);

    # currently we don't attempt to insert tx_id or dry_run when using argv,
    # we'll just give up
    if ($self->{_send_argv} && ($dry_run || $using_tx)) {
        my $res = $self->{_getargs_result};
        $self->_err("Failed parsing arguments (2): $res->[0] - $res->[1]");
    }

    # call function
    if ($self->{_send_argv}) {
        $self->{_res} = $self->_pa->request(
            call => $self->{_subcommand}{url},
            {argv=>$self->{_orig_argv}}, # XXX tx_id, dry_run (see above)
        );
    } else {
        #$log->tracef("Calling function via _pa with arguments: %s", \%fargs);
        $self->{_res} = $self->_pa->request(
            call => $self->{_subcommand}{url},
            {args=>\%fargs, tx_id=>$tx_id, dry_run=>$dry_run});
    }
    #$log->tracef("call res=%s", $self->{_res});

    my $resmeta = $self->{_res}[3] // {};
    if (defined $resmeta->{"cmdline.exit_code"}) {
        return $resmeta->{"cmdline.exit_code"};
    } else {
        return $self->{_res}[0] =~ /\A(?:200|304)\z/ ?
            0 : $self->{_res}[0] - 300;
    }
}

sub hook_before_run {}

sub hook_after_run {}

1;
# ABSTRACT: A lightweight Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(hook_.+|)$

=head1 SYNOPSIS

In your command-line script:

 #!/usr/bin/perl
 use Perinci::CmdLine::Lite; # or Perinci::CmdLine::Any

 our %SPEC;
 $SPEC{foo} = {
     v => 1.1,
     summary => 'Does foo to your computer',
     args => {
         bar => {
             summary=>'Barrr',
             req=>1,
             schema=>['str*', {in=>[qw/aa bb cc/]}],
         },
         baz => {
             summary=>'Bazzz',
             schema=>'str',
         },
     },
 };
 sub foo {
     my %args = @_;
     $log->debugf("Arguments are %s", \%args);
     [200, "OK", $args{bar} . ($args{baz} ? "and $args{baz}" : "")];
 }

 Perinci::CmdLine::Lite->new(url => '/main/foo')->run;

To run this program:

 % foo --help ;# display help message
 % foo --version ;# display version
 % foo --bar aa ;# run function and display the result
 % foo --baz x  ;# fail because required argument 'bar' not specified

To do bash tab completion:

 % complete -C foo foo ;# can be put in ~/.bashrc
 % foo <tab> ;# completes to --help, --version, --bar, --baz and others
 % foo --b<tab> ;# completes to --bar and --baz
 % foo --bar <tab> ;# completes to aa, bb, cc


=head1 DESCRIPTION

B<NOTE: This module is still experimental.>

Perinci::CmdLine::Lite (hereby PCLite) module offers a lightweight (low startup
overhead, minimal dependencies) alternative to L<Perinci::CmdLine> (hereby PC).
It offers a subset of functionality and a pretty compatible API. The main
difference is that, to keep dependencies minimal and startup overhead small,
PCLite does not access code and metadata through the L<Riap> client library
L<Perinci::Access> layer, but instead accesses Perl modules/packages directly.
This means B<no remote URL support>, you can only access Perl modules on the
filesystem. Below is summary of the differences:

=over

=item * As mentioned above, no remote URL support

Also, no automatic validation from schema, as this currently adds some startup
overhead.

=item * PCLite starts much faster

The target is under 0.05-0.1s, while PC can start between 0.2-0.5s.

=item * PCLite does not support color themes

=item * PCLite does not support undo

=item * PCLite does not support logging

Something more lightweight than L<Log::Any::App> will be considered. If you want
logging, you can do something like this:

 % DEBUG=1 PERL5OPT=-MLog::Any::App app.pl

=item * PCLite does not support progress indicator

=item * PCLite does not support I18N

=item * PCLite does not yet support these Rinci function metadata properties

 x.perinci.cmdline.default_format

=item * PCLite does not yet support these Rinci function argument specification properties

 cmdline_src

=item * PCLite does not yet support these Rinci result metadata properties/attributes

 is_stream
 cmdline.display_result
 cmdline.page_result
 cmdline.pager
 cmdline.exit_code

=item * PCLite uses simpler formatting

Instead of L<Perinci::Result::Format> (especially the 'text' formats which use
L<Data::Format::Pretty::Console> and L<Text::ANSITable>), PCLite uses the
following simple rules that work for a significant portion of common data
structures:

1) if result is undef, print nothing.

2) if result is scalar, print it.

3) if result is an array of scalars (check at most 5 first rows), print it one
line for each element.

4) if result is a hash of scalars (check at most 5 keys), print a two column
table, first column is key and second column is value. Keys will be sorted.

5) if result is an array of hashes of scalars (check at most 5 elements), print
as table.

6) if result is an array of arrays of scalars (check at most 5 elements), print
as table.

7) otherwise print as JSON (after cleaning it with L<Data::Clean::JSON>).

YAML and the other formats are not supported.

Table is printed using the more lightweight and faster L<Text::TabularDisplay>.

=item * PCLite does not yet support these environment variables

 PERINCI_CMDLINE_COLOR_THEME
 PERINCI_CMDLINE_SERVER
 PROGRESS
 PAGER
 COLOR
 UTF8

 DEBUG, VERBOSE, QUIET, TRACE, and so on

=item * In passing command-line object to functions, PCLite object is passed

Some functions might expect a L<Perinci::CmdLine> instance.

=back


=head1 ENVIRONMENT

=over

=item * PERINCI_CMDLINE_PROGRAM_NAME => STR

Can be used to set CLI program name.

=back


=head1 SEE ALSO

L<Perinci::CmdLine>

L<Perinci::CmdLine::Any>

=cut
