package Perinci::CmdLine::Lite;

# DATE
# VERSION

use 5.010001;

#use Mo; extends 'Perinci::CmdLine::Base';

# when debugging, use this instead of the above because Mo doesn't give clear
# error message if base class has errors.
use parent 'Perinci::CmdLine::Base';

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
                handler => sub { $self->selected_action('version') },
            },
            help => {
                getopt  => 'help|h|?',
                summary => 'Show help message',
                handler => sub { $self->selected_action('help') },
            },
            format => {
                getopt  => 'format=s',
                summary => 'Set output format (text/text-simple/text-pretty/json/json-pretty)',
                handler => sub { $self->selected_format($_[1]) },
            },
            json => {
                getopt  => 'json',
                summary => 'Set output format to json',
                handler => sub { $self->selected_format('json') },
            },
        };
        if ($self->subcommands) {
            $co->{subcommands} = {
                getopt  => 'subcommands',
                summary => 'List available subcommands',
                handler => sub { $self->selected_action('subcommands') },
            };
        }
        if ($self->default_subcommand) {
            $co->{cmd} = {
                getopt  => 'cmd=s',
                summary => 'Select subcommand',
                handler => sub { $self->select_subcommand($_[1]) },
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
    my ($self, $args, $meta) = @_;

    if (!$self->subcommands) {
        say "There are no subcommands.";
        return 0;
    }

    say "Available subcommands:";
    my $subcommands = $self->list_subcommands;
    [200, "OK",
     join("",
          (map { "  $_->{name} $_->{url}" } @$subcommands),
      )];
}

sub run_version {
    my ($self, $args, $meta) = @_;

    [200, "OK",
     join("",
          $self->get_program_and_subcommand_name,
          " version ", ($meta->{entity_v} // "?"),
          ($meta->{entity_date} ? " ($meta->{entity_date})" : ''),
          "\n",
          "  ", __PACKAGE__,
          " version ", ($Perinci::CmdLine::Lite::VERSION // "?"),
          ($Perinci::CmdLine::Lite::DATE ? " ($Perinci::CmdLine::Lite::DATE)":''),
      )];
}

sub __require_url {
    my ($url) = @_;

    $url =~ m!\A(?:pl:)?/(\w+(?:/\w+)*)/(\w*)\z!
        or die [500, "Unsupported/bad URL '$url'"];
    my ($mod, $func) = ($1, $2);
    require "$mod.pm";
    $mod =~ s!/!::!g;
    ($mod, $func);
}

sub get_meta {
    my ($self, $url) = @_;

    my ($mod, $func) = __require_url($url);

    my $meta;
    {
        no strict 'refs';
        if (length $func) {
            $meta = ${"$mod\::SPEC"}{$func}
                or die [500, "No metadata for '$url'"];
        } else {
            $meta = ${"$mod\::SPEC"}{':package'} // {v=>1.1};
        }
        $meta->{entity_v}    //= ${"$mod\::VERSION"};
        $meta->{entity_date} //= ${"$mod\::DATE"};
    }

    require Perinci::Sub::Normalize;
    $meta = Perinci::Sub::Normalize::normalize_function_metadata($meta);

    require Perinci::Object;
    if (Perinci::Object::risub($meta)->can_dry_run) {
        $self->common_opts->{dry_run} = {
            getopt  => 'dry-run',
            summary => "Run in simulation mode (also via DRY_RUN=1)",
            handler => sub {
                $self->dry_run(1);
                #$ENV{VERBOSE} = 1;
            },
        };
    }

    $meta;
}

# XXX
sub run_help {
    my ($self) = @_;

    [200, "OK", "Help message"];
}

sub run_call {
    my ($self, $args, $meta) = @_;

    my $scd = $self->selected_subcommand_data;
    my ($mod, $func) = __require_url($scd->{url});

    no strict 'refs';
    &{"$mod\::$func"}(%$args);
}

sub hook_before_run {}

sub hook_after_run {}

sub hook_after_parse_opts {}

sub hook_after_select_subcommand {}

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
