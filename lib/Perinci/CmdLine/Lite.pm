package Perinci::CmdLine::Lite;

# DATE
# VERSION

use 5.010001;
# use strict; # already enabled by Mo
# use warnings; # already enabled by Mo

use Mo; extends 'Perinci::CmdLine::Base';

# when debugging, use this instead of the above because Mo doesn't give clear
# error message if base class has errors.
#use parent 'Perinci::CmdLine::Base';

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
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{action} = 'version';
                    $r->{skip_parse_subcommand_argv} = 1;
                },
            },
            help => {
                getopt  => 'help|h|?',
                summary => 'Show help message',
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{action} = 'help';
                    $r->{skip_parse_subcommand_argv} = 1;
                },
            },
            format => {
                getopt  => 'format=s',
                summary => 'Set output format',
                schema => ['str*' => in => [qw/text text-simple text-pretty
                                               json json-pretty/]],
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{format} = $val;
                },
            },
            json => {
                getopt  => 'json',
                summary => 'Set output format to json',
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{format} = 'json';
                },
            },
        };
        if ($self->subcommands) {
            $co->{subcommands} = {
                getopt  => 'subcommands',
                summary => 'List available subcommands',
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{action} = 'subcommands';
                    $r->{skip_parse_subcommand_argv} = 1;
                },
            };
        }
        if ($self->default_subcommand) {
            $co->{cmd} = {
                getopt  => 'cmd=s',
                summary => 'Select subcommand',
                handler => sub {
                    my ($go, $val, $r) = @_;
                    $r->{subcommand_name} = $val;
                    $r->{subcommand_name_from} = '--cmd';
                },
                completion => sub {
                    require Complete::Util;
                    my %args = @_;
                    Complete::Util::complete_array_elem(
                        array => [keys %{ $self->list_subcommands }],
                        word => $args{word});
                },
            };
        }
        $self->{common_opts} = $co;
    }

    $self->{formats} //= [qw/text text-simple text-pretty json/];

    $self->{per_arg_json} //= 1;
}

sub hook_before_run {}

sub hook_after_parse_argv {}

sub hook_format_result {
    my ($self, $r) = @_;

    my $res    = $r->{res};
    my $format = $r->{format} // 'text';
    my $meta   = $r->{meta};

    if ($format =~ /\Atext(-simple|-pretty)?\z/) {
        my $is_pretty = $format eq 'text-pretty' ? 1 :
            $format eq 'text-simple' ? 0 : (-t STDOUT);
        no warnings 'uninitialized';
        if ($res->[0] != 200) {
            my $fres = "ERROR $res->[0]: $res->[1]";
            if (my $prev = $res->[3]{prev}) {
                $fres .= " ($prev->[0]: $prev->[1])";
            }
            return "$fres\n";
        } else {
            require Data::Check::Structure;
            my $data = $res->[2];
            my $max = 5;
            if (!ref($data)) {
                $data //= "";
                $data .= "\n" unless !length($data) || $data =~ /\n\z/;
                return $data;
            } elsif (ref($data) eq 'ARRAY' && !@$data) {
                return "";
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

sub hook_display_result {
    my ($self, $r) = @_;
    print $r->{fres};
}

sub hook_after_run {}

# copy-pasted from SHARYANTO::Package::Util
sub __package_exists {
    no strict 'refs';

    my $pkg = shift;

    return unless $pkg =~ /\A\w+(::\w+)*\z/;
    if ($pkg =~ s/::(\w+)\z//) {
        return !!${$pkg . "::"}{$1 . "::"};
    } else {
        return !!$::{$pkg . "::"};
    }
}

sub __require_url {
    my ($url) = @_;

    $url =~ m!\A(?:pl:)?/(\w+(?:/\w+)*)/(\w*)\z!
        or die [500, "Unsupported/bad URL '$url'"];
    my ($mod, $func) = ($1, $2);
    # skip if package already exists, e.g. 'main'
    require "$mod.pm" unless __package_exists($mod);
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
                my ($go, $val, $r) = @_;
                $r->{dry_run} = 1;
                #$ENV{VERBOSE} = 1;
            },
        };
    }

    $meta;
}

sub run_subcommands {
    my ($self, $r) = @_;

    if (!$self->subcommands) {
        say "There are no subcommands.";
        return 0;
    }

    say "Available subcommands:";
    my $scs = $self->list_subcommands;
    my $longest = 6;
    for (keys %$scs) { my $l = length; $longest = $l if $l > $longest }
    [200, "OK",
     join("",
          (map { sprintf("  %-${longest}s  %s\n",$_,$scs->{$_}{summary}//"") }
               sort keys %$scs),
      )];
}

sub run_version {
    my ($self, $r) = @_;

    my $meta = $r->{meta} = $self->get_meta($self->url);

    [200, "OK",
     join("",
          $self->get_program_and_subcommand_name($r),
          " version ", ($meta->{entity_v} // "?"),
          ($meta->{entity_date} ? " ($meta->{entity_date})" : ''),
          "\n",
          "  ", __PACKAGE__,
          " version ", ($Perinci::CmdLine::Lite::VERSION // "?"),
          ($Perinci::CmdLine::Lite::DATE ? " ($Perinci::CmdLine::Lite::DATE)":''),
      )];
}

sub run_help {
    my ($self, $r) = @_;

    my @help;
    my $scn    = $r->{subcommand_name};
    my $scd    = $r->{subcommand_data};
    my $meta   = $self->get_meta($scd->{url} // $self->{url});
    my $args_p = $meta->{args} // {};

    # summary
    my $cmdname = $self->get_program_and_subcommand_name($r);
    push @help, $cmdname;
    {
        my $sum = ($scd ? $scd->{summary} : undef) //
            $meta->{summary};
        last unless $sum;
        push @help, " - ", $sum, "\n";
    }

    # usage
    push @help, "\n";
    push @help, "Usage:\n";
    {
        push @help, "  $cmdname --help (or -h, -?)\n";
        push @help, "  $cmdname --verbose (or -v)\n";
        my @args;
        my %args = %{ $args_p };
        my $max_pos = -1;
        for (values %args) {
            $max_pos = $_->{pos} if defined($_->{pos}) && $_->{pos} > $max_pos;
        }
        my $pos = 0;
        while ($pos <= $max_pos) {
            my ($arg, $as);
            for (keys %args) {
                $as = $args{$_};
                if (defined($as->{pos}) && $as->{pos}==$pos) { $arg=$_; last }
            }
            next unless defined($arg);
            if ($as->{req}) {
                push @args, "<$arg>" . ($as->{greedy} ? " ...":"");
            } else {
                push @args, "[$arg]" . ($as->{greedy} ? " ...":"");
            }
            delete $args{$arg};
            $pos++;
        }
        unshift @args, "[options]" if keys %args;
        push @help, "  $cmdname ".join(" ", @args)."\n";
    }

    # description
    push @help, "\n";
    {
        my $desc = ($scd ? $scd->{description} : undef) //
            $meta->{description};
        last unless $desc;
        $desc =~ s/\A\n+//;
        $desc =~ s/\n+\z//;
        push @help, $desc, "\n";
    }

    # options
    {
        require Perinci::Sub::GetArgs::Argv;
        my $co = $self->common_opts;
        my $co_by_ospec = { map {$co->{$_}{getopt} => $_ } keys %$co };
        my $res = Perinci::Sub::GetArgs::Argv::gen_getopt_long_spec_from_meta(
            meta         => $meta,
            common_opts  => $co,
            per_arg_json => $self->{per_arg_json},
            per_arg_yaml => $self->{per_arg_yaml},
        );
        my $sms = $res->[3]{'func.specmeta'};

        # first, all common options first
        my @opts;
        for my $k (sort(grep {!defined($sms->{$_}{arg})} keys %$sms)) {
            my $p = $sms->{$k}{parsed};
            # XXX currently ad-hoc, skip irrelevant common opt
            next if $scn && $k eq 'subcommands';
            my $i = 0;
            my $opt = '';
            for (@{ $p->{opts} }) {
                $i++;
                $opt .= ", " if $i > 1;
                $opt .= (length($_) > 1 ? '--':'-').$_;
                $opt .= "=$p->{type}" if $p->{type} && $i==1;
            }
            push @opts, [$opt, $co->{$co_by_ospec->{$k}}{summary}];
        }
        my $longest = 6;
        for (@opts) { my $l = length($_->[0]); $longest = $l if $l > $longest }
        push @help, "\nCommon options:\n" if @opts;
        for (@opts) {
            push @help, sprintf("  %-${longest}s  %s\n",
                                $_->[0], $_->[1] // "");
        }

        # now the rest
        @opts = ();
        for my $k (sort(grep {defined($sms->{$_}{arg})} keys %$sms)) {
            my $sm = $sms->{$k};
            # skip non-code aliases
            next if $sm->{is_alias} && !$sm->{is_code};
            my $p = $sm->{parsed};
            my $i = 0;
            my $opt = '';
            for (@{ $p->{opts} }) {
                $i++;
                $opt .= ", " if $i > 1;
                $opt .= (length($_) > 1 ? '--':'-').$_;
                $opt .= "=$p->{type}" if $p->{type} && $i==1;
            }
            # add non-code aliases
            for my $al (@{ $sm->{noncode_aliases} // [] }) {
                $al =~ s/=.+//; $al = (length($al) > 1 ? "--":"-").$al;
                $opt .= ", $al";
            }
            my $arg = $sm->{arg};
            my $as = $args_p->{$arg};
            my $alspec = $sm->{alias} ? $as->{cmdline_aliases}{$sm->{alias}} : {};
            my $sum = join(
                "",
                (defined($as->{pos}) ? "(or via arg #$as->{pos}".
                     ($as->{greedy} ? "+":"").") " : ""),
                ($sm->{alias_for} ? $alspec->{summary} // "Alias for $sm->{alias_for}" :
                     $as->{summary} // ''),
            );
            my $sch = ($sm->{is_alias} ?
                           $as->{cmdline_aliases}{$sm->{alias}}{schema} : undef) //
                               $as->{schema};
            if ($sch && $sch->[1]{in}) {
                $sum .= " (".join("|", @{ $sch->[1]{in} }).")";
            }
            push @opts, [$opt, $sum];
        }
        for (@opts) { my $l = length($_->[0]); $longest = $l if $l > $longest }
        push @help, "\nOptions:\n" if @opts;
        for (@opts) {
            push @help, sprintf("  %-${longest}s  %s\n",
                                $_->[0], $_->[1] // "");
        }
        push @help, "\n" if @opts;
    }

    [200, "OK", join("", @help)];
}

sub run_call {
    my ($self, $r) = @_;

    my $scd = $r->{subcommand_data};
    my ($mod, $func) = __require_url($scd->{url});

    # convert args
    my $aa = $r->{meta}{args_as} // 'hash';
    my @args;
    if ($aa =~ /array/) {
        require Perinci::Sub::ConvertArgs::Array;
        my $convres = Perinci::Sub::ConvertArgs::Array::convert_args_to_array(
            args => $r->{args}, meta => $r->{meta},
        );
        return $convres unless $convres->[0] == 200;
        if ($aa =~ /ref/) {
            @args = ($convres->[2]);
        } else {
            @args = @{ $convres->[2] };
        }
    } elsif ($aa eq 'hashref') {
        @args = ({ %{ $r->{args} } });
    } else {
        # hash
        @args = %{ $r->{args} };
    }

    # call!
    my $res;
    {
        no strict 'refs';
        $res = &{"$mod\::$func"}(@args);
    }

    # add envelope
    if ($r->{meta}{result_naked}) {
        $res = [200, "OK (envelope added by ".__PACKAGE__.")", $res];
    }

    $res;
}

1;
# ABSTRACT: A lightweight Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(BUILD|get_meta|hook_.+|run_.+)$

=head1 SYNOPSIS

See L<Perinci::CmdLine::Manual::Examples>.


=head1 DESCRIPTION

Perinci::CmdLine::Lite (hereby P::C::Lite) is a lightweight (low startup
overhead, minimal dependencies) alternative to L<Perinci::CmdLine> (hereby
P::C). It offers a subset of functionality and a compatible API. Unless you use
the unsupported features of P::C, P::C::Lite is a drop-in replacement for P::C
(also see L<Perinci::CmdLine::Any> for automatic fallback).

P::C::Lite stays lightweight by avoiding the use of libraries that have large
dependencies or add too much to startup overhead. This includes
L<Perinci::Access> for metadata access, L<Data::Sah> for validator generation,
L<Text::ANSITable> for formatting results, and L<Log::Any::App> (which uses
L<Log::Log4perl>) for logging.

I first developed P::C::Lite mainly for CLI applications that utilize shell tab
completion as their main feature, e.g. L<App::PMUtils>, L<App::ProgUtils>,
L<App::GitUtils>.

Below is summary of the differences between P::C::Lite and P::C:

=over

=item * P::C::Lite starts much faster

The target is under 0.05s to make shell tab completion convenient. On the other
hand, P::C can start between 0.2-0.5s.

=item * P::C::Lite uses simpler formatting

Instead of L<Perinci::Result::Format> (especially for 'text*' formats which use
L<Data::Format::Pretty::Console> and L<Text::ANSITable>), to keep dependencies
minimal and formatting quick, P::C::Lite uses the following simple rules that
work for a significant portion of common data structures:

1) if result is undef, print nothing.

2) if result is scalar, print it (with newline automatically added).

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

Table is printed using the more lightweight and much faster
L<Text::Table::Tiny>.

=item * No remote URL support in P::C::Lite

Instead of using Perinci::Access, P::C::Lite accesses Perl packages on the
filesystem directly. This means only code on the filesystem is available. (But I
plan to write another subclass P::C::Lite::HTTP that uses L<HTTP::Tiny> or
L<HTTP::Tiny::UNIX> for Riap::HTTP support).

=item * No automatic validation from schema in P::C::Lite

Since code wrapping and schema code generation done by L<Perinci::Sub::Wrapper>
and L<Data::Sah> (which are called automatically by Perinci::Access) adds too
much startup overhead.

=item * P::C::Lite does not support color themes

=item * P::C::Lite does not support undo

=item * P::C::Lite does not currently support logging

Something more lightweight than L<Log::Any::App> will be considered. But for
now, if you want to view logging and your function uses L<Log::Any>, you can do
something like this:

 % DEBUG=1 PERL5OPT=-MLog::Any::App app.pl

=item * P::C::Lite does not support progress indicator

=item * P::C::Lite does not support I18N

=item * P::C::Lite does not yet support these Rinci function argument specification properties

 cmdline_src

=item * P::C::Lite does not yet support these Rinci result metadata properties/attributes

 is_stream
 cmdline.page_result
 cmdline.pager

=item * P::C::Lite does not yet support these environment variables

 PERINCI_CMDLINE_COLOR_THEME
 PERINCI_CMDLINE_SERVER
 PROGRESS
 PAGER
 COLOR
 UTF8

 DEBUG, VERBOSE, QUIET, TRACE, and so on

=item * In passing command-line object to functions, P::C::Lite object is passed

Some functions might expect a L<Perinci::CmdLine> instance.

=back


=head1 ATTRIBUTES

All the attributes of L<Perinci::CmdLine::Base>, plus:

=over

=back


=head1 METHODS

All the methods of L<Perinci::CmdLine::Base>, plus:

=over

=back


=head1 ENVIRONMENT

All the environment variables that L<Perinci::CmdLine::Base> supports, plus:

=over

=back


=head1 SEE ALSO

L<Perinci::CmdLine>, L<Perinci::CmdLine::Manual>

L<Perinci::CmdLine::Any>

=cut
