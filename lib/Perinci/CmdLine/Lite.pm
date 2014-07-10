package Perinci::CmdLine::Lite;

use 5.010001;

# DATE
# VERSION

sub format_result {
    require Perinci::Result::Format;

    my $self = shift;
    my $res  = $self->{_res};
    return unless $res;

    my $resmeta = $res->[3] // {};
    unless ($resmeta->{"cmdline.display_result"} // 1) {
        $res->[2] = undef;
        return;
    }

    my $format = $self->format_set ?
        $self->format :
            $self->{_meta}{"x.perinci.cmdline.default_format"} // $self->format;
    $self->_err("Unknown output format '$format', please choose one of: ".
        join(", ", sort keys(%Perinci::Result::Format::Formats)))
            unless $Perinci::Result::Format::Formats{$format};
    if ($self->format_options_set) {
        $res->[3]{result_format_options} = $self->format_options;
        $resmeta = $res->[3];
    }

    if ($resmeta->{is_stream}) {
        $log->tracef("Result is a stream");
    } else {
        $log->tracef("Formatting output with %s", $format);
        $self->{_fres} = Perinci::Result::Format::format(
            $self->{_res}, $format);
    }
}

sub display_result {
    require File::Which;

    my $self = shift;

    my $res  = $self->{_res};
    return unless $res;

    my $resmeta = $res->[3] // {};

    my $handle;
    {
        if ($resmeta->{"cmdline.page_result"}) {
            my $pager = $resmeta->{"cmdline.pager"} //
                $ENV{PAGER};
            unless (defined $pager) {
                $pager = "less -FRSX" if File::Which::which("less");
            }
            unless (defined $pager) {
                $pager = "more" if File::Which::which("more");
            }
            unless (defined $pager) {
                $self->_err("Can't determine PAGER");
            }
            last unless $pager; # ENV{PAGER} can be set 0/'' to disable paging
            $log->tracef("Paging output using %s", $pager);
            open $handle, "| $pager";
        }
    }
    $handle //= \*STDOUT;

    if ($resmeta->{is_stream}) {
        $self->_err("Can't format stream as " . $self->format .
                        ", please use --format text")
            unless $self->format =~ /^text/;
        my $r = $res->[2];
        if (ref($r) eq 'GLOB') {
            while (!eof($r)) {
                print $handle ~~<$r>;
            }
        } elsif (blessed($r) && $r->can('getline') && $r->can('eof')) {
            # IO::Handle-like object
            while (!$r->eof) {
                print $r->getline;
            }
        } elsif (ref($r) eq 'ARRAY') {
            # tied array
            while (~~(@$r) > 0) {
                print $self->format_row(shift(@$r));
            }
        } else {
            $self->_err("Invalid stream in result (not a glob/IO::Handle-like ".
                            "object/(tied) array)\n");
        }
    } else {
        print $handle $self->{_fres} // "";
    }
}

sub get_subcommand {
    my ($self, $name) = @_;

    my $scs = $self->subcommands;
    return undef unless $scs;

    if (reftype($scs) eq 'CODE') {
        return $scs->($self, name=>$name);
    } else {
        return $scs->{$name};
    }
}

sub list_subcommands {
    my ($self) = @_;
    state $cached;
    return $cached if $cached;

    my $scs = $self->subcommands;
    my $res;
    if ($scs) {
        if (reftype($scs) eq 'CODE') {
            $scs = $scs->($self);
            $self->_err("Subcommands code didn't return a hashref")
                unless ref($scs) eq 'HASH';
        }
        $res = $scs;
    } else {
        $res = {};
    }
    $cached = $res;
}

sub run_subcommands {
    my ($self) = @_;

    if (!$self->subcommands) {
        say __("There are no subcommands") . ".";
        return 0;
    }

    my $subcommands = $self->list_subcommands;

    # XXX get summary from Riap if not exist, but this results in multiple Riap
    # requests.

    my %percat_subc; # (cat1 => {subcmd1=>..., ...}, ...)
    while (my ($scn, $sc) = each %$subcommands) {
        my $cat = "";
        for my $tag (@{$sc->{tags} // []}) {
            my $tn = ref($tag) ? $tag->{name} : $tag;
            next unless $tn =~ /^category:(.+)/;
            $cat = $1;
            last;
        }
        $percat_subc{$cat}       //= {};
        $percat_subc{$cat}{$scn}   = $sc;
    }
    my $has_many_cats = scalar(keys %percat_subc) > 1;

    my $i = 0;
    for my $cat (sort keys %percat_subc) {
        if ($has_many_cats) {
            $self->_help_add_heading(
                __x("{category} subcommands",
                    category => ucfirst($cat || __("main"))));
        }
        my $subc = $percat_subc{$cat};
        for my $scn (sort keys %$subc) {
            my $sc = $subc->{$scn};
            my $summary = rimeta($sc)->langprop("summary");
            $self->_help_add_row(
                [$self->_color('program_name', $scn), $summary],
                {column_widths=>[-17, -40], indent=>1});
        }
    }
    $self->_help_draw_curtbl;

    0;
}

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
        $log->warnf("Can't request 'meta' action on %s: %d - %s",
                    $url, $res->[0], $res->[1]);
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

sub run_completion {
    require Complete::Bash;
    require Complete::Util;
    require Perinci::Sub::Complete;

    my ($self) = @_;

    my $sc = $self->{_subcommand};
    my ($words, $cword) = @{ $self->{_comp_parse_res} };
    my $word  = $words->[$cword] // "";

    # determine whether we should complete function arg names/values or just
    # top-level opts + subcommands name
    my $do_arg;
    {
        if (!$self->subcommands) {
            $log->trace("do_arg because single command");
            $do_arg++; last;
        }

        my $scn = $sc->{name} // "";

        # whether user typed 'blah blah ^' or 'blah blah^'
        my $space_typed = !defined($word);

        # e.g: spanel delete-account ^
        if ($self->subcommands && $cword > 0 && $space_typed) {
            $log->trace("do_arg because last word typed (+space) is ".
                            "subcommand name");
            $do_arg++; last;
        }

        # e.g: spanel delete-account --format=yaml --acc^
        if ($cword > 0 && !$space_typed && $word ne $scn) {
            $log->trace("do_arg because subcommand name has been typed ".
                            "in past words");
            $do_arg++; last;
        }

        $log->tracef("not do_arg, cword=%d, words=%s, scn=%s, space_typed=%s",
                     $cword, $words, $scn, $space_typed);
    }

    my @top_opts; # contain --help, -h, etc.
    for my $o (keys %{{@{ $self->{_go_specs_common} }}}) {
        $o =~ s/^--//;
        $o =~ s/=.+$//;
        my @o = split /\|/, $o;
        for (@o) { push @top_opts, length > 1 ? "--$_" : "-$_" }
    }

    my $res;
    if ($do_arg) {
        $log->trace("Completing subcommand argument names & values ...");

        # remove subcommand name and general options from words so it doesn't
        # interfere with matching function args
        my $i = 0;
        while ($i < @$words) {
            if ($words->[$i] ~~ @top_opts ||
                    (defined($self->{_scn_in_argv}) &&
                         $words->[$i] eq $self->{_scn_in_argv})) {
                splice @$words, $i, 1;
                $cword-- unless $cword <= $i;
                next;
            } else {
                $i++;
            }
        }
        $log->tracef("cleaned words=%s, cword=%d", $words, $cword);

        # convert @getopts' ('help|h|?' => ..., ...) to ['--help', '-h', '-?',
        # ...]. XXX this should be moved to another module to remove
        # duplication, as Perinci::Sub::GetArgs::Argv also does something
        # similar.
        my $common_opts = [];
        for my $k (keys %{{@{ $self->{_go_specs_common} }}}) {
            $k =~ s/^--?//;
            $k =~ s/^([\w?-]+(?:\|[\w?-]+)*)(?:\W.*)?/$1/;
            for (split /\|/, $k) {
                push @$common_opts, (length == 1 ? "-$_" : "--$_");
            }
        }

        my $rres = $self->_pa->request(meta => $sc->{url});
        if ($rres->[0] != 200) {
            $log->debugf("Can't get meta for completion: %s", $rres);
            $res = [];
            goto DISPLAY_RES;
        }
        my $meta = $rres->[2];

        $res = Perinci::Sub::Complete::complete_cli_arg(
            meta=>$meta, words=>$words, cword=>$cword,
            common_opts => $common_opts,
            riap_server_url => $sc->{url},
            riap_uri        => undef,
            riap_client     => $self->_pa,
            custom_completer     => $self->custom_completer,
            custom_arg_completer => $self->custom_arg_completer,
        );

    } else {
        $log->trace("Completing top-level options + subcommand name ...");
        my @ary;
        push @ary, @top_opts;
        my $scs = $self->list_subcommands;
        push @ary, keys %$scs;
        $res = {
            completion => Complete::Util::complete_array_elem(
                word=>$word, array=>\@ary,
            ),
            type=>'option',
        };
    }

  DISPLAY_RES:
    my $compres = Complete::Bash::format_completion($res);
    if ($ENV{PERINCI_CMDLINE_SERVER}) {
        # a flag that causes completion result to be stored in the object
        # instead of printed. this is because we want to return result in a
        # function
        $self->{_compres} = $compres;
    } else {
        print $compres;
    }
    0;
}

# some common opts can be added only after we get the function metadata
sub _add_common_opts_after_meta {
    my $self = shift;

    if (risub($self->{_meta})->can_dry_run) {
        $self->common_opts->{dry_run} = {
            getopt  => 'dry-run',
            summary => "Run in simulation mode (also via DRY_RUN=1)",
            handler => sub {
                $self->{_dry_run} = 1;
                $ENV{VERBOSE} = 1;
            },
        };
    }

    # update the cached getopt specs
    my @go_opts = $self->_gen_go_specs_from_common_opts;
    $self->{_go_specs_common} = \@go_opts;
}

sub _help_draw_curtbl {
    my $self = shift;

    if ($self->{_help_curtbl}) {
        print $self->{_help_curtbl}->draw;
        undef $self->{_help_curtbl};
    }
}

# ansitables are used to draw formatted help. they are 100% wide, with no
# borders (except space), but you can customize the number of columns (which
# will be divided equally)
sub _help_add_table {
    require Text::ANSITable;

    my ($self, %args) = @_;
    my $columns = $args{columns} // 1;

    $self->_help_draw_curtbl;
    my $t = Text::ANSITable->new;
    $t->border_style('Default::spacei_ascii');
    $t->cell_pad(0);
    if ($args{column_widths}) {
        for (0..$columns-1) {
            $t->set_column_style($_, width => $args{column_widths}[$_]);
        }
    } else {
        my $tw = $self->term_width;
        my $cw = int($tw/$columns)-1;
        $t->cell_width($cw);
    }
    $t->show_header(0);
    $t->column_wrap(0); # we'll do our own wrapping, before indent
    $t->columns([0..$columns-1]);

    $self->{_help_curtbl} = $t;
}

sub _help_add_row {
    my ($self, $row, $args) = @_;
    $args //= {};
    my $wrap    = $args->{wrap}   // 0;
    my $indent  = $args->{indent} // 0;
    my $columns = @$row;

    # start a new table if necessary
    $self->_help_add_table(
        columns=>$columns, column_widths=>$args->{column_widths})
        if !$self->{_help_curtbl} ||
            $columns != @{ $self->{_help_curtbl}{columns} };

    my $t = $self->{_help_curtbl};
    my $rownum = @{ $t->{rows} };

    $t->add_row($row);

    for (0..@{$t->{columns}}-1) {
        my %styles = (formats=>[]);
        push @{ $styles{formats} },
            [wrap=>{ansi=>1, mb=>1, width=>$t->{cell_width}-$indent*2}]
                if $wrap;
        push @{ $styles{formats} }, [lins=>{text=>"  " x $indent}]
            if $indent && $_ == 0;
        $t->set_cell_style($rownum, $_, \%styles);
    }
}

sub _help_add_heading {
    my ($self, $heading) = @_;
    $self->_help_add_row([$self->_color('heading', $heading)]);
}

sub _color {
    my ($self, $color_name, $text) = @_;
    my $color_code = $color_name ?
        $self->get_theme_color_as_ansi($color_name) : "";
    my $reset_code = $color_code ? "\e[0m" : "";
    "$color_code$text$reset_code";
}

sub help_section_summary {
    my ($self, %opts) = @_;

    my $summary = rimeta($self->{_help_meta})->langprop("summary");
    return unless $summary;

    my $name = $self->_program_and_subcommand_name;
    my $ct = join(
        "",
        $self->_color('program_name', $name),
        ($name && $summary ? ' - ' : ''),
        $summary // "",
    );
    $self->_help_add_row([$ct], {wrap=>1});
}

sub _usage_args {
    my $self = shift;

    my $m = $self->{_help_meta};
    return "" unless $m;
    my $aa = $m->{args};
    return "" unless $aa;

    # arguments with pos defined
    my @a = sort { $aa->{$a}{pos} <=> $aa->{$b}{pos} }
        grep { defined($aa->{$_}{pos}) } keys %$aa;
    my $res = "";
    for (@a) {
        $res .= " ";
        my $label = lc($_);
        $res .= $aa->{$_}{req} ? "<$label>" : "[$label]";
        $res .= " ..." if $aa->{$_}{greedy};
        last if $aa->{$_}{greedy};
    }
    $res;
}

sub help_section_usage {
    my ($self, %opts) = @_;

    my $co = $self->common_opts;
    my @con = grep {
        my $cov = $co->{$_};
        my $show = $cov->{show_in_usage} // 1;
        for ($show) { if (ref($_) eq 'CODE') { $_ = $_->($self) } }
        $show;
    } sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co;

    my $pn = $self->_color('program_name', $self->_program_and_subcommand_name);
    my $ct = "";
    for my $con (@con) {
        my $cov = $co->{$con};
        next unless $cov->{usage};
        $ct .= ($ct ? "\n" : "") . $pn . " " . __($cov->{usage});
    }
    if ($self->subcommands && !$self->{_subcommand}) {
        if (defined $self->default_subcommand) {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("--cmd=<other-subcommand> [options]");
        } else {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("<subcommand> [options]");
        }
    } else {
            $ct .= ($ct ? "\n" : "") . $pn .
                " " . __("[options]"). $self->_usage_args;
    }
    $self->_help_add_heading(__("Usage"));
    $self->_help_add_row([$ct], {indent=>1});
}

sub help_section_options {
    require Getopt::Long::Util;

    my ($self, %opts) = @_;
    my $verbose = $opts{verbose};
    my $info = $self->{_help_info};
    my $meta = $self->{_help_meta};
    my $args_p = $meta->{args};
    my $sc = $self->subcommands;

    # stored gathered options by category, e.g. $catopts{"Common options"} (an
    # array containing options)
    my %catopts;

    my $t_opts = __("Options");
    my $t_copts = __("Common options");

    # gather common opts
    my $co = $self->common_opts;
    my @con = grep {
        my $cov = $co->{$_};
        my $show = $cov->{show_in_options} // 1;
        for ($show) { if (ref($_) eq 'CODE') { $_ = $_->($self) } }
        $show;
    } sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co;
    for my $con (@con) {
        my $cov = $co->{$con};
        my $cat = $cov->{category} ? __($cov->{category}) :
            ($sc ? $t_copts : $t_opts);
        my $go = $cov->{getopt};
        push @{ $catopts{$cat} }, {
            getopt=>Getopt::Long::Util::humanize_getopt_long_opt_spec($cov->{getopt}),
            summary=> $cov->{summary} ? __($cov->{summary}) : "",
        };
    }

    # gather function opts (XXX: categorize according to tags)
    if ($info && $info->{type} eq 'function' && $args_p && %$args_p) {
        for my $an (sort {
            ($args_p->{$a}{pos} // 99) <=> ($args_p->{$b}{pos} // 99) ||
                $a cmp $b
            } keys %$args_p) {
            my $a = $args_p->{$an};
            my $s = $a->{schema} || [any=>{}];
            my $got = Perinci::ToUtil::sah2human_short($s);
            my $ane = $an; $ane =~ s/_/-/g; $ane =~ s/\W/-/g;
            my $summary = rimeta($a)->langprop("summary");

            my $suf = "";
            if ($s->[0] eq 'bool') {
                $got = undef;
                if ($s->[1]{default}) {
                    $ane = "no$ane";
                    my $negsummary = rimeta($a)->langprop(
                        "x.perinci.cmdline.negative_summary");
                    $summary = $negsummary if $negsummary;
                } elsif (defined $s->[1]{default}) {
                    #$ane = $ane;
                } else {
                    $ane = "[no]$ane";
                }
            } elsif ($s->[0] eq 'float' || $s->[0] eq 'num') {
                $ane .= "=f";
            } elsif ($s->[0] eq 'int') {
                $ane .= "=i";
            } elsif ($s->[0] eq 'hash' || $s->[0] eq 'array') {
                $suf = "-json";
                $ane = "$ane-json=val";
            } else {
                $ane .= "=s";
            }

            # add aliases which does not have code
            for my $al0 (keys %{ $a->{cmdline_aliases} // {}}) {
                my $alspec = $a->{cmdline_aliases}{$al0};
                next if $alspec->{code};
                my $al = $al0; $al =~ s/_/-/g;
                if (length($al) == 1) {
                    $al = "-$al";
                    $ane .= ", $al";
                } else {
                    $al = "--$al";
                    $ane .= ", $al$suf";
                }
            }

            my $def = defined($s->[1]{default}) && $s->[0] ne 'bool' ?
                " (default: ".dump1($s->[1]{default}).")" : "";
            my $src = $a->{cmdline_src} // "";
            my $in;
            if ($s->[1]{in} && @{ $s->[1]{in} }) {
                $in = dump1($s->[1]{in});
            }

            my $cat;
            for my $tag (@{ $a->{tags} // []}) {
                my $tn = ref($tag) ? $tag->{name} : $tag;
                next unless $tn =~ /^category:(.+)/;
                $cat = $1;
                last;
            }
            if ($cat) {
                $cat = __x("{category} options", category=>ucfirst($cat));
            } else {
                $cat = $t_opts;
            }

            push @{ $catopts{$cat} }, {
                getopt => "--$ane",
                getopt_type => $got,
                getopt_note =>join(
                    "",
                    ($a->{req} ? " (" . __("required") . ")" : ""),
                    (defined($a->{pos}) ? " (" .
                         __x("or as argument #{index}",
                            index => ($a->{pos}+1).($a->{greedy} ? "+":"")).")":""),
                    ($src eq 'stdin' ?
                         " (" . __("from stdin") . ")" : ""),
                    ($src eq 'stdin_or_files' ?
                         " (" . __("from stdin/files") . ")" : ""),
                    $def
                ),
                req => $a->{req},
                summary => $summary,
                description => rimeta($a)->langprop("description"),
                in => $in,
            };

            # add aliases which have code as separate options
            for my $al0 (keys %{ $a->{cmdline_aliases} // {}}) {
                my $alspec = $a->{cmdline_aliases}{$al0};
                next unless $alspec->{code};
                push @{ $catopts{$cat} }, {
                    getopt => length($al0) > 1 ? "--$al0" : "-$al0",
                    getopt_type => $got,
                    getopt_note => undef,
                    #req => $a->{req},
                    summary => rimeta($alspec)->langprop("summary"),
                    description => rimeta($alspec)->langprop("description"),
                    #in => $in,
                };
            }

        }
    }

    # output gathered options
    for my $cat (sort keys %catopts) {
        $self->_help_add_heading($cat);
        my @opts = sort {
            my $va = $a->{getopt};
            my $vb = $b->{getopt};
            for ($va, $vb) { s/^--(\[no\])?// }
            $va cmp $vb;
        } @{$catopts{$cat}};
        if ($verbose) {
            for my $o (@opts) {
                my $ct = $self->_color('option_name', $o->{getopt}) .
                    ($o->{getopt_type} ? " [$o->{getopt_type}]" : "").
                        ($o->{getopt_note} ? $o->{getopt_note} : "");
                $self->_help_add_row([$ct], {indent=>1});
                if ($o->{in} || $o->{summary} || $o->{description}) {
                    my $ct = "";
                    $ct .= ($ct ? "\n\n":"").ucfirst(__("value in")).
                        ": $o->{in}" if $o->{in};
                    $ct .= ($ct ? "\n\n":"")."$o->{summary}." if $o->{summary};
                    $ct .= ($ct ? "\n\n":"").$o->{description}
                        if $o->{description};
                    $self->_help_add_row([$ct], {indent=>2, wrap=>1});
                }
            }
        } else {
            # for compactness, display in columns
            my $tw = $self->term_width;
            my $columns = int($tw/40); $columns = 1 if $columns < 1;
            while (1) {
                my @row;
                for (1..$columns) {
                    last unless @opts;
                    my $o = shift @opts;
                    push @row, $self->_color('option_name', $o->{getopt}) .
                        #($o->{getopt_type} ? " [$o->{getopt_type}]" : "") .
                            ($o->{getopt_note} ? $o->{getopt_note} : "");
                }
                last unless @row;
                for (@row+1 .. $columns) { push @row, "" }
                $self->_help_add_row(\@row, {indent=>1});
            }
        }
    }
}

sub help_section_subcommands {
    my ($self, %opts) = @_;

    my $scs = $self->subcommands;
    return unless $scs && !$self->{_subcommand};

    my @scs = sort keys %$scs;
    my @shown_scs;
    for my $scn (@scs) {
        my $sc = $scs->{$scn};
        next unless $sc->{show_in_help} // 1;
        $sc->{name} = $scn;
        push @shown_scs, $sc;
    }

    # for help_section_hints
    my $some_not_shown = @scs > @shown_scs;
    $self->{_some_subcommands_not_shown_in_help} = 1 if $some_not_shown;

    $self->_help_add_heading(
        $some_not_shown ? __("Popular subcommands") : __("Subcommands"));

    # in compact mode, we try to not exceed one screen, so show long mode only
    # if there are a few subcommands.
    my $long_mode = $opts{verbose} || @shown_scs < 12;
    if ($long_mode) {
        for (@shown_scs) {
            my $summary = rimeta($_)->langprop("summary");
            $self->_help_add_row(
                [$self->_color('program_name', $_->{name}), $summary],
                {column_widths=>[-17, -40], indent=>1});
        }
    } else {
        # for compactness, display in columns
        my $tw = $self->term_width;
        my $columns = int($tw/25); $columns = 1 if $columns < 1;
            while (1) {
                my @row;
                for (1..$columns) {
                    last unless @shown_scs;
                    my $sc = shift @shown_scs;
                    push @row, $sc->{name};
                }
                last unless @row;
                for (@row+1 .. $columns) { push @row, "" }
                $self->_help_add_row(\@row, {indent=>1});
            }

    }
}

sub help_section_hints {
    my ($self, %opts) = @_;
    my @hints;
    unless ($opts{verbose}) {
        push @hints, N__("For more complete help, use '--help --verbose'");
    }
    if ($self->{_some_subcommands_not_shown_in_help}) {
        push @hints,
            N__("To see all available subcommands, use '--subcommands'");
    }
    return unless @hints;

    $self->_help_add_row(
        [join(" ", map { __($_)."." } @hints)], {wrap=>1});
}

sub help_section_description {
    my ($self, %opts) = @_;

    my $desc = rimeta($self->{_help_meta})->langprop("description") //
        $self->description;
    return unless $desc;

    $self->_help_add_heading(__("Description"));
    $self->_help_add_row([$desc], {wrap=>1, indent=>1});
}

sub help_section_examples {
    my ($self, %opts) = @_;

    my $verbose = $opts{verbose};
    my $meta = $self->{_help_meta};
    my $egs = $meta->{examples};
    return unless $egs && @$egs;

    $self->_help_add_heading(__("Examples"));
    my $pn = $self->_color('program_name', $self->_program_and_subcommand_name);
    for my $eg (@$egs) {
        my $argv;
        my $ct;
        if (defined($eg->{src})) {
            # we only show shell command examples
            if ($eg->{src_plang} =~ /^(sh|bash)$/) {
                $ct = $eg->{src};
            } else {
                next;
            }
        } else {
            require String::ShellQuote;
            if ($eg->{argv}) {
                $argv = $eg->{argv};
            } else {
                require Perinci::Sub::ConvertArgs::Argv;
                my $res = Perinci::Sub::ConvertArgs::Argv::convert_args_to_argv(
                    args => $eg->{args}, meta => $meta);
                $self->_err("Can't convert args to argv: $res->[0] - $res->[1]")
                    unless $res->[0] == 200;
                $argv = $res->[2];
            }
            $ct = $pn;
            for my $arg (@$argv) {
                $arg = String::ShellQuote::shell_quote($arg);
                if ($arg =~ /^-/) {
                    $ct .= " ".$self->_color('option_name', $arg);
                } else {
                    $ct .= " $arg";
                }
            }
        }
        $self->_help_add_row([$ct], {indent=>1});
        if ($verbose) {
            $ct = "";
            my $summary = rimeta($eg)->langprop('summary');
            if ($summary) { $ct .= "$summary." }
            my $desc = rimeta($eg)->langprop('description');
            if ($desc) { $ct .= "\n\n$desc" }
            $self->_help_add_row([$ct], {indent=>2}) if $ct;
        }
    }
}

sub help_section_result {
    my ($self, %opts) = @_;

    my $meta   = $self->{_help_meta};
    my $rmeta  = $meta->{result};
    my $rmetao = rimeta($rmeta);
    my $text;

    my $summary = $rmetao->langprop('summary') // '';
    my $desc    = $rmetao->langprop('description') // '';
    $text = $summary . ($summary ? "\n\n" : "") . $desc;

    # collect handler
    my %handler_args;
    my %handler_metas;
    for my $k0 (keys %$rmeta) {
        my $v = $rmeta->{$k0};

        my $k = $k0; $k =~ s/\..+//;
        next if $k =~ /\A_/;

        # check builtin result spec key
        next if $k =~ /\A(
                           summary|description|tags|default_lang|
                           schema|
                           x
                       )\z/x;

        # try a property module first
        require "Perinci/Sub/Property/result/$k.pm";
        my $meth = "help_hookmeta_result__$k";
        unless ($self->can($meth)) {
            die "No help handler for property result/$k0 ($meth)";
        }
        my $hm = $self->$meth;
        my $ha = {
            prio=>$hm->{prio}, value=>$v->{$k0}, property=>$k0,
            meth=>"help_hook_result__$k",
        };
        $handler_args{$k} = $ha;
        $handler_metas{$k} = $hm;
    }

    # call all the handlers in order
    for my $k (sort {$handler_args{$a}{prio} <=> $handler_args{$b}{prio}}
                   keys %handler_args) {
        my $ha = $handler_args{$k};
        my $meth = $ha->{meth};
        my $t = $self->$meth(meta => $meta, %$ha);
        $text .= $t if $t;
    }

    return unless length $text;

    $self->_help_add_heading(__("Result"));
    $self->_help_add_row([$text], {wrap=>1, indent=>1});
}

sub help_section_links {
    # not yet
}

sub run_help {
    my ($self) = @_;

    my $verbose = $ENV{VERBOSE} // 0;
    my %opts = (verbose=>$verbose);

    # get function metadata first
    my $sc = $self->{_subcommand};
    my $url = $sc ? $sc->{url} : $self->url;
    if ($url) {
        my $res = $self->_pa->request(info => $url);
        $self->_err("Can't info '$url': $res->[0] - $res->[1]")
            unless $res->[0] == 200;
        $self->{_help_info} = $res->[2];
        $res = $self->_pa->request(meta => $url);
        $self->_err("Can't meta '$url': $res->[0] - $res->[1]")
            unless $res->[0] == 200;
        $self->{_help_meta} = $res->[2];
    }

    # ux: since --verbose will potentially show lots of paragraph text, let's
    # default to 80 and not wider width, unless user specifically requests
    # column width via COLUMNS.
    if ($verbose && !defined($ENV{COLUMNS}) && $self->term_width > 80) {
        $self->term_width(80);
    }

    # determine which help sections should we generate
    my @hsects;
    if ($verbose) {
        @hsects = (
            'summary',
            'usage',
            'subcommands',
            'examples',
            'description',
            'options',
            'result',
            'links',
            'hints',
        );
    } else {
        @hsects = (
            'summary',
            'usage',
            'subcommands',
            'examples',
            'options',
            'hints',
        );
    }

    for my $s (@hsects) {
        my $meth = "help_section_$s";
        $log->tracef("=> $meth(%s)", \%opts);
        $self->$meth(%opts);
    }
    $self->_help_draw_curtbl;
    0;
}

my ($ph1, $ph2); # patch handles
my $setup_progress;
sub _setup_progress_output {
    my $self = shift;

    if ($ENV{PROGRESS} // (-t STDOUT)) {
        require Progress::Any::Output;
        Progress::Any::Output->set("TermProgressBarColor");
        my $out = $Progress::Any::outputs{''}[0];
        $setup_progress = 1;
        # we need to patch the logger adapters so it won't interfere with
        # progress meter's output
        require Monkey::Patch::Action;
        $ph1 = Monkey::Patch::Action::patch_package(
            'Log::Log4perl::Appender::Screen', 'log',
            'replace', sub {
                my ($self, %params) = @_;

                my $msg = $params{message};
                $msg =~ s/\n//g;

                # clean currently displayed progress bar first
                if ($out->{lastlen}) {
                    print
                        "\b" x $out->{lastlen},
                            " " x $out->{lastlen},
                                "\b" x $out->{lastlen};
                    undef $out->{lastlen};
                }

                # force output update so progress bar is displayed again
                # immediately
                $Progress::Any::output_data{"$out"}{force_update} = 1;

                say $msg;
            },
        ) if defined &{"Log::Log4perl::Appender::Screen::log"};
        $ph2 = Monkey::Patch::Action::patch_package(
            'Log::Log4perl::Appender::ScreenColoredLevels', 'log',
            'replace', sub {
                my ($self, %params) = @_;
                # BEGIN copy-paste'ish from ScreenColoredLevels.pm
                my $msg = $params{message};
                $msg =~ s/\n//g;
                if (my $color=$self->{color}->{$params{log4p_level}}) {
                    $msg = Term::ANSIColor::colored($msg, $color);
                }
                # END copy-paste'ish

                # clean currently displayed progress bar first
                if ($out->{lastlen}) {
                    print
                        "\b" x $out->{lastlen},
                            " " x $out->{lastlen},
                                "\b" x $out->{lastlen};
                    undef $out->{lastlen};
                }

                # force output update so progress bar is displayed again
                # immediately
                $Progress::Any::output_data{"$out"}{force_update} = 1;

                # XXX duplicated code above, perhaps move this to
                # TermProgressBarColor's clean_bar() or something

                say $msg;
            }
        ) if defined &{"Log::Log4perl::Appender::ScreenColoredLevels::log"};
    }
}

sub _unsetup_progress_output {
    my $self = shift;

    return unless $setup_progress;
    my $out = $Progress::Any::outputs{''}[0];
    $out->cleanup if $out->can("cleanup");
    undef $ph1;
    undef $ph2;
    $setup_progress = 0;
}

sub run_call {
    require File::Which;

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

    if ($using_tx) {
        require UUID::Random;
        $tx_id = UUID::Random::generate();
        $tx_id =~ s/-.+//; # 32bit suffices for small number of txs
        my $summary = join(" ", @{ $self->{_orig_argv} });
        my $res = $self->_pa->request(
            begin_tx => "/", {tx_id=>$tx_id, summary=>$summary});
        if ($res->[0] != 200) {
            $self->{_res} = [$res->[0],
                             "Can't start transaction '$tx_id': $res->[1]"];
            return 1;
        }
    }

    # setup output progress indicator
    if ($self->{_meta}{features}{progress}) {
        $self->_setup_progress_output;
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
    $log->tracef("call res=%s", $self->{_res});

    # commit transaction (if using tx)
    if ($using_tx && $self->{_res}[0] =~ /\A(?:200|304)\z/) {
        my $res = $self->_pa->request(commit_tx => "/", {tx_id=>$tx_id});
        if ($res->[0] != 200) {
            $self->{_res} = [$res->[0],
                             "Can't commit transaction '$tx_id': $res->[1]"];
            return 1;
        }
    }

    my $resmeta = $self->{_res}[3] // {};
    if (defined $resmeta->{"cmdline.exit_code"}) {
        return $resmeta->{"cmdline.exit_code"};
    } else {
        return $self->{_res}[0] =~ /\A(?:200|304)\z/ ?
            0 : $self->{_res}[0] - 300;
    }
}

sub run_history {
    my $self = shift;
    my $res = $self->_pa->request(list_txs => "/", {detail=>1});
    $log->tracef("list_txs res=%s", $res);
    return 1 unless $res->[0] == 200;
    $res->[2] = [sort {($b->{tx_commit_time}//0) <=> ($a->{tx_commit_time}//0)}
                     @{$res->[2]}];
    my @txs;
    for my $tx (@{$res->[2]}) {
        next unless $tx->{tx_status} =~ /[CUX]/;
        push @txs, {
            id          => $tx->{tx_id},
            start_time  => $tx->{tx_start_time},
            commit_time => $tx->{tx_commit_time},
            status      => $tx->{tx_status} eq 'X' ? 'error' :
                $tx->{tx_status} eq 'U' ? 'undone' : '',
            summary     => $tx->{tx_summary},
        };
    }
    $self->{_res} = [200, "OK", \@txs];
    0;
}

sub run_clear_history {
    my $self = shift;
    $self->{_res} = $self->_pa->request(discard_all_txs => "/");
    $self->{_res}[0] == 200 ? 0 : 1;
}

sub run_undo {
    my $self = shift;
    $self->{_res} = $self->_pa->request(undo => "/");
    $self->{_res}[0] == 200 ? 0 : 1;
}

sub run_redo {
    my $self = shift;
    $self->{_res} = $self->_pa->request(redo => "/");
    $self->{_res}[0] == 200 ? 0 : 1;
}

sub _gen_go_specs_from_common_opts {
    my $self = shift;

    my @go_opts;
    my $co = $self->common_opts;
    for my $con (sort {
        ($co->{$a}{order}//1) <=> ($co->{$b}{order}//1) || $a cmp $b
    } keys %$co) {
        my $cov = $co->{$con};
        $self->_err("Invalid common option '$con': empty getopt")
            unless $cov->{getopt};
        push @go_opts, $cov->{getopt} => $cov->{handler};
    }

    @go_opts;
}

sub parse_common_opts {
    require Getopt::Long;

    $log->tracef("-> parse_common_opts()");
    my ($self) = @_;

    my @orig_ARGV = @ARGV;
    $self->{_orig_argv} = \@orig_ARGV;

    my @go_opts = $self->_gen_go_specs_from_common_opts;
    $self->{_go_specs_common} = \@go_opts;
    my $old_go_opts = Getopt::Long::Configure(
        "pass_through", "no_ignore_case", "no_getopt_compat", "no_auto_abbrev");
    # we disable auto abbreviation to reduce surprise, e.g. -a can be
    # abbreviated from common option --action. this is still not the proper
    # solution. the proper solution is to only do option parsing once, but is it
    # feasible?
    Getopt::Long::GetOptions(@go_opts);
    $log->tracef("result of GetOptions for common options: remaining argv=%s, ".
                     "actions=%s", \@ARGV, $self->{_actions});
    Getopt::Long::Configure($old_go_opts);

    if ($self->{_force_call}) {
        @ARGV = @orig_ARGV;
    }

    $log->tracef("<- parse_common_opts()");
}

sub parse_subcommand_opts {
    require Perinci::Sub::GetArgs::Argv;

    my ($self) = @_;
    my $sc = $self->{_subcommand};
    return unless $sc && $sc->{url};
    $log->tracef("-> parse_subcommand_opts()");

    my $res = $self->_pa->request(meta=>$sc->{url});
    if ($res->[0] == 200) {
        # prefill arguments using 'args' from subcommand specification, if any
        $self->{_args} = {};
        if ($sc->{args}) {
            for (keys %{ $sc->{args} }) {
                $self->{_args}{$_} = $sc->{args}{$_};
            }
        }
    } else {
        $log->warnf("Can't get metadata from %s: %d - %s", $sc->{url},
                    $res->[0], $res->[1]);
        $log->tracef("<- parse_subcommand_opts() (bailed)");
        return;
    }
    my $meta = $res->[2];
    $self->{_meta} = $meta;
    $self->_add_common_opts_after_meta;

    # also set dry-run on environment
    do { $self->{_dry_run} = 1; $ENV{VERBOSE} = 1 } if $ENV{DRY_RUN};

    # parse argv
    my $src_seen;
    my %ga_args = (
        argv                => \@ARGV,
        meta                => $meta,
        check_required_args => $self->{_check_required_args} // 1,
        allow_extra_elems   => 1,
        per_arg_json        => 1,
        per_arg_yaml        => 1,
        on_missing_required_args => sub {
            my %a = @_;
            my ($an, $aa, $as) = ($a{arg}, $a{args}, $a{spec});
            my $src = $as->{cmdline_src};
            if ($src && $as->{req}) {
                # don't complain, we will fill argument from other source
                return 1;
            } else {
                # we have no other sources, so we complain about missing arg
                say "Missing required argument: $an"
                    if $self->{_check_required_args} // 1;
            }
            0;
        },
    );
    if ($self->{_force_call}) {
        $ga_args{extra_getopts_before} = $self->{_go_specs_common};
    } else {
        $ga_args{extra_getopts_after}  = $self->{_go_specs_common};
    }
    $res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(%ga_args);

    # We load Log::Any::App rather late here, to be able to customize level via
    # --debug, --dry-run, etc.
    unless ($ENV{COMP_LINE}) {
        my $do_log = $self->{_subcommand}{log_any_app};
        $do_log //= $ENV{LOG};
        $do_log //= $self->{action_metadata}{$self->{_actions}[0]}{default_log}
            if @{ $self->{_actions} };
        $do_log //= $self->log_any_app;
        $self->_load_log_any_app if $do_log;
    }

    # we'll try giving argv to server side, but this currently means we skip
    # processing cmdline_src.
    if ($res->[0] == 502) {
        $log->debugf("Failed parsing arguments (status 502), will try to send ".
                         "argv to server");
        $self->{_getargs_result} = $res;
        $self->{_send_argv} = 1;
        return;
    }

    $self->_err("Failed parsing arguments: $res->[0] - $res->[1]")
        unless $res->[0] == 200;
    for (keys %{ $res->[2] }) {
        $self->{_args}{$_} = $res->[2]{$_};
    }
    $log->tracef("result of GetArgs for subcommand: remaining argv=%s, args=%s".
                     ", actions=%s", \@ARGV, $self->{_args}, $self->{_actions});

    # handle cmdline_src
    if (!$ENV{COMP_LINE} && ($self->{_actions}[0] // "") eq 'call') {
        my $args_p = $meta->{args} // {};
        my $stdin_seen;
        for my $an (sort keys %$args_p) {
            #$log->tracef("TMP: handle cmdline_src for arg=%s", $an);
            my $as = $args_p->{$an};
            my $src = $as->{cmdline_src};
            if ($src) {
                $self->_err(
                    "Invalid 'cmdline_src' value for argument '$an': $src")
                    unless $src =~ /\A(stdin|file|stdin_or_files)\z/;
                $self->_err(
                    "Sorry, argument '$an' is set cmdline_src=$src, but type ".
                        "is not 'str' or 'array', only those are supported now")
                    unless $as->{schema}[0] =~ /\A(str|array)\z/;
                if ($src =~ /stdin/) {
                    $self->_err("Only one argument can be specified ".
                                    "cmdline_src stdin/stdin_or_files")
                        if $stdin_seen++;
                }
                my $is_ary = $as->{schema}[0] eq 'array';
                if ($src eq 'stdin' || $src eq 'file' &&
                        ($self->{_args}{$an}//"") eq '-') {
                    $self->_err("Argument $an must be set to '-' which means ".
                                    "from stdin")
                        if defined($self->{_args}{$an}) &&
                            $self->{_args}{$an} ne '-';
                    $log->trace("Getting argument '$an' value from stdin ...");
                    $self->{_args}{$an} = $is_ary ? [<STDIN>] :
                        do { local $/; <STDIN> };
                } elsif ($src eq 'stdin_or_files') {
                    # push back argument value to @ARGV so <> can work to slurp
                    # all the specified files
                    local @ARGV = @ARGV;
                    unshift @ARGV, $self->{_args}{$an}
                        if defined $self->{_args}{$an};
                    $log->tracef("Getting argument '$an' value from ".
                                     "stdin_or_files, \@ARGV=%s ...", \@ARGV);
                    $self->{_args}{$an} = $is_ary ? [<>] : do { local $/; <> };
                } elsif ($src eq 'file') {
                    unless (exists $self->{_args}{$an}) {
                        if ($as->{req}) {
                            $self->_err(
                                "Please specify filename for argument '$an'");
                        } else {
                            next;
                        }
                    }
                    $self->_err("Please specify filename for argument '$an'")
                        unless defined $self->{_args}{$an};
                    $log->trace("Getting argument '$an' value from ".
                                    "file ...");
                    my $fh;
                    unless (open $fh, "<", $self->{_args}{$an}) {
                        $self->_err("Can't open file '$self->{_args}{$an}' ".
                                        "for argument '$an': $!")
                    }
                    $self->{_args}{$an} = $is_ary ? [<$fh>] :
                        do { local $/; <$fh> };
                }
            }
        }
    }
    $log->tracef("args after cmdline_src is processed: %s", $self->{_args});

    $log->tracef("<- _parse_subcommand_opts()");
}

# set $self->{_subcommand} for convenience, it can be taken from subcommands(),
# or, in the case of app with a single command, {name=>'', url=>$self->url()}.
sub _set_subcommand {
    my ($self) = @_;

    if ($self->subcommands) {
        my $scn;
        if (defined $self->{_selected_subcommand}) {
            $scn = $self->{_selected_subcommand};
        } elsif (defined $self->default_subcommand) {
            $scn = $self->default_subcommand;
        } elsif (@ARGV) {
            $scn = shift @ARGV;
            $self->{_scn_in_argv} = $scn;
        } else {
            goto L1;
        }
        my $sc = $self->get_subcommand($scn);
        unless ($sc) {
            if ($ENV{COMP_LINE}) {
                goto L1;
            } else {
                $self->_err(
                    "Unknown subcommand '$scn', use '".
                        $self->program_name.
                            " --subcommands' to list available subcommands");
            }
        }
        $self->{_subcommand} = $sc;
        $self->{_subcommand}{name} = $scn;
        if ($self->{_force_call}) {
            unshift @{$self->{_actions}}, 'call';
        } else {
            push @{$self->{_actions}}, 'call';
        }
    } else {
        $self->{_subcommand} = {url=>$self->url, summary=>$self->summary};
        $self->{_subcommand}{name} = '';
        if ($self->{_force_call}) {
            unshift @{$self->{_actions}}, 'call';
        } else {
            push @{$self->{_actions}}, 'call';
        }
    }
  L1:
    unshift @{$self->{_actions}}, 'completion' if $ENV{COMP_LINE};
    push @{$self->{_actions}}, 'help' if !@{$self->{_actions}};

    # unlogged, too early
    $log->tracef("actions=%s, subcommand=%s",
                 $self->{_actions}, $self->{_subcommand});
}

sub _load_log_any_app {
    my ($self) = @_;
    # Log::Any::App::init can already avoid being run twice, but we need to
    # check anyway to avoid logging starting message below twice.
    return if $self->{_log_any_app_loaded}++;
    require Log::Any::App;
    Log::Any::App::init();

    # we log this after we initialize Log::Any::App, since Log::Any::App might
    # not be loaded at all. yes, this means that this log message is printed
    # rather late and might not be the first message to be logged (see log
    # messages in run()) if user already loads Log::Any::App by herself.
    $log->debugf("Program %s started with arguments: %s",
                 $0, $self->{_orig_argv});
}

# since there's now module like Perinci::CmdLine::Server which might reuse the
# same instance for multiple run()'s/requests, use this to clean state between
# requests.
sub _init_request {
    my ($self) = @_;
    $self->{_actions} = []; # first action will be tried first, then 2nd, ...
    undef $self->{_selected_subcommand};
    undef $self->{_check_required_args};
    undef $self->{_subcommand};
    undef $self->{_args};
    undef $self->{_send_argv};
    undef $self->{_orig_argv};
    undef $self->{_getargs_result};
    undef $self->{_comp_parse_res};
    undef $self->{_go_specs_common};
    undef $self->{_scn_in_argv};
    undef $self->{_dry_run};
    undef $self->{_help_curtbl};
    undef $self->{_help_info};
    undef $self->{_help_meta};
    undef $self->{_some_subcommands_not_shown_in_help};
    undef $self->{_tx_id};
    undef $self->{_meta};
    undef $self->{_res};
    undef $self->{_fres};
    undef $self->{_compres};

    # not reset
    #$self->{_log_any_app_loaded};
}

sub run {
    my ($self) = @_;

    $log->trace("-> CmdLine's run()");

    $self->_init_request;

    #
    # workaround: detect (1) if we're being invoked for bash completion, get
    # @ARGV from parsing COMP_LINE/COMP_POINT instead, since @ARGV given by bash
    # is messed up / different
    #

    if ($ENV{COMP_LINE}) {
        require Complete::Bash;
        my ($words, $cword) = Complete::Bash::parse_cmdline();
        @ARGV = @$words;
        $self->{_comp_parse_res} = [$words, $cword]; # store for run_completion()
    }

    #
    # parse common opts first so we can catch --help, --subcommands, etc.
    #

    $self->parse_common_opts;

    #
    # find out which subcommand to run, store it in $self->{_subcommand}
    #

    $self->_set_subcommand();

    #
    # parse subcommand options, this is to give change to function arguments
    # like --help to be parsed into $self->{_args}
    #

    $self->parse_subcommand_opts unless $ENV{COMP_LINE};

    #
    # finally invoke the appropriate run_*() action method(s)
    #

    my $exit_code;
    while (@{$self->{_actions}}) {
        my $action = shift @{$self->{_actions}};
        #$log->tracef("Trying action $action");

        unless ($ENV{COMP_LINE}) {
            # determine whether to binmode(STDOUT,":utf8")
            my $utf8 = $ENV{UTF8};
            if (!defined($utf8)) {
                my $am = $self->action_metadata->{$action};
                $utf8 //= $am->{use_utf8};
            }
            if (!defined($utf8) && $self->{_subcommand}) {
                $utf8 //= $self->{_subcommand}{use_utf8};
            }
            $utf8 //= $self->use_utf8;
            if ($utf8) {
                binmode(STDOUT, ":utf8");
            }
        }

        my $meth = "run_$action";
        unless ($self->can($meth)) {
            $self->_err("Unknown action '$action'");
        }
        $log->tracef("-> %s()", $meth);
        $exit_code = $self->$meth;
        $log->tracef("<- %s(), return=%s", $meth, $exit_code);
        last if defined $exit_code;
    }
    $self->format_result;
    $self->display_result;

    $log->tracef("<- CmdLine's run(), exit code=%s", $exit_code);
    if ($self->exit) {
        $log->debugf("Program ending with exit code %d", $exit_code);
        $self->_unsetup_progress_output;
        exit $exit_code;
    } else {
        $self->_unsetup_progress_output;
        return $exit_code;
    }
}

1;
# ABSTRACT: A lightweight Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(.*)$

=head1 SYNOPSIS

In your command-line script:

 #!/usr/bin/perl
 use Perinci::CmdLine::Lite qw(run_cmdline_app);

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

 run_cmdline_app(url => '/main/foo');

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

B<NOTE: This module is experimental.>

Perinci::CmdLine::Lite (hereby P::C::Lite) module offers a lightweight (low
startup overhead, minimal dependencies) alternative to L<Perinci::CmdLine>
(hereby P::C). It offers a subset of functionality and a pretty compatible API.
The main difference is that P::C::Lite does not access code and metadata through
the L<Riap> client library L<Perinci::Access> layer, but instead access Perl
modules/packages directly. This means B<no remote URL support>, you can only
access Perl modules on the filesystem. Also not (currently) supported are:

=over

=item * Color themes

=item * Custom action

=item * Undo

=item * Logging

Something more lightweight than L<Log::Any::App> will be considered. If you want
logging, you can do something like this:

 % DEBUG=1 PERL5OPT=-MLog::Any::App

=item * Progress indicator

=item * These Rinci function metadata properties not yet supported

 x.perinci.cmdline.default_format

=item * These Rinci function argument specification properties not yet supported

 cmdline_src

=item * These Rinci result metadata properties/attributes not yet supported

 is_stream
 cmdline.display_result
 cmdline.page_result
 cmdline.pager
 cmdline.exit_code

=back

=item * These environment variables are not (yet) supported

 PERINCI_CMDLINE_COLOR_THEME
 PERINCI_CMDLINE_SERVER
 PROGRESS
 PAGER
 COLOR
 UTF8

 DEBUG, VERBOSE, QUIET, TRACE, and so on

=item * In passsing command-line object to functions, Perinci::CmdLine::Lite object is passed

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
