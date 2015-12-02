package Perinci::CmdLine::Util::Config;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use PERLANCAR::File::HomeDir qw(get_my_home_dir);

our %SPEC;

$SPEC{get_default_config_dirs} = {
    v => 1.1,
    args => {},
};
sub get_default_config_dirs {
    my @dirs;
    local $PERLANCAR::File::HomeDir::DIE_ON_FAILURE = 1;
    my $home = get_my_home_dir();
    if ($^O eq 'MSWin32') {
        push @dirs, $home;
    } else {
        push @dirs, "$home/.config", $home, "/etc";
    }
    \@dirs;
}

$SPEC{read_config} = {
    v => 1.1,
    args => {
        config_paths => {},
        config_filenames => {},
        config_dirs => {},
        program_name => {},
    },
};
sub read_config {
    require Config::IOD::Reader;

    my %args = @_;

    my $config_dirs = $args{config_dirs} // get_default_config_dirs();

    my $paths;
    if ($args{config_paths}) {
        $paths = $args{config_paths};
    } else {
        my $name = $args{config_filename} //
            $args{program_name} . ".conf";
        for my $dir (@$config_dirs) {
            my $path = "$dir/" . $name;
            push @$paths, $path if -e $path;
        }
    }

    my $reader = Config::IOD::Reader->new;
    my %res;
    my @read;
    for my $path (@$paths) {
        my $hoh = $reader->read_file($path);
        push @read, $path;
        for my $section (keys %$hoh) {
            my $hash = $hoh->{$section};
            for (keys %$hash) {
                $res{$section}{$_} = $hash->{$_};
            }
        }
    }
    [200, "OK", \%res, {'func.read_files' => \@read}];
}

$SPEC{get_args_from_config} = {
    v => 1.1,
    args => {
        r => {},
        config => {},
        args => {},
        subcommand_name => {},
        config_profile => {},
        common_opts => {},
        meta => {},
        meta_is_normalized => {},
    },
};
sub get_args_from_config {
    my %fargs = @_;

    my $r       = $fargs{r};
    my $conf    = $fargs{config};
    my $scn     = $fargs{subcommand_name} // '';
    my $profile = $fargs{config_profile};
    my $args    = $fargs{args} // {};
    my $copts   = $fargs{common_opts};
    my $meta    = $fargs{meta};
    my $found;

    unless ($fargs{meta_is_normalized}) {
        require Perinci::Sub::Normalize;
        $meta = Perinci::Sub::Normalize::normalize_function_metadata($meta);
    }

    # put GLOBAL before all other sections
    my @sections = sort {
        ($a eq 'GLOBAL' ? 0:1) <=> ($b eq 'GLOBAL' ? 0:1) ||
            $a cmp $b
        } keys %$conf;

    my %seen_profiles; # for debugging message
    for my $section (@sections) {
        my %keyvals;
        for my $word (split /\s+/, ($section eq 'GLOBAL' ? '' : $section)) {
            if ($word =~ /(.+)=(.*)/) {
                $keyvals{$1} = $2;
            } else {
                # old syntax, will be removed sometime in the future
                $keyvals{subcommand} = $word;
            }
        }
        $seen_profiles{$keyvals{profile}}++ if defined $keyvals{profile};

        my $sect_scn     = $keyvals{subcommand} // '';
        my $sect_profile = $keyvals{profile};

        if (length $scn) {
            next if length($sect_scn) && $sect_scn ne $scn;
        } else {
            next if length $sect_scn;
        }
        if (defined $profile) {
            next if defined($sect_profile) && $sect_profile ne $profile;
            $found = 1 if defined($sect_profile) && $sect_profile eq $profile;
        } else {
            next if defined($sect_profile);
        }

        my $as = $meta->{args} // {};
        for my $k (keys %{ $conf->{$section} }) {
            my $v = $conf->{$section}{$k};
            if ($copts->{$k} && $copts->{$k}{is_settable_via_config}) {
                my $sch = $copts->{$k}{schema};
                if ($sch) {
                    require Data::Sah::Normalize;
                    $sch = Data::Sah::Normalize::normalize_schema($sch);
                    # since IOD might return a scalar or an array (depending on
                    # whether there is a single param=val or multiple param=
                    # lines), we need to arrayify the value if the argument is
                    # expected to be an array.
                    if (ref($v) ne 'ARRAY' && $sch->[0] eq 'array') {
                        $v = [$v];
                    }
                }
                $copts->{$k}{handler}->(undef, $v, $r);
            } else {
                # when common option clashes with function argument name, user
                # can use NAME.arg to refer to function argument.
                $k =~ s/\.arg\z//;

                # since IOD might return a scalar or an array (depending on
                # whether there is a single param=val or multiple param= lines),
                # we need to arrayify the value if the argument is expected to
                # be an array.
                if (ref($v) ne 'ARRAY' && $as->{$k} && $as->{$k}{schema} &&
                        $as->{$k}{schema}[0] eq 'array') {
                    $v = [$v];
                }
                $args->{$k} = $v;
            }
        }
    }
    $log->tracef("[pericmd] Seen config profiles: %s",
                 [sort keys %seen_profiles]);

    [200, "OK", $args, {'func.found'=>$found}];
}

1;
# ABSTRACT: Utility routines related to config files
