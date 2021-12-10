package Perinci::CmdLine::PluginBase;

# put pragmas + Log::ger here
use strict 'subs', 'vars';
use warnings;

# put other modules alphabetically here
#require Perinci::CmdLine::Base;

# put global variables alphabetically here
# AUTHORITY
# DATE
# DIST
# VERSION

sub new {
    my ($class, %args) = (shift, @_);
    bless \%args, $class;
}

sub activate {
    my ($self, $wanted_event, $wanted_prio) = @_;

    my $pkg = ref($self);
    my $symtbl = \%{$pkg . "::"};

    (my $plugin_name = $pkg) =~ s/\APerinci::CmdLine::Plugin:://;

    my $meta;
  CHECK_META: {
        defined &{"$pkg\::meta"} or die "$pkg does not define meta()";
        $meta = &{"$pkg\::meta"}();
        my $v = $meta->{v}; $v = 1 unless defined $v;
        if ($v != 1) {
            die "Cannot use $pkg: meta: I only support v=1 ".
                "but the module has v=$v";
        }
    }

    # register in @Plugin_Instances
    {
        no warnings 'once';
        push @Perinci::CmdLine::Base::Plugin_Instances, $self;
    }

    for my $k (keys %$symtbl) {
        my $v = $symtbl->{$k};
        next unless ref $v eq 'CODE' || defined *$v{CODE};
        next unless $k =~ /^(before_|on_|after_)(.+)$/;

        my $meta_method = "meta_$k";
        my $methmeta = $self->can($meta_method) ? $self->$meta_method : {};

        (my $event = $k) =~ s/^on_//;

        $self->cmdline->_plugin_add_handler(
            defined $wanted_event ? $wanted_event : $event,
            $plugin_name,
            (defined $wanted_prio ? $wanted_prio :
             defined $methmeta->{prio} ? $methmeta->{prio} :
             defined $meta->{prio} ? $meta->{prio} : 50),
            sub {
                my $stash = shift;
                $self->$k($stash);
            },
        );
    }
}

sub cmdline {
    $_[0]{cmdline};
}

1;
# ABSTRACT: Base class for Perinci::CmdLine plugin

=for Pod::Coverage ^(.+)$

=head1 DESCRIPTION

This base class allows you to write handlers as methods with names
/^(before_|on_|after_)EVENT_NAME$/ and metadata like priority in the
/^meta_HANDLER/ method.
