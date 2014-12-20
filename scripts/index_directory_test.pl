#! /usr/bin/perl -w
    eval 'exec /usr/bin/perl -S $0 ${1+"$@"}'
        if 0; #$running_under_some_shell

use strict;
use File::Find ();

# Set the variable $File::Find::dont_use_nlink if you're using AFS,
# since AFS cheats.

# for the convenience of &wanted calls, including -eval statements:
use vars qw/*name *dir *prune/;
*name   = *File::Find::name;
*dir    = *File::Find::dir;
*prune  = *File::Find::prune;

use Data::Dumper qw(Dumper);
use JSON;
use HTTP::Tiny;
my $struct;
my $prefix;
my $IndexId;
sub wanted {
	return if /^\.{1,2}$/;
	$name =~ s/^$prefix//;
	my @parts = split('/', $name);
	my $node = $struct;
	while( my $part = shift @parts ) {
		if (@parts) {
			$node->{$part} ||={};
		}
		else {
			if (-f $_) {
				$node->{$part} = [];
				my $data = {};
				@{$data}{qw(mode size atime mtime ctime)} = (stat($_))[2,7,8,9,10];
				$data->{mode} &&= sprintf("%o", $data->{mode});
				$data->{time} = $IndexId;
				push(@{$node->{$part}}, $data);
			}
			else {
				$node->{$part} ||={};
			}
		}
		$node = $node->{$part};
	}
}

for my $path (@ARGV) {
	$struct = {};
	$prefix = '/'.join('/', grep { $_ } split('/', $path)).'/';
	my $file = join('-', grep { $_ } split('/', $path));
	$IndexId = time;
	File::Find::find({wanted => \&wanted}, $prefix);


print "$file\n";
	HTTP::Tiny->new->put("http://127.0.0.1:8080/tree/$file", { content => JSON->new->encode($struct) });
#	open(my $fh, '>', $file);
#	print $fh JSON->new->encode($struct);
#	print JSON->new->pretty->encode($struct);
#	close($fh);
}
exit;


