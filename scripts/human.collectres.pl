use strict;

my $repl=500;
my $outD="/home/knam/work/life_psrate/humans";
my $output="$outD/summary.txt";

my $result="p\ti\tps\tfixT\n";
opendir my $D,"$outD/raw";
my @files=readdir($D);
close $D;

my %N;
foreach my $file (@files)
{
	unless($file=~/h/) {next}
	my $fileN="$outD/raw/$file";
	open my $fd,$fileN;
	my @res=<$fd>;
	close $fd;

	if($#res < 20) {next}

	$file=~s/\.\d+$//;
	$file=~s/h\.//;
	my $Ne=$file;

	$N{$Ne}++;
	unless($N{$Ne} > $repl)
	{
		$result.="$Ne\t$N{$Ne}\t".getres(\@res)."\n";
	}
}

open my $fd,">$output";
print $fd $result;
close $fd;

my @P=keys %N;
foreach my $prop (@P)
{
	print "$prop ::: $N{$prop}\n";
}

sub getres
{
	(my $resF)=(@_);

	my $ps=0;
	my $fixT=0;

	foreach my $line (@$resF)
	{
		if($line=~/m2/)
		{
			$line=~s/\n//;
			$line=~/(\d+) (\d+)$/;
			my $gen=$1;
			my $fix=$2;

			if($gen<50000) {next}
			$fixT+=($fix-$gen);
			$ps++;
		}
	}

	return "$ps\t$fixT";
}














