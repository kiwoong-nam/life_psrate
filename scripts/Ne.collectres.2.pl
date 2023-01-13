use strict;

my $resN=500;

my $resD="/home/knam/work/life_psrate/Ne/result2";
my $output="/home/knam/work/life_psrate/Ne/summary2.txt";

opendir my $D,$resD;
my @files=readdir($D);
close $D;

my %NUMB;
my $result="N\ti\tps\tfixT\n";
foreach my $fi (@files)
{
	if($fi=~/\.$/) {next}
	$fi=~s/^s//;

	my @s=split("_",$fi);
	my $popSize=$s[0];
	my $key="$popSize";

	open my $fd,"$resD/s$fi";
	my @es=<$fd>;
	close $fd;

	if($#es < 20) {next}

	my $counts=getres(@es);
	
	if($NUMB{$key} > $resN) {next}
	$NUMB{$key}++;

	$result.="$key\t$NUMB{$key}\t$counts\n";
}

for(my $i=1;$i<11;$i++)
{
	my $key=$i*2000;
	print "$key $NUMB{$key}\n"
}

open my $fd,">$output";
print $fd $result;
close $fd;

sub getres
{
	(my @ESSS)=(@_);
	my $ps=0;
	my $fixT=0;

	foreach my $line (@ESSS)
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














