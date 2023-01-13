use strict;

my $resN=500;

my $resD="/home/knam/work/life_psrate/Bacteria/result";
my $output="/home/knam/work/life_psrate/Bacteria/summary.txt";

opendir my $D,$resD;
my @files=readdir($D);
close $D;

my %NUMB;
my $result="N\ti\ts\n";
foreach my $fi (@files)
{
	if($fi=~/\.$/) {next}
	my @s=split("_",$fi);
	my $popSize=$s[1];
	my $key=$popSize;

	open my $fd,"$resD/$fi";
	my @res=<$fd>;
	close $fd;

	if($#res < 20) {next}
	if($NUMB{$key} > $resN) {next}

	$NUMB{$key}++;
	$result.=getres("$key\t$NUMB{$key}",\@res);

#	print "$key $NUMB{$key}\n"
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
	(my $tag,my $resdata)=(@_);

	my $Ss;
	foreach my $line (@$resdata)
	{
		if($line=~/m2/)
		{
			$line=~s/\n//;
			$line=~/(\d+) (\d+)$/;
			my $gen=$1;
			if($gen<50000) {next}
			my @single=split(" ",$line);
			$Ss.="$tag\t$single[4]\n";

		}
	}
	return $Ss;
}














