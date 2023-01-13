library("ggplot2")
library(gridExtra)
library(grid)

Output='/home/kiwoong/Projects/life_psrate/paper/F2.pdf'

v=read.table('/home/kiwoong/Projects/life_psrate/Bacteria/summary.txt',header=T)

vn=aggregate(v[3],by=list(v$N),length)
colnames(vn)=c('N','ps')

sp=function(ne,nps)
{
	nes=sort(ne)
	npss=nps[order(ne)]
	tests=smooth.spline(nes,npss,df=4)
	bf=fitted(tests)
	bf
}
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

vn$fit=sp(vn$N,vn$ps)

v$N=as.factor(v$N)

vs.1=subset(v,s<0.01 & s > 0)
vs.2=subset(v,s<0.02 & s >= 0.01)
vs.3=subset(v,s<0.03 & s >= 0.02)
vs.4=subset(v,s<0.04 & s >= 0.03)
vs.5=subset(v,s<0.05 & s >= 0.04)
vs.6=subset(v,s<0.06 & s >= 0.05)
vs.7=subset(v,s<0.07 & s >= 0.06)
vs.8=subset(v,s<0.08 & s >= 0.07)
vs.9=subset(v,s<0.09 & s >= 0.08)
vs.10=subset(v,s<0.10 & s >= 0.09)
vs.11=subset(v,s<0.11 & s >= 0.10)
vs.12=subset(v,s<0.12 & s >= 0.11)
vs.13=subset(v,s<0.13 & s >= 0.12)
vs.14=subset(v,s<0.14 & s >= 0.13)
vs.15=subset(v,s<0.15 & s >= 0.14)
vs.16=subset(v,s<0.16 & s >= 0.15)
vs.17=subset(v,s<0.17 & s >= 0.16)
vs.18=subset(v,s<0.18 & s >= 0.17)
vs.19=subset(v,s<0.19 & s >= 0.18)
vs.20=subset(v,s<0.20 & s >= 0.19)

vg.1=aggregate(vs.1[1],by=list(vs.1$N),length)
vg.2=aggregate(vs.2[1],by=list(vs.2$N),length)
vg.3=aggregate(vs.3[1],by=list(vs.3$N),length)
vg.4=aggregate(vs.4[1],by=list(vs.4$N),length)
vg.5=aggregate(vs.5[1],by=list(vs.5$N),length)
vg.6=aggregate(vs.6[1],by=list(vs.6$N),length)
vg.7=aggregate(vs.7[1],by=list(vs.7$N),length)
vg.8=aggregate(vs.8[1],by=list(vs.8$N),length)
vg.9=aggregate(vs.9[1],by=list(vs.9$N),length)
vg.10=aggregate(vs.10[1],by=list(vs.10$N),length)
vg.11=aggregate(vs.11[1],by=list(vs.11$N),length)
vg.12=aggregate(vs.12[1],by=list(vs.12$N),length)
vg.13=aggregate(vs.13[1],by=list(vs.13$N),length)
vg.14=aggregate(vs.14[1],by=list(vs.14$N),length)
vg.15=aggregate(vs.15[1],by=list(vs.15$N),length)
vg.16=aggregate(vs.16[1],by=list(vs.16$N),length)
vg.17=aggregate(vs.17[1],by=list(vs.17$N),length)
vg.18=aggregate(vs.18[1],by=list(vs.18$N),length)
vg.19=aggregate(vs.19[1],by=list(vs.19$N),length)
vg.20=aggregate(vs.20[1],by=list(vs.20$N),length)


colnames(vg.1)=c('N','counts')
colnames(vg.2)=c('N','counts')
colnames(vg.3)=c('N','counts')
colnames(vg.4)=c('N','counts')
colnames(vg.5)=c('N','counts')
colnames(vg.6)=c('N','counts')
colnames(vg.7)=c('N','counts')
colnames(vg.8)=c('N','counts')
colnames(vg.9)=c('N','counts')
colnames(vg.10)=c('N','counts')
colnames(vg.11)=c('N','counts')
colnames(vg.12)=c('N','counts')
colnames(vg.13)=c('N','counts')
colnames(vg.14)=c('N','counts')
colnames(vg.15)=c('N','counts')
colnames(vg.16)=c('N','counts')
colnames(vg.17)=c('N','counts')
colnames(vg.18)=c('N','counts')
colnames(vg.19)=c('N','counts')
colnames(vg.20)=c('N','counts')


vg.1$s='0-0.01'
vg.2$s='0.01-0.02'
vg.3$s='0.02-0.03'
vg.4$s='0.03-0.04'
vg.5$s='0.04-0.05'
vg.6$s='0.05-0.06'
vg.7$s='0.06-0.07'
vg.8$s='0.07-0.08'
vg.9$s='0.08-0.09'
vg.10$s='0.09-0.10'
vg.11$s='0.10-0.11'
vg.12$s='0.11-0.12'
vg.13$s='0.12-0.13'
vg.14$s='0.13-0.14'
vg.15$s='0.14-0.15'
vg.16$s='0.15-0.16'
vg.17$s='0.16-0.17'
vg.18$s='0.17-0.18'
vg.19$s='0.18-0.19'
vg.20$s='0.19-0.20'


vg.a=rbind(vg.1,vg.2,vg.3,vg.4,vg.5,vg.6,vg.7,vg.8,vg.9,vg.10,vg.11,vg.12)
vg.b=rbind(vg.13,vg.14,vg.15,vg.16,vg.17,vg.18,vg.19,vg.20)

vg=rbind(vg.a,vg.b)

#################

x=c(1:300)/200
y=1/0.087*exp(-x/0.087)
p=2*x*y

dfe=data.frame(x=x,y=y,p=p)
thr=subset(dfe,p==max(p))$x

######################################

getmode <- function(a) {
	ht=hist(a,breaks=100)
	as=data.frame(mid=ht$mids,dt=ht$density)
	subset(as,dt==max(as$dt))$mi
}

MODES=data.frame()

for(i in 1:10)
{
	Ne=i*2000
	ss=subset(v,N==Ne)$s
	mode=getmode(ss)

	MODES=rbind(MODES,data.frame(N=Ne,mode=getmode(subset(v,N==Ne)$s)))
}

with(MODES,cor.test(N,mode))

#> with(MODES,cor.test(N,mode))
#
#	Pearson's product-moment correlation
#
#data:  N and mode
#t = 9.5519, df = 8, p-value = 1.194e-05
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.8307665 0.9904957
#sample estimates:
#      cor 
#0.9588466 

#####################################

p4=ggplot(dfe,aes(x=x,y=y))+geom_point(size=0.5)+theme_bw()+xlab("s")+ylab("probability density")+xlab('selection coefficient')+ggtitle('A')+ theme(axis.text.x = element_text(angle = 45 ,hjust = 1),plot.title = element_text(hjust = -0.12,size=23))

p5=ggplot(dfe,aes(x=x,y=p))+geom_point(size=0.5)+theme_bw()+xlab("s")+ylab("2s X probability density")+xlab('selection coefficient')+ggtitle(' ')+ theme(axis.text.x = element_text(angle = 45 ,hjust = 1),plot.title = element_text(hjust = -0.12,size=23))+ geom_vline(xintercept = thr, color = "red",size=1)

p6=ggplot(v,aes(x=s,fill=N))   +  geom_density(alpha=0.4)+theme_bw()+ geom_vline(xintercept = thr, color = "red",size=1)+xlab('selection coefficient')+ggtitle('B')+ theme(axis.text.x = element_text(angle = 45 ,hjust = 1),plot.title = element_text(hjust = -0.12,size=23))

p2=ggplot(vg.a,aes(x=s,y=counts,fill=N))+geom_bar(stat='identity',position='dodge')+theme_bw()+ylab("The number of positively selected sites")+xlab('selection coefficient')+ggtitle(' ')+ theme(axis.text.x = element_text(angle = 45 ,hjust = 1,size=6),plot.title = element_text(hjust = -0.12,size=23))

p3=ggplot(v,aes(x=s,fill=N))   +  geom_histogram(position="dodge",bins=50)+theme_bw()+xlab('selection coefficient')+ggtitle('D')+ theme(axis.text.x = element_text(angle = 45 ,hjust = 1),plot.title = element_text(hjust = -0.20,size=23),legend.position = "none")

p7=ggplot(MODES,aes(x=N,y=mode))   +  geom_point()+xlab('N')+ggtitle(' ')+ylab('mode of the distribution')+theme_bw()+theme(plot.title = element_text(hjust = -0.05,size=23))

p8=ggplot(vn)+geom_point(aes(x=N,y=ps))+theme_bw()+ylab("number of positively selected sites")+scale_y_continuous(limits = c(0, NA))+ggtitle('C')+ theme(plot.title = element_text(hjust = -0.12,size=23))

pdf(Output,width=18/1.2,height=10/1.2)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1000)))
print(p4, vp = vplayout(1,c(1:180)))
print(p5, vp = vplayout(1,c(181:380)))
print(p6, vp = vplayout(1,c(431:770)))
print(p7, vp = vplayout(1,c(771:1000)))
print(p8, vp = vplayout(2,c(1:300)))
print(p3, vp = vplayout(2,c(351:690)))
print(p2, vp = vplayout(2,c(691:1000)))
dev.off()


# thr
#[1] 0.085


