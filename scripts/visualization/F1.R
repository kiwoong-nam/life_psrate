library("ggplot2")
library(gridExtra)
library(grid)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

Output='/home/kiwoong/Projects/life_psrate/paper/F1.pdf'

########################
s=0.02

vg=data.frame()

N=c(1:1000)*10
f= (1 - exp(-2 * s)) / (1 - exp(-4 * N * s))
v=data.frame(N,f)

######################


v.H=read.table("/home/kiwoong/Projects/life_psrate/humans/summary.txt",header=T)

vg.H=aggregate(v.H[c(3,4)],by=list(v.H$p),mean)
colnames(vg.H)[1]='P'

n_gens = 50000
locus_size = 1000000
N=2000
mu=1.45e-08
s=0.02/2

vg.H$p_fix=with(vg.H, (1 - exp(-2 * s)) / (1 - exp(-4 * N * s)))

vg.H$exp=with(vg.H,mu * locus_size * n_gens * 2 * N * P * p_fix)

vs.H=rbind(data.frame(P=vg.H$P,ps=vg.H$ps,category='simulation'),data.frame(P=vg.H$P,ps=vg.H$exp,category='expectation'))
vss.H=subset(vs.H, P > 1e-07)

##############################


v1=read.table('/home/kiwoong/Projects/life_psrate/Ne/summary.txt',header=T)
v2=read.table('/home/kiwoong/Projects/life_psrate/Ne/summary2.txt',header=T)

vg1=aggregate(v1[c(3,4)],by=list(v1$N),mean)
vg2=aggregate(v2[c(3,4)],by=list(v2$N),mean)

colnames(vg1)[1]='N'
colnames(vg2)[1]='N'

n_gens = 80000
locus_size = 1000000
mu=1.45e-08
s=0.02/2
P=0.001

vg1$p_fix=with(vg1, (1 - exp(-2 * s)) / (1 - exp(-4 * N * s)))
vg1$exp=with(vg1,mu * locus_size * n_gens * 2 * N * P * p_fix)
vg1$propF=with(vg1,ps/(mu * locus_size * n_gens * 2 * N * P ))
vg1$timeF=with(vg1,fixT/ps)

vg2$p_fix=with(vg2, (1 - exp(-2 * s)) / (1 - exp(-4 * N * s)))
vg2$exp=with(vg2,mu * locus_size * n_gens * 2 * N * P * p_fix)
vg2$propF=with(vg2,ps/(mu * locus_size * n_gens * 2 * N * P ))
vg2$timeF=with(vg2,fixT/ps)

sp=function(ne,nps)
{
	nes=sort(ne)
	npss=nps[order(ne)]
	tests=smooth.spline(nes,npss,df=4)
	bf=fitted(tests)
	bf
}

v.ps.1=data.frame(N=vg1$N,ps=vg1$ps,category="simulation, recombination rate = 1.13e-08")
v.ps.1$fitted=sp(v.ps.1$N,v.ps.1$ps)

v.ps.2=data.frame(N=vg2$N,ps=vg2$ps,category="simulation, recombination rate = 2.26e-08")
v.ps.2$fitted=sp(v.ps.2$N,v.ps.2$ps)

v.ps.3=data.frame(N=vg1$N,ps=vg1$exp,category="expectation")
v.ps.3$fitted=sp(v.ps.3$N,v.ps.3$ps)

v.ps=rbind(v.ps.1,v.ps.2,v.ps.3)


v.f1=data.frame(N=vg1$N,f=vg1$propF,category='recombination rate = 1.13e-08')
v.f1$fitted=sp(v.f1$N,v.f1$f)

v.f2=data.frame(N=vg2$N,f=vg2$propF,category='recombination rate = 2.26e-08')
v.f2$fitted=sp(v.f2$N,v.f2$f)

v.f=rbind(v.f1,v.f2)

########################

pA=ggplot(v,aes(x=N,y=f))+geom_line()+ylim(0,0.08)+ylab('probability of fixaton')+ggtitle("s=0.02")+theme_bw()+ggtitle('A')+xlab('N')+theme(plot.title = element_text(hjust = -0.12,size=20))

pB= ggplot(vss.H,aes(x=P,y=ps,colour=category))+geom_point()+geom_line()+ scale_x_continuous(trans='log10')+theme_bw()+ylab('the number of positively selected sites')+xlab('the proportion of beneficial mutation')+scale_color_manual(values = c("red","blue"))+theme(legend.title=element_blank(),legend.position = c(0.2, 0.8))+ggtitle('B')+theme(plot.title = element_text(hjust = -0.12,size=20))+scale_y_continuous(trans='log10')

pC=ggplot(v.ps,aes(x=N,y=ps,colour=category))+geom_point()+geom_line(aes(x = N, y = fitted,col=category))+theme_bw()+ylab("number of positively selected sites")+theme(legend.title=element_blank(),legend.position = c(0.35, 0.8))+scale_color_manual(values = c("red","green","blue"))+ggtitle('C')+theme(plot.title = element_text(hjust = -0.12,size=20))+xlab('N')

pD=ggplot(v.f,aes(x=N,y=f,colour=category))+geom_point()+geom_line(aes(x = N, y = f))+theme_bw()+ylab("probability of fixation")+scale_y_continuous(limits = c(0, NA))+theme(legend.title=element_blank(),legend.position = c(0.3, 0.2))+scale_color_manual(values = c("red","green"))+ggtitle('D')+theme(plot.title = element_text(hjust = -0.12,size=20))+xlab('N')

(vss,aes(x=P,y=ps,colour=category))+geom_point()+geom_line()+ scale_x_continuous(trans='log10')+theme_bw()+ylab('the number of positively selected sites')+xlab('the proportion of beneficial mutation')+scale_color_manual(values = c("red","blue"))+theme(legend.title=element_blank(),legend.position = c(0.2, 0.8)

pdf(Output,width=10,height=10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2 )))
print(pA, vp = vplayout(1,1))
print(pB, vp = vplayout(1,2))
print(pC, vp = vplayout(2,1))
print(pD, vp = vplayout(2,2))
dev.off()



