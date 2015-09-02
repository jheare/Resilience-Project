#Necessary Packages to manipulate data and plot values. 
require(plyr)
require(ggplot2)
require(splitstackshape)

#Read in mean Ct value table
dCt<-read.csv("CTvalues83115.csv", header=T)
#Split SAMPLE_ID column to create columns for population, treatment, and sample number
dCt<-cSplit(dCt,"SAMPLE_ID", sep= "_", drop=F)
#rename columns appropriately
dCt<-rename(dCt,replace=c("SAMPLE_ID_1"="Pop","SAMPLE_ID_2"="Treat","SAMPLE_ID_3"="Sample"))

#divide each target of interest by the mean Ct value of the Actin Normalizing gene
dCt$CARM<-(dCt$CarmmeanCt/dCt$Actinmeanct)
dCt$TLR<-(dCt$TLRaverage/dCt$Actinmeanct)
dCt$CRAF<-(dCt$CRAFctaverage/dCt$Actinmeanct)
dCt$H2AV<-(dCt$H2AVavgct/dCt$Actinmeanct)
dCt$PGRP<-(dCt$PGRPaverage/dCt$Actinmeanct)
dCt$HSP70<-(dCt$HSP70averageCt/dCt$Actinmeanct)
dCt$BMP2<-(dCt$BMP2average/dCt$Actinmeanct)
dCt$GRB2<-(dCt$GRB2average/dCt$Actinmeanct)
dCt$PGEEP4<-(dCt$PGEEP4ctav/dCt$Actinmeanct)

#log transform the data to develop normality in data
dCt$CARMlog<-log(dCt$CARM)
dCt$TLRlog<-log(dCt$TLR)
dCt$H2AVlog<-log(dCt$H2AV)
dCt$PGRPlog<-log(dCt$PGRP)
dCt$HSP70log<-log(dCt$HSP70)
dCt$BMP2log<-log(dCt$BMP2)
dCt$GRB2log<-log(dCt$GRB2)
dCt$PGEEP4log<-log(dCt$PGEEP4)
dCt$CRAFlog<-log(dCt$CRAF)

#Run ANOVA's on all log transformed data as well as Tukey's Honestly Significant Difference post hoc test
CARM<-aov(CARMlog~Pop+Treat+Pop:Treat, data=dCt)
CARM
TukeyHSD(CARM)

TLR<-aov(TLRlog~Pop+Treat+Pop:Treat, data=dCt)
TLR
TukeyHSD(TLR)

H2AV<-aov(H2AVlog~Pop+Treat+Pop:Treat, data=dCt)
H2AV
TukeyHSD(H2AV)

PGRP<-aov(PGRPlog~Pop+Treat+Pop:Treat, data=dCt)
PGRP
TukeyHSD(PGRP)

HSP70<-aov(HSP70log~Pop+Treat+Pop:Treat, data=dCt)
HSP70
TukeyHSD(HSP70)

BMP2<-aov(BMP2log~Pop+Treat+Pop:Treat, data=dCt)
BMP2
TukeyHSD(BMP2)

GRB2<-aov(GRB2log~Pop+Treat+Pop:Treat, data=dCt)
GRB2
TukeyHSD(GRB2)

PGEEP4<-aov(PGEEP4log~Pop+Treat+Pop:Treat, data=dCt)
PGEEP4
TukeyHSD(PGEEP4)

CRAF<-aov(CRAFlog~Pop+Treat+Pop:Treat, data=dCt)
CRAF
TukeyHSD(CRAF)

#graph all raw mean Ct values to produce boxplots to visualize data

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=CARM,fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  annotate("text",x=c("C","M","T"), y=1.5, label=c("A", "B", "A"), size=10)+
  annotate("text",x=c(2.25,3.25), y=1.27, label=c("*","*"), size=12)+
  labs(x="Treatment", y=expression(paste("CARM Expression (",Delta,"Ct)")))


ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=TLR, fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  annotate("text",x=c("C","M","T"), y=1.5, label=c("A", "B", "A"), size=10)+
  labs(x="Treatment", y=expression(paste("TLR Expression (",Delta,"Ct)")))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=H2AV,fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  annotate("text",x=c("C","M","T"), y=1.5, label=c("A", "A", "B"), size=10)+
  annotate("text",x=c(1.25,3.25), y=1.27, label=c("*","*"), size=12)+
  labs(x="Treatment", y=expression(paste("H2AV Expression (",Delta,"Ct)")))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=PGRP,fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  annotate("text",x=c("C","M","T"), y=1.8, label=c("A", "A", "B"), size=10)+
  annotate("text",x=c(2.25,3.25), y=1.67, label=c("*","*"), size=12)+
  labs(x="Treatment", y=expression(paste("PGRP Expression (",Delta,"Ct)")))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=HSP70,fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  annotate("text",x=c("C","M","T"), y=1.7, label=c("A", "A", "B"), size=10)+
  annotate("text",x=c(2.25,3.25), y=1.57, label=c("*","*"), size=12)+
  labs(x="Treatment", y=expression(paste("HSP70 Expression (",Delta,"Ct)")))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=BMP2,fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  annotate("text",x=c(1.25,2.25), y=1.27, label=c("*","*"), size=12)+
  labs(x="Treatment", y=expression(paste("BMP2 Expression (",Delta,"Ct)")))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=GRB2,fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  annotate("text",x=c(1.25,2.25), y=1.27, label=c("*","*"), size=12)+
  annotate("text",x=c(2,2.25), y=1.25, label=c("#","#"), size=7)+
  labs(x="Treatment", y=expression(paste("GRB2 Expression (",Delta,"Ct)")))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=PGEEP4,fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  labs(x="Treatment", y=expression(paste("PGEEP4 Expression (",Delta,"Ct)")))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=CRAF,fill=Pop))+theme_bw()+
  scale_fill_manual(values=c("#CCCCCC","#999999","#666666"),
                    labels=c("Dabob Bay","Fidalgo Bay","Oyster Bay"))+
  guides(fill=guide_legend(title="Population"))+
  theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=25), axis.title.y=element_text(size=25),
        legend.position=c(.1,.1),panel.grid.major=element_blank(),
        legend.key=element_rect(fill=NA))+
  ylim(c(0.7,1.9))+scale_x_discrete(labels=c("Control","Mechanical","Temperature"))+
  annotate("text",x=c("C","M","T"), y=1.5, label=c("A", "B", "AB"), size=10)+
  labs(x="Treatment", y=expression(paste("CRAF Expression (",Delta,"Ct)")))




