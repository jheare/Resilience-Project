require(plyr)
require(ggplot2)
require(splitstackshape)

dCt<-read.csv("CTvalues83115.csv", header=T)
dCt<-cSplit(dCt,"SAMPLE_ID", sep= "_", drop=F)
dCt<-rename(dCt,replace=c("SAMPLE_ID_1"="Pop","SAMPLE_ID_2"="Treat","SAMPLE_ID_3"="Sample"))

dCt$CARM<-(dCt$CarmmeanCt/dCt$Actinmeanct)
dCt$TLR<-(dCt$TLRaverage/dCt$Actinmeanct)
dCt$CRAF<-(dCt$CRAFctaverage/dCt$Actinmeanct)
dCt$H2AV<-(dCt$H2AVavgct/dCt$Actinmeanct)
dCt$PGRP<-(dCt$PGRPaverage/dCt$Actinmeanct)
dCt$HSP70<-(dCt$HSP70averageCt/dCt$Actinmeanct)
dCt$BMP2<-(dCt$BMP2average/dCt$Actinmeanct)
dCt$GRB2<-(dCt$GRB2average/dCt$Actinmeanct)
dCt$PGEEP4<-(dCt$PGEEP4ctav/dCt$Actinmeanct)

dCt$CARMlog<-log(dCt$CARM)
dCt$TLRlog<-log(dCt$TLR)
dCt$H2AVlog<-log(dCt$H2AV)
dCt$PGRPlog<-log(dCt$PGRP)
dCt$HSP70log<-log(dCt$HSP70)
dCt$BMP2log<-log(dCt$BMP2)
dCt$GRB2log<-log(dCt$GRB2)
dCt$PGEEP4log<-log(dCt$PGEEP4)
dCt$CRAFlog<-log(dCt$CRAF)

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

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=CARMlog,fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.3, label=c("AB  AB  AB","A  A  A","A  A  B"), size=10)

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=TLRlog, fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.6, label=c("AC  AC  AC","A  AB  AB","AB  BC  BC"), size=10)

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=H2AVlog,fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.3, label=c("AB  AB  A","AB  AB  AB","AB  B  B"), size=10)

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=PGRPlog,fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.5, label=c("AB  A  AB","A  A  A","AB  AB  B"), size=10)

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=HSP70log,fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.5, label=c("AB  AB  AB","AB  AB  A","AB  AB  B"), size=10)

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=BMP2log,fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.2, label=c("AB  AB  B","AB  AB  A","AB  AB  AB"), size=10)

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=GRB2log,fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.2, label=c("AB  A  A","AB  A  B","A  AB  AB"), size=10)

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=PGEEP4log,fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.35, label=c("A  A  A","A  A  A","A  A  A"), size=10)

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=CRAFlog,fill=Pop))+
  annotate("text",x=c("C","M","T"), y=0.3, label=c("A  A  A","A  A  A","A  A  A"), size=10)



