require(plyr)
require(ggplot2)
require(splitstackshape)

dCt<-read.csv("deltactactin.csv", header=T)
dCt<-cSplit(dCt,"SAMPLE_ID", sep= "_", drop=F)
dCt<-rename(dCt,replace=c("SAMPLE_ID_1"="Pop","SAMPLE_ID_2"="Treat","SAMPLE_ID_3"="Sample"))

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

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=CARMlog,fill=Pop))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=TLRlog,fill=Pop))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=H2AVlog,fill=Pop))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=PGRPlog,fill=Pop))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=HSP70log,fill=Pop))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=BMP2log,fill=Pop))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=GRB2log,fill=Pop))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=PGEEP4log,fill=Pop))

ggplot(data=dCt)+geom_boxplot(aes(x=Treat, y=CRAFlog,fill=Pop))



