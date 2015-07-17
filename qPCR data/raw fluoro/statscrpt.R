require(qpcR)
require(plyr)
require(ggplot2)
require(splitstackshape)


rep2<-read.csv("HSPb11rawfluoro.csv", header = T)
rep2$X<-NULL
rep2<-rename(rep2, c("Cycle" = "Cycles", "A1" = "H_C_1", "A2" = "N_C_1",
                     "A3"= "S_C_1", "A4"="H_T_1", "A5"="N_T_1","A6"="S_T_1",
                     "A7"="NT_C_1","B1" = "H_C_2", "B2" = "N_C_2","B3"= "S_C_2",
                     "B4"="H_T_2", "B5"="N_T_2", "B6"="S_T_2","B7"="NT_C_2",
                     "C1" = "H_C_3", "C2" = "N_C_3","C3"= "S_C_3","C4"="H_T_3",
                     "C5"="N_T_3", "C6"="S_T_3", "C7"="NT_C_3","D1" = "H_C_4",
                     "D2" = "N_C_4","D3"= "S_C_4", "D4"="H_T_4", "D5"="N_T_4",
                     "D6"="S_T_4", "D7"="NT_C_4","E1" = "H_C_5", "E2" = "N_C_5",
                     "E3"= "S_C_5", "E4"="H_T_5", "E5"="N_T_5", "E6"="S_T_5",
                     "F1" = "H_C_6", "F2" = "N_C_6","F3"= "S_C_6", "F4"="H_T_6",
                     "F5"="N_T_6", "F6"="S_T_6","G1" = "H_C_7", "G2" = "N_C_7",
                     "G3"= "S_C_7", "G4"="H_T_7", "G5"="N_T_7", "G6"="S_T_7",
                     "H1" = "H_C_8", "H2" = "N_C_8","H3"= "S_C_8", "H4"="H_T_8",
                     "H5"="N_T_8", "H6"="S_T_8"))

rep2ct<-pcrbatch(rep2, fluo=NULL)

rep2res<-setNames(data.frame(t(rep2ct)),rep2ct[,1])
rep2res<-rep2res[-1,]

rep2res$Names<-rownames(rep2res)

rep2res2<-cSplit_f(rep2res, splitCols=c("Names"), sep="_", drop = F)

rep2res2<-rename(rep2res2, c("Names_1"="Pop", "Names_2"="Treat", "Names_3"="Sample"))

rep2res2$Gene<-rep("H2AV", length(rep2res2))

rep2res2$sig.eff<-as.numeric(as.character(rep2res2$sig.eff))
rep2res2$sig.cpD2<-as.numeric(as.character(rep2res2$sig.cpD2))


ggplot(rep2res2, aes(x=Names,y=sig.cpD2, fill=Pop))+geom_bar(stat="identity")

expr<-function(x,y){
  newVar<-(1+x)^y
  1/newVar
}

rep2res2$expression<-expr(rep2res2$sig.eff, rep2res2$sig.cpD2)

rep2res2<-rep2res2[which(rep2res2$Pop!=c("NT")),]

rep2res2<-rep2res2[rep2res2$expression<=.000000001,]

ggplot(rep2res2, aes(x=Names,y=expression, fill=Pop))+geom_bar(stat="identity")
ggplot(rep2res2, aes(x=Treat, y=expression, fill=Pop))+geom_boxplot()


fit<-aov(expression~Pop+Treat+Pop:Treat,data=rep2res2)
fit
TukeyHSD(fit)

fit2<-aov(expression~Pop, data=rep2res2[Treat=="C"])
fit2
TukeyHSD(fit2)

fit3<-aov(expression~Pop, data=rep2res2[Treat=="T"])
fit3
TukeyHSD(fit3)

fit4<-t.test(expression~Treat, data=rep2res2[Pop=="H"])
fit4

fit5<-t.test(expression~Treat, data=rep2res2[Pop=="N"])
fit5

fit6<-t.test(expression~Treat, data=rep2res2[Pop=="S"])
fit6


