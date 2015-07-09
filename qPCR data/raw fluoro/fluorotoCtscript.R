require(qpcR)
require(plyr)
require(ggplot2)
require(splitstackshape)

craf3<-read.csv("CRAF3rawfluoro.csv", header = T)
craf3$X<-NULL
craf3<-rename(craf3, c("Cycle" = "Cycles", "A1" = "H_C_1", "A2" = "N_C_1",
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

craf3ct<-pcrbatch(craf3, fluo=NULL)
write.csv(craf3ct, file="CRAF3ct.csv", row.names=F)


test <- setNames(data.frame(t(craf3ct)),craf3ct[,1])
test<-test[-1,]

test$Names<-rownames(test)

test2<-cSplit_f(test, splitCols=c("Names"), sep="_", drop = F)

test2<-rename(test2, c("Names_1"="Pop", "Names_2"="Treat", "Names_3"="Sample"))

test2$Gene<-rep("CRAF", length(test2))

test2$sig.eff<-as.numeric(as.character(test2$sig.eff))
test2$sig.cpD2<-as.numeric(as.character(test2$sig.cpD2))

ggplot(test2, aes(x=Names,y=sig.cpD2, fill=c(Pop)))+geom_bar(stat="identity")

test2$expression<-expr(test2$sig.eff, test2$sig.cpD2)

expr<-function(x,y){
  newVar<-(1+x)^y
  1/newVar
}

ggplot(test2, aes(x=Names,y=expression, fill=c(Pop)))+geom_bar(stat="identity")

test2$normalized<-(test2$expression/acttest2$expression)

ggplot(test3, aes(x=Names,y=normalized, fill=c(Pop)))+geom_bar(stat="identity")

popwe<-ddply(test2, .(Pop,Treat),summarize,pop_mean=mean(normalized),pop_sd=sd(normalized))

test3<-test2[which(test2$Pop!="NT"),]

popwe<-ddply(test3, .(Pop,Treat),summarize,pop_mean=mean(normalized),pop_sd=sd(normalized))

ggplot(test3, aes(x=Treat,y=normalized, fill=Pop))+geom_boxplot()


act1<-read.csv("Actin1rawfluoro.csv", header = T)
act1$X<-NULL
act1<-rename(act1, c("Cycle" = "Cycles", "A1" = "H_C_1", "A2" = "N_C_1",
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

act1ct<-pcrbatch(act1, fluo=NULL)
write.csv(act1ct, file="CRAF3ct.csv", row.names=F)


acttest <- setNames(data.frame(t(act1ct)),act1ct[,1])
acttest<-acttest[-1,]

acttest$Names<-rownames(acttest)

acttest2<-cSplit_f(acttest, splitCols=c("Names"), sep="_", drop = F)

acttest2<-rename(acttest2, c("Names_1"="Pop", "Names_2"="Treat", "Names_3"="Sample"))

acttest2$Gene<-rep("Actin", length(acttest2))

acttest2$sig.eff<-as.numeric(as.character(acttest2$sig.eff))
acttest2$sig.cpD2<-as.numeric(as.character(acttest2$sig.cpD2))

ggplot(acttest2, aes(x=Names,y=sig.cpD2, fill=c(Pop)))+geom_bar(stat="identity")

acttest2$expression<-expr(acttest2$sig.eff, acttest2$sig.cpD2)

expr<-function(x,y){
  newVar<-(1+x)^y
  1/newVar
}

ggplot(acttest2, aes(x=Names,y=expression, fill=c(Pop)))+geom_bar(stat="identity")

