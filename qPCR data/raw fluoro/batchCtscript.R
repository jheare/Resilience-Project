require(qpcR)




tlr1<-read.csv("TLR1rawfluoro.csv", header = T)
tlr2<-read.csv("TLR2rawfluoro.csv", header = T)
act1<-read.csv("Actin1rawfluoro.csv", header = T)
act2<-read.csv("Actin2rawfluoro.csv", header = T)
carm1<-read.csv("CARM1rawfluoro.csv", header = T)
carm2<-read.csv("CARM2rawfluoro.csv", header = T)
craf1<-read.csv("CRAF1rawfluoro.csv", header = T)

tlr1ct<-pcrbatch(tlr1, fluo=NULL)
tlr2ct<-pcrbatch(tlr2, fluo=NULL)
act1ct<-pcrbatch(act1, fluo=NULL)
act2ct<-pcrbatch(act2, fluo=NULL)
carm1ct<-pcrbatch(carm1, fluo=NULL)
carm2ct<-pcrbatch(carm2, fluo=NULL)
craf1ct<-pcrbatch(craf1, fluo=NULL)


write.csv(tlr1ct, file = "TLRrep1ct.csv", row.names = F)
write.csv(tlr2ct, file = "TLRrep2ct.csv", row.names = F)
write.csv(act1ct, file = "Actinrep1ct.csv", row.names = F)
write.csv(act2ct, file = "Actinrep2ct.csv", row.names = F)
write.csv(carm1ct, file = "CARMrep1ct.csv", row.names = F)
write.csv(carm2ct, file = "CARMrep2ct.csv", row.names = F)
write.csv(craf1ct, file = "CRAFrep1ct.csv", row.names = F)


repcomp<-repcomp[which(repcomp$Pop!=c("NT","**NT")),]