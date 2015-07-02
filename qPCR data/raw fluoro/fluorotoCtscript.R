require(qpcR)
require(plyr)
require(ggplot2)

craf3<-read.csv("CRAF3rawfluoro.csv", header = T)
craf3$X<-NULL
craf3<-rename(craf3, c("Cycle" = "Cycles", "A1" = "HC1", "A2" = "NC1",
                "A3"= "SC1", "A4"="HT1", "A5"="NT1","A6"="ST1",
                "A7"="NTC1","B1" = "HC2", "B2" = "NC2","B3"= "SC2",
                "B4"="HT2", "B5"="NT2", "B6"="ST2","B7"="NTC2",
                "C1" = "HC3", "C2" = "NC3","C3"= "SC3","C4"="HT3",
                "C5"="NT3", "C6"="ST3", "C7"="NTC3","D1" = "HC4",
                "D2" = "NC4","D3"= "SC4", "D4"="HT4", "D5"="NT4",
                "D6"="ST4", "D7"="NTC4","E1" = "HC5", "E2" = "NC5",
                "E3"= "SC5", "E4"="HT5", "E5"="NT5", "E6"="ST5",
                "F1" = "HC6", "F2" = "NC6","F3"= "SC6", "F4"="HT6",
                "F5"="NT6", "F6"="ST6","G1" = "HC7", "G2" = "NC7",
                "G3"= "SC7", "G4"="HT7", "G5"="NT7", "G6"="ST7",
                "H1" = "HC8", "H2" = "NC8","H3"= "SC8", "H4"="HT8",
                "H5"="NT8", "H6"="ST8"))

craf3ct<-pcrbatch(craf3, fluo=NULL)
write.csv(craf3ct, file="CRAF3ct.csv", row.names=F)


test <- setNames(data.frame(t(craf3ct)),craf3ct[,1])
test<-test[-1,]

ggplot(test, aes(x=rownames(test),y=sig.cpD2, fill=rownames(test)))+geom_bar(stat="identity")

craf3cte<-rename(craf3ct, c("HC1"="g1c1","NC1"="g1c2",
                       "SC1"="g1c3","HT1"="g1s1","NT1"="g1s2","ST1"="g1s3",
                       "NTC1"="g1s4","HC2"="g1c5","NC2"="g1c6","SC2"="g1c7",
                       "HT2"="g1s5","NT2"="g1s6","ST2"="g1s7","NTC2"="g1s4",
                       "HC3"="g1c8","NC3"="g1c9","SC3"="g1c10","HT3"="g1s8",
                       "NT3"="g1s9", "ST3"="g1s10","NTC3"="g1cNTC4","HC4"="g1c11",
                       "NC4"="g1c12","SC4"="g1c13","HT4"="g1s11","NT4"="g1s12",
                       "ST4"="g1s13","NTC4"="g1c4","HC5"="g1c14", "NC5"="g1c15",
                       "SC5"="g1c16","HT5"="g1s14","NT5"="g1s15","ST5"="g1s16",
                       "HC6"="g1c17", "NC6"="g1c18","SC6"="g1c19","HT6"="g1s17",
                       "NT6"="g1s18", "ST6"="g1s19","HC7"="g1c20","NC7"="g1c21",
                       "SC7"="g1c22","HT7"="g1s20","NT7"="g1s21","ST7"="g1s22",
                       "HC8"="g1c23", "NC8"="g1c24","SC8"="g1c25","HT8"="g1s23",
                       "NT8"="g1s24", "ST8"="g1s25"))

ratiobatch(craf3cte, group=c("g1c1","g1c2",
                                      "g1c3","g1s1","g1s2","g1s3",
                                      "g1s4","g1c5","g1c6","g1c7",
                                      "g1s5","g1s6","g1s7","g1s4",
                                      "g1c8","g1c9","g1c10","g1s8",
                                      "g1s9", "g1s10","g1cNTC4","g1c11",
                                      "g1c12","g1c13","g1s11","g1s12",
                                      "g1s13","g1c4","g1c14", "g1c15",
                                      "g1c16","g1s14","g1s15","g1s16",
                                      "g1c17", "g1c18","g1c19","g1s17",
                                      "g1s18", "g1s19","g1c20","g1c21",
                                      "g1c22","g1s20","g1s21","g1s22",
                                      "g1c23", "g1c24","g1c25","g1s23",
                                      "g1s24", "g1s25"),
                  which.eff="sig")
