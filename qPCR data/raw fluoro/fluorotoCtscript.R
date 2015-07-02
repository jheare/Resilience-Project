require(qpcR)
require(plyr)

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
