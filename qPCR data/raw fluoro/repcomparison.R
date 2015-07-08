#Load in required packages for functions below
require(qpcR)
require(plyr)
require(ggplot2)
require(splitstackshape)

#Read in raw fluorescence data from 1st Actin replicate
act3<-read.csv("Actin3rawfluoro.csv", header = T)
#Remove blank first column entitled "X"
act3$X<-NULL
#Rename columns so that qpcR package and appropriately handle the data
act3<-rename(act3, c("Cycle" = "Cycles", "A1" = "H_C_1", "A2" = "N_C_1",
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

#Run data through pcrbatch in qpcR package which analyzes fluorescence and produces efficiency and cycle threshold values
act3ct<-pcrbatch(act3, fluo=NULL)

#pcrbatch creates a file with each sample as an individual column in the dataframe. The problem with this is
#that I want to compare all the Ct (labelled sig.cpD2) and generate expression data for them but these values have to be
#in individual columns. To do this I must transpose the data and set the first row as the column names.
act3res<-setNames(data.frame(t(act3ct)),act3ct[,1])
#Now I must remove the first row as it is a duplicate and will cause errors with future analysis
act3res<-act3res[-1,]

#since the sample names are now in the first column the column title is row.names. This makes analys hard based on the ability to call the first column.
#to eliminate this issue, I copied the first column into a new column called "Names"
act3res$Names<-rownames(act3res)

#Since each sample name contains information such as Population, Treatment, and Sample Number I want to separate out these factors
#into new columns so that I can run future analysis based on population, treatment, or both. Also note the "drop = F" this is so the original names column remains.
act3res2<-cSplit_f(act3res, splitCols=c("Names"), sep="_", drop = F)

#After splitting the names column into three new columns I need to rename them appropriately. 
act3res2<-rename(act3res2, c("Names_1"="Pop", "Names_2"="Treat", "Names_3"="Sample"))

#I also create a column with the target gene name. This isn't used in this analysis but will be helpful for future work.
act3res2$Gene<-rep("Actin", length(act3res2))

#In transposing the data frame, the column entries became factors which cannot be used for equations.
#to fix this, I set the entries for sig.eff (efficiency) and sig.cpD2 (Ct value) to numeric. Be aware, without the as.character function the factors will be transformed inappropriately.
act3res2$sig.eff<-as.numeric(as.character(act3res2$sig.eff))
act3res2$sig.cpD2<-as.numeric(as.character(act3res2$sig.cpD2))

#Now I plot the Ct values to see how they align without converting them to expression.
ggplot(act3res2, aes(x=Names,y=sig.cpD2, fill=Pop))+geom_bar(stat="identity")

#Now I want to get expression information from my data set. qpcR has a way of doing this but its complicated and I'm not comfortable using it.
#Luckily there is an equation I can use to do it. The equation is expression = 1/(1+efficiency)^Ctvalue. I tried multiple ways to get this to work in R
#but it doesn't handle the complicated equation easily.
#To work around this, I created a function in R to run the equation and produce an outcome. x = efficiency argument, y=Ctvalue argument
expr<-function(x,y){
  newVar<-(1+x)^y
  1/newVar
}

#Now I run the data through the function and produce a useful expression value
act3res2$expression<-expr(act3res2$sig.eff, act3res2$sig.cpD2)

#Graphing the expression values is a good way to examine the data quickly for errors that might have occurred. 
ggplot(act3res2, aes(x=Names,y=expression, fill=Pop))+geom_bar(stat="identity")

#Before I'm able to compare the replicates I need to process the raw fluorescence from the second Actin run.
#To do this I perform all the same steps as the previous replicate.
act4<-read.csv("Actin4rawfluoro.csv", header = T)
act4$X<-NULL
act4<-rename(act4, c("Cycle" = "Cycles", "A1" = "H_C_1", "A2" = "N_C_1",
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

act4ct<-pcrbatch(act4, fluo=NULL)

act4res<-setNames(data.frame(t(act4ct)),act4ct[,1])
act4res<-act4res[-1,]

act4res$Names<-rownames(act4res)

act4res2<-cSplit_f(act4res, splitCols=c("Names"), sep="_", drop = F)

act4res2<-rename(act4res2, c("Names_1"="Pop", "Names_2"="Treat", "Names_3"="Sample"))

act4res2$Gene<-rep("Actin", length(act4res2))

act4res2$sig.eff<-as.numeric(as.character(act4res2$sig.eff))
act4res2$sig.cpD2<-as.numeric(as.character(act4res2$sig.cpD2))

ggplot(act4res2, aes(x=Names,y=sig.cpD2, fill=Pop))+geom_bar(stat="identity")

expr<-function(x,y){
  newVar<-(1+x)^y
  1/newVar
}

act4res2$expression<-expr(act4res2$sig.eff, act4res2$sig.cpD2)

ggplot(act4res2, aes(x=Names,y=expression, fill=Pop))+geom_bar(stat="identity")

#Now that I have Ct values, efficiencies and expression values for both replicates I can create a table of the differences between reps.
#To do this I create a data frame with a single formula that creates a column of values generated by subtracting the first run from the second.
repcomp<-as.data.frame(act3res2$sig.cpD2-act4res2$sig.cpD2)

#Now I need to add some Names for the samples to use with ggplot.Since the names column contains all the relevant information
#I copy only that column and run the split function on it again as well as the rename function. 
repcomp$Names<-act3res2$Names
repcomp<-cSplit_f(repcomp, splitCols=c("Names"), sep="_", drop = F)

#To better address the difference column in ggplot I need to rename it something simple and short. 
repcomp<-rename(repcomp, c("act3res2$sig.cpD2 - act4res2$sig.cpD2"="rep.diff", "Names_1"="Pop", "Names_2"="Treat", "Names_3"="Sample"))

#Now I just run the data through ggplot to generate a bar graph exploring the differences between the two replicate in terms of Ct values.
ggplot(repcomp, aes(x=Names, y=rep.diff, fill=Pop))+geom_bar(stat="identity")