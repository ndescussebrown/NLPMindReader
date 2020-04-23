######### ngram modelling based on corpus (not stemmed and stopwords not removed)
############################################################################################
library(quanteda)
library(data.table)
library(feather)
memory.limit(size=20000)

corpustok <- tokens(corpus(corpus),remove_punct = TRUE)
save(corpustok,file="corpustok.RData")

load("corpustok.RData")
tokbigram <- tokens_ngrams(corpustok,n=2)
rm(corpustok)
save(tokbigram,file="tokbigram.RData")
dfmtokbigram <- dfm(tokbigram)
rm(tokbigram)
save(dfmtokbigram,file="dfmtokbigram.RData")
freqdfmtokbigram <- textstat_frequency(dfmtokbigram[1,], dim(dfmtokbigram[1,])[2])
rm(dfmtokbigram)
ind <- which(is.na(freqdfmtokbigram$frequency))
freqdfmtokbigram <- freqdfmtokbigram[-ind,1:2]
save(freqdfmtokbigram,file="freqdfmtokbigram.RData")
rm(freqdfmtokbigram,ind)

load("corpustok.RData")
toktrigram <- tokens_ngrams(corpustok,n=3)
rm(corpustok)
save(toktrigram,file="toktrigram.RData")
dfmtoktrigram <- dfm(toktrigram)
rm(toktrigram)
save(dfmtoktrigram,file="dfmtoktrigram.RData")
freqdfmtoktrigram <- textstat_frequency(dfmtoktrigram[1,], dim(dfmtoktrigram[1,])[2])
rm(dfmtoktrigram)
ind <- which(is.na(freqdfmtoktrigram$frequency))
freqdfmtoktrigram <- freqdfmtoktrigram[-ind,1:2]
save(freqdfmtoktrigram,file="freqdfmtoktrigram.RData")
rm(freqdfmtoktrigram,ind)

load("corpustok.RData")
tokquadgram <- tokens_ngrams(corpustok,n=4)
rm(corpustok)
save(tokquadgram,file="tokquadgram.RData")
dfmtokquadgram <- dfm(tokquadgram)
rm(tokquadgram)
save(dfmtokquadgram,file="dfmtokquadgram.RData")
freqdfmtokquadgram <- textstat_frequency(dfmtokquadgram[1,], dim(dfmtokquadgram[1,])[2])
rm(dfmtokquadgram)
ind <- which(is.na(freqdfmtokquadgram$frequency))
freqdfmtokquadgram <- freqdfmtokquadgram[-ind,1:2]
save(freqdfmtokquadgram,file="freqdfmtokquadgram.RData")
rm(freqdfmtokquadgram,ind)

load("corpustok.RData")
tokfivegram <- tokens_ngrams(corpustok,n=5)
rm(corpustok)
save(tokfivegram,file="tokfivegram.RData")
dfmtokfivegram <- dfm(tokfivegram)
rm(tokfivegram)
save(dfmtokfivegram,file="dfmtokfivegram.RData")
freqdfmtokfivegram <- textstat_frequency(dfmtokfivegram[1,], 0.1*dim(dfmtokfivegram[1,])[2])
rm(dfmtokfivegram)
ind <- which(is.na(freqdfmtokfivegram$frequency))
if (length(ind)!=0) {
        freqdfmtokfivegram <- freqdfmtokfivegram[-ind,1:2]
} else {
        freqdfmtokfivegram <- freqdfmtokfivegram[,1:2]
}
save(freqdfmtokfivegram,file="freqdfmtokfivegram.RData")
rm(freqdfmtokfivegram,ind)

##########################################################################
## CHECK NUMBER OF ROWS NEEDED FIRST BASED ON LENGTH OF "freqdfmtokfivegram.RData"
#############################################################################

DFngram <- data.frame(matrix(NA, nrow = 30431752, ncol = 8))
names(DFngram) <- c("bigram","bigramfreq","trigram","trigramfreq","quadgram",
                    "quadgramfreq","fivegram","fivegramfreq")
load("freqdfmtokbigram.RData")
DFngram$bigram[1:length(freqdfmtokbigram$feature)] <- freqdfmtokbigram$feature[1:length(freqdfmtokbigram$feature)]
DFngram$bigramfreq[1:length(freqdfmtokbigram$feature)] <- freqdfmtokbigram$frequency[1:length(freqdfmtokbigram$feature)]
rm(freqdfmtokbigram)
load("freqdfmtoktrigram.RData")
DFngram$trigram[1:length(freqdfmtoktrigram$feature)] <- freqdfmtoktrigram$feature[1:length(freqdfmtoktrigram$feature)]
DFngram$trigramfreq[1:length(freqdfmtoktrigram$feature)] <- freqdfmtoktrigram$frequency[1:length(freqdfmtoktrigram$feature)]
rm(freqdfmtoktrigram)
load("freqdfmtokquadgram.RData")
DFngram$quadgram[1:length(freqdfmtokquadgram$feature)] <- freqdfmtokquadgram$feature[1:length(freqdfmtokquadgram$feature)]
DFngram$quadgramfreq[1:length(freqdfmtokquadgram$feature)] <- freqdfmtokquadgram$frequency[1:length(freqdfmtokquadgram$feature)]
rm(freqdfmtokquadgram)
load("freqdfmtokfivegram.RData")
DFngram$fivegram[1:length(freqdfmtokfivegram$feature)] <- freqdfmtokfivegram$feature[1:length(freqdfmtokfivegram$feature)]
DFngram$fivegramfreq[1:length(freqdfmtokfivegram$feature)] <- freqdfmtokfivegram$frequency[1:length(freqdfmtokfivegram$feature)]
rm(freqdfmtokfivegram)
save(DFngram,file="DFngram.RData")

DTngram <- as.data.table(DFngram[,1:8])
rm(DFngram)
save(DTngram,file="DTngram.RData")

# Try a different format for storage and use of sqldf
DTbigram <- DTngram[,1:2]
DTbigram$bigram1 <- gsub('(.*)\\_(.*)','\\1',DTbigram$bigram)
DTbigram$bigram2 <- gsub('(.*)\\_(.*)','\\2',DTbigram$bigram)
DTbigram$bigram1 <- gsub("'"," ",DTbigram$bigram1)
DTbigram$bigram2 <- gsub("'"," ",DTbigram$bigram2)
DTbigram$bigram1 <- gsub(",","",DTbigram$bigram1)
DTbigram$bigram2 <- gsub(",","",DTbigram$bigram2)
DTbigramsplit <- DTbigram[,c(3,4,2)]
write.table(DTbigramsplit,file="DTbigramsplit.csv",sep=",",row.names =F,quote=F)


DTtrigram <- DTngram[,3:4]
DTtrigram$trigram1 <- gsub('(.*)\\_(.*)\\_(.*)','\\1',DTtrigram$trigram)
DTtrigram$trigram2 <- gsub('(.*)\\_(.*)\\_(.*)','\\2',DTtrigram$trigram)
DTtrigram$trigram3 <- gsub('(.*)\\_(.*)\\_(.*)','\\3',DTtrigram$trigram)
DTtrigram$trigram1 <- gsub("'"," ",DTtrigram$trigram1)
DTtrigram$trigram2 <- gsub("'"," ",DTtrigram$trigram2)
DTtrigram$trigram3 <- gsub("'"," ",DTtrigram$trigram3)
DTtrigram$trigram1 <- gsub(",","",DTtrigram$trigram1)
DTtrigram$trigram2 <- gsub(",","",DTtrigram$trigram2)
DTtrigram$trigram3 <- gsub(",","",DTtrigram$trigram3)
DTtrigramsplit <- DTtrigram[,c(3,4,5,2)]
write.table(DTtrigramsplit,file="DTtrigramsplit.csv",sep=",",row.names =F,quote=F)

DTquadgram <- DTngram[,5:6]
DTquadgram$quadgram1 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)','\\1',DTquadgram$quadgram)
DTquadgram$quadgram2 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)','\\2',DTquadgram$quadgram)
DTquadgram$quadgram3 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)','\\3',DTquadgram$quadgram)
DTquadgram$quadgram4 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)','\\4',DTquadgram$quadgram)
DTquadgram$quadgram1 <- gsub("'"," ",DTquadgram$quadgram1)
DTquadgram$quadgram2 <- gsub("'"," ",DTquadgram$quadgram2)
DTquadgram$quadgram3 <- gsub("'"," ",DTquadgram$quadgram3)
DTquadgram$quadgram4 <- gsub("'"," ",DTquadgram$quadgram4)
DTquadgram$quadgram1 <- gsub(",","",DTquadgram$quadgram1)
DTquadgram$quadgram2 <- gsub(",","",DTquadgram$quadgram2)
DTquadgram$quadgram3 <- gsub(",","",DTquadgram$quadgram3)
DTquadgram$quadgram4 <- gsub(",","",DTquadgram$quadgram4)
DTquadgramsplit <- DTquadgram[,c(3,4,5,6,2)]
write.table(DTquadgramsplit,file="DTquadgramsplit.csv",sep=",",row.names =F,quote=F)
# setkey(DTquadgramsplit,quadgram1,quadgram2,quadgram3,quadgram4)
# DTquadgramsplit <- DTquadgramsplit[-c(1:20),]

DTfivegram <- DTngram[,7:8]
DTfivegram$fivegram1 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)\\_(.*)','\\1',DTfivegram$fivegram)
DTfivegram$fivegram2 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)\\_(.*)','\\2',DTfivegram$fivegram)
DTfivegram$fivegram3 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)\\_(.*)','\\3',DTfivegram$fivegram)
DTfivegram$fivegram4 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)\\_(.*)','\\4',DTfivegram$fivegram)
DTfivegram$fivegram5 <- gsub('(.*)\\_(.*)\\_(.*)\\_(.*)\\_(.*)','\\5',DTfivegram$fivegram)
DTfivegram$fivegram1 <- gsub("'"," ",DTfivegram$fivegram1)
DTfivegram$fivegram2 <- gsub("'"," ",DTfivegram$fivegram2)
DTfivegram$fivegram3 <- gsub("'"," ",DTfivegram$fivegram3)
DTfivegram$fivegram4 <- gsub("'"," ",DTfivegram$fivegram4)
DTfivegram$fivegram5 <- gsub("'"," ",DTfivegram$fivegram5)
DTfivegram$fivegram1 <- gsub(",","",DTfivegram$fivegram1)
DTfivegram$fivegram2 <- gsub(",","",DTfivegram$fivegram2)
DTfivegram$fivegram3 <- gsub(",","",DTfivegram$fivegram3)
DTfivegram$fivegram4 <- gsub(",","",DTfivegram$fivegram4)
DTfivegram$fivegram5 <- gsub(",","",DTfivegram$fivegram5)
DTfivegramsplit <- DTfivegram[,c(3,4,5,6,7,2)]
write.table(DTfivegramsplit,file="DTfivegramsplit.csv",sep=",",row.names =F,quote=F)

vector1 <- fread("DTbigramsplit.csv")
vector1[which(vector1$bigramfreq<2)] <- data.frame(matrix(NA, nrow = length(which(vector1$bigramfreq<2)), ncol = 3))
vector2 <- fread("DTtrigramsplit.csv")
vector2[which(vector2$trigramfreq<2)] <- data.frame(matrix(NA, nrow = length(which(vector2$trigramfreq<2)), ncol = 4))
vector12 <- cbind(vector1,vector2)
rm(vector1,vector2)
vector3 <- fread("DTquadgramsplit.csv")
vector3[which(vector3$quadgramfreq<2)] <- data.frame(matrix(NA, nrow = length(which(vector3$quadgramfreq<2)), ncol = 5))
vector123 <- cbind(vector12,vector3)
rm(vector12,vector3)
vector4 <- fread("DTfivegramsplit.csv")
vector4[which(vector4$fivegramfreq<2)] <- data.frame(matrix(NA, nrow = length(which(vector4$fivegramfreq<2)), ncol = 6))

DTngramsplit <- cbind(vector123,vector4)
rm(vector123,vector4)
write.table(DTngramsplit,file="DTngramsplit.csv",sep=",",row.names =F,quote=F)



DTngramsplit <- fread("DTngramsplit.csv")
DTbitrisplit <- DTngramsplit[,1:7]
index <- which(is.na(DTbitrisplit$trigramfreq))
DTbitrisplit <- DTbitrisplit[-index,]    
# index2 <- which(DTbitrisplit$trigram1 %in% c("!<U+FEFF>","!<U+FEFF><U+FEFF><U+FEFF><U+FEFF><U+FEFF><U+FEFF>",'"<U+FEFF>'))
# DTbitrisplit <- DTbitrisplit[-index2,] 

write.table(DTbitrisplit,file="DTbitrisplit.csv",sep=",",row.names =F,quote=F)



