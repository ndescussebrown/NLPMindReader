textpredictbitrigram <- function(input,input2) {
                library(quanteda)
                library(data.table)
        
                # DTngram <- fread("DTbitrisplit.csv",quote="")  
                DTngram <- input2
        
                inputtok <- tokens(input,remove_punct = TRUE)
                testlength <- tokens_ngrams(inputtok,n=1)
                if (length(testlength[[1]]) >1) {
                        nextword <- ""
                        # inputgram <- tokens_ngrams(inputtok,n=2)
                        # inputgram <- tolower(inputgram)
                        inputgram <- tolower(tokens_ngrams(inputtok,n=2))
                        # l <- length(inputgram)
                        # inputgram <- inputgram[l]
                        inputgram1 <- gsub('(.*)\\_(.*)','\\1',inputgram[length(inputgram)])
                        inputgram2 <- gsub('(.*)\\_(.*)','\\2',inputgram[length(inputgram)])
                        temp <- DTngram[trigram1==inputgram1 & trigram2==inputgram2]
                        temp2 <- temp[order(temp$trigramfreq,decreasing=TRUE),]
                        nextword <- temp2$trigram3[1]
                        if (length(nextword)==0 | is.na(nextword)) {
                                nextword <- ""
                                inputgram <- tokens_ngrams(inputtok,n=1)
                                inputgram <- tolower(inputgram)
                                l <- length(inputgram)
                                inputgram <- inputgram[l]
                                inputgram1 <- gsub('(.*)\\_(.*)','\\1',inputgram)
                                temp <- DTngram[bigram1==inputgram1]
                                temp2 <- temp[order(temp$bigramfreq,decreasing=TRUE),]
                                nextword <- temp2$bigram2[1]
                        }       
                }        
                else if (length(testlength[[1]]) ==1) {
                        nextword <- ""
                        inputgram <- tokens_ngrams(inputtok,n=1)
                        inputgram <- tolower(inputgram)
                        l <- length(inputgram)
                        inputgram <- inputgram[l]
                        inputgram1 <- gsub('(.*)\\_(.*)','\\1',inputgram)
                        temp <- DTngram[bigram1==inputgram1]
                        temp2 <- temp[order(temp$bigramfreq,decreasing=TRUE),]
                        nextword <- temp2$bigram2[1]
                } else {nextword <- "the"}
                if (length(nextword)==0 | is.na(nextword)) {
                        nextword <- "the"
                }        
                nextword
        }
        
        