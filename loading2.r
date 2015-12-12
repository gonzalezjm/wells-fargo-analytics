
setwd("/home/datauser30/WellsFargo/texts")
df = read.table('dataset.txt',sep="|",header=T)
# load('df.Rda')
df$FullText = as.character(df$FullText)

# Grab just the texts, so you can load them in the Corpus
df.texts = as.data.frame(df[,ncol(df)])
colnames(df.texts) = 'FullText'

# Remove non-ascii characters
df.texts.clean = as.data.frame(iconv(df.texts$FullText, "latin1", "ASCII", sub=""))
colnames(df.texts.clean) = 'FullText'

df$FullText = df.texts.clean$FullText

# If you want to test on just 10000 records using df.10000 created below
idx.10000 = sample(1:nrow(df),10000)
df.10000 = df[idx.10000,]

df.entire = df
df = df.10000

# Load using the tm library
library(tm) 
docs <- Corpus(DataframeSource(as.data.frame(df[,6])))   

docs <- tm_map(docs, PlainTextDocument)   

# Strip extra whitespace
docs <- tm_map(docs, stripWhitespace)

bankA.idx = which(sapply(df$FullText,function(x) grepl("BankA",x)))
bankB.idx = which(sapply(df$FullText,function(x) grepl("BankB",x)))
bankC.idx = which(sapply(df$FullText,function(x) grepl("BankC",x)))
bankD.idx = which(sapply(df$FullText,function(x) grepl("BankD",x)))

df$BankID = NaN*df$MediaType # Only do this because I want it to be the right size
df$BankID[bankA.idx] = "BankA"
df$BankID[bankB.idx] = "BankB"
df$BankID[bankC.idx] = "BankC"
df$BankID[bankD.idx] = "BankD"

bankA.docs = docs[bankA.idx]
bankB.docs = docs[bankB.idx]
bankC.docs = docs[bankC.idx]
bankD.docs = docs[bankD.idx]

summary(docs)

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords,c("their","from", "had", "who", "was", "have", "you", "but", "they", "its", "just", "got", "one", "will", "with", "has","Name", "name", "nameresp", "dirmsg", "and","for","name", "twithndl",
                                   "let","this", "are", "what", "would", "here", "nameresp", "that", "the", "rettwit", "https", "http", "twithndlbanka")) 

tdm <- TermDocumentMatrix(docs)
dtm <- DocumentTermMatrix(docs)

findFreqTerms(dtm,2000)

freq <- colSums(as.matrix(dtm))  
ord <- order(freq)   
freq[head(ord)]  
freq[tail(ord)]
freq[ord[(length(ord)-100):length(ord)]]

dtm = removeSparseTerms(dtm, 0.98)

library(wordcloud)   
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")  
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) # 100 most frequent words

dtmss <- removeSparseTerms(dtm, 0.99) 
#inspect(dtmss)

library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   

plot(fit, hang=-1)  

groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters  

findAssocs(dtmss, c("banka", "bankb", "bankc", "bankd"), corlimit=0.01)

findAssocs(dtmss, c("credit","internet", "business","account","financial", "over", "money", "phone"), corlimit=0.01)

findAssocs(dtmss, c("rating", "thank", "card", "time"), corlimit=0.01)



#bank A stuff


bankA.docs <- tm_map(bankA.docs, removeWords,c("twit_hndl_banka","twit_hndl", "their","from", "had", "who", "was", "have", "you", "but", "they", "its", "just", "got", "one", "will", "with", "has","Name", "name", "nameresp", "dirmsg", "and","for","name", "twithndl",
                                               "let", "it.", "...","name_resp", "them", "then", "tt/","ly/","bit.","our","these", "way",
                                               "this", "are", "what", "would", "here", "nameresp", "that", "the", "rettwit", "https", "http", "twithndlbanka")) 

tdmA <- TermDocumentMatrix(bankA.docs)
dtmA <- DocumentTermMatrix(bankA.docs)

findFreqTerms(dtmA,2000)

freqA <- colSums(as.matrix(dtmA))  
ordA <- order(freqA)   
freq[head(ordA)]  
freq[tail(ordA)]
freq[ordA[(length(ordA)-100):length(ordA)]]

dtmA = removeSparseTerms(dtmA, 0.99)

library(wordcloud)   
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")  
wordcloud(names(freqA), freqA, max.words=100, rot.per=0.2, colors=dark2) # 100 most frequent words

dtmssA <- removeSparseTerms(dtmA, 0.99) 
#inspect(dtmssA)

findAssocs(dtmssA, c("banka"), corlimit=0.02)
findAssocs(dtmA, c("banka"), corlimit=0.02)
findAssocs(dtmA, c("banka"), corlimit=0.02)

freq.termsA <- findFreqTerms(tdmA, lowfreq = 50)
termA.freq <- rowSums(as.matrix(tdmA))

termA.freq <- subset(termA.freq, termA.freq >= 50)

df.A <- data.frame(term = names(termA.freq), freq = termA.freq)

library(ggplot2)
ggplot(df.A, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()





#bank B stuff

bankB.docs <- tm_map(bankB.docs, removeWords,c("twit_hndl_bankb","twit_hndl", "their","from", "had", "who", "was", "have", "you", "but", "they", "its", "just", "got", "one", "will", "with", "has","Name", "name", "nameresp", "dirmsg", "and","for","name", "twithndl",
                                               "let", "it.", "...","name_resp", "them", "then", "tt/","ly/","bit.","our","these", "way",
                                               "this", "are", "what", "would", "here", "nameresp", "that", "the", "rettwit", "https", "http", "twithndlbanka")) 

tdmB <- TermDocumentMatrix(bankB.docs)
dtmB <- DocumentTermMatrix(bankB.docs)

findFreqTerms(dtmB,2000)

freqB <- colSums(as.matrix(dtmB))  
ordB <- order(freqB)   
freq[head(ordB)]  
freq[tail(ordB)]
freq[ordB[(length(ordB)-100):length(ordB)]]

dtmB = removeSparseTerms(dtmB, 0.98)

library(wordcloud)   
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")  
wordcloud(names(freqB), freqB, max.words=100, rot.per=0.2, colors=dark2) # 100 most frequent words

dtmssB <- removeSparseTerms(dtmB, 0.99) 
#inspect(dtmssB)

findAssocs(dtmB, c("bankb"), corlimit=0.02)
findAssocs(dtmB, c("bankb"), corlimit=0.02)
findAssocs(dtmB, c("bankb"), corlimit=0.02)

freq.termsB <- findFreqTerms(tdmB, lowfreq = 50)
termB.freq <- rowSums(as.matrix(tdmB))

termB.freq <- subset(termB.freq, termB.freq >= 50)

df.B <- data.frame(term = names(termB.freq), freq = termB.freq)

library(ggplot2)
ggplot(df.B, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()







#bank C stuff


bankC.docs <- tm_map(bankC.docs, removeWords, c("twit_hndl_bankc","twit_hndl", "their","from", "had", "who", "was", "have", "you", "but", "they", "its", "just", "got", "one", "will", "with", "has","Name", "name", "nameresp", "dirmsg", "and","for","name", "twithndl",
                                               "let", "it.", "...","name_resp", "them", "then", "tt/","ly/","bit.","our","these", "way",
                                               "this", "are", "what", "would", "here", "nameresp", "that", "the", "rettwit", "https", "http", "twithndlbanka")) 

tdmC <- TermDocumentMatrix(bankC.docs)
dtmC <- DocumentTermMatrix(bankC.docs)

findFreqTerms(dtmC,2000)

freqC <- colSums(as.matrix(dtmC))  
ordC <- order(freqC)   
freq[head(ordC)]  
freq[tail(ordC)]
freq[ordC[(length(ordC)-100):length(ordC)]]

dtmC = removeSparseTerms(dtmC, 0.98)

library(wordcloud)   
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")  
wordcloud(names(freqC), freqC, max.words=100, rot.per=0.2, colors=dark2) # 100 most frequent words

dtmssC <- removeSparseTerms(dtmC, 0.99) 
#inspect(dtmssC)

findAssocs(dtmC, c("bankc"), corlimit=0.02)
findAssocs(dtmC, c("bankc"), corlimit=0.02)
findAssocs(dtmC, c("bankc"), corlimit=0.02)

freq.termsC <- findFreqTerms(tdmC, lowfreq = 50)
termC.freq <- rowSums(as.matrix(tdmC))

termC.freq <- subset(termC.freq, termC.freq >= 50)

df.C <- data.frame(term = names(termC.freq), freq = termC.freq)

library(ggplot2)
ggplot(df.C, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()








#bank D stuff

bankD.docs <- tm_map(bankD.docs, removeWords,c("twit_hndl_bankc","twit_hndl", "their","from", "had", "who", "was", "have", "you", "but", "they", "its", "just", "got", "one", "will", "with", "has","Name", "name", "nameresp", "dirmsg", "and","for","name", "twithndl",
                                               "let", "it.", "...","name_resp", "them", "then", "tt/","ly/","bit.","our","these", "way",
                                               "this", "are", "what", "would", "here", "nameresp", "that", "the", "rettwit", "https", "http", "twithndlbanka")) 

tdmD <- TermDocumentMatrix(bankD.docs)
dtmD <- DocumentTermMatrix(bankD.docs)

findFreqTerms(dtmD,2000)

freqD <- colSums(as.matrix(dtmD))  
ordD <- order(freqD)   
freq[head(ordD)]  
freq[tail(ordD)]
freq[ordD[(length(ordD)-100):length(ordD)]]

dtmD = removeSparseTerms(dtmD, 0.98)

library(wordcloud)   
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")  
wordcloud(names(freqD), freqD, max.words=100, rot.per=0.2, colors=dark2) # 100 most frequent words

dtmssD <- removeSparseTerms(dtmD, 0.99) 
#inspect(dtmssD)

findAssocs(dtmssD, c("bankd"), corlimit=0.02)
findAssocs(dtmD, c("bankd"), corlimit=0.02)
findAssocs(dtmD, c("bankd"), corlimit=0.02)

freq.termsD <- findFreqTerms(tdmD, lowfreq = 50)
termD.freq <- rowSums(as.matrix(tdmD))

termD.freq <- subset(termD.freq, termD.freq >= 50)

df.D <- data.frame(term = names(termD.freq), freq = termD.freq)

library(ggplot2)
ggplot(df.D, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()


#serviceA = bankA.docs[grep("service", bankA.docs), ]

#grep(bankA.docs,"service")
#grep(txt,['strings' 'Strings'])

#bankA.docs[grep("service", rownames(content)), ]

#searchPattern <- "\\bbankA\\b.\\bservice\\b|\\bservice\\b.\\bbankA\\b"

#serviceBankA.idx = which(sapply(df$FullText,function(x) grepl(searchPattern,x)))




