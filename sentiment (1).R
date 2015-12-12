# Since we can't find a great package in R, I'm going to use an
# example I found online to build our own
# Based on: http://www.ihub.co.ke/blogs/23216

# Only need to do once
# Download and upload: http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
#system('unrar e opinion-lexicon-English.rar')

pos <- scan('positive-words.txt',what='character',comment.char=';')
neg <- scan('negative-words.txt',what='character',comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

df.sentiment = df[bankA.idx,]

#scores = score.sentiment(df.sentiment$FullText, pos, neg, .progress='text')
scores = score.sentiment(df$FullText, pos, neg, .progress='text')
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

# global score
global_score = round( 100 * numpos / (numpos + numneg) )

scores$mediatype = df$MediaType
scores$bank = df$BankID

# colors
cols = c("#7CAE00", "#00BFC4")
names(cols) = c("twitter", "facebook")

# boxplot
library(ggplot2)
ggplot(scores, aes(x=mediatype, y=score, group=mediatype)) +
  geom_boxplot(aes(fill=mediatype)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",position=position_jitter(width=0.2), alpha=0.3) +
  labs(title = "Media Type's Sentiment Scores") + 
  xlab('Media Type') + ylab('Sentiment Score')

###################################################

### BOXPLOT OF BANK SENTIMENTS
# COLORS
colsN = c("#7CAE00", "#00BFC4", "#FFB90F", "#C39797")
names(colsN) = c("Bank A", "Bank B", "Bank C", "Bank D")
# BOXPLOT
library(ggplot2)
ggplot(scores, aes(x = bank, y = score, group = bank)) +
  geom_boxplot(aes(fill = bank)) +
  scale_fill_manual(values = colsN) +
  geom_jitter(colour="gray40",position=position_jitter(width=0.2), alpha=0.3) +
  labs(title = "Sentiment Scores by Bank") + 
  xlab('Bank') + ylab('Sentiment Score')

###################################################

# barplot of average score
meanscore = tapply(scores$score, scores$mediatype, mean)
df.plot = data.frame(mediatype=names(meanscore), meanscore=meanscore)
df.plot$mediatypes <- reorder(df.plot$mediatype, df.plot$meanscore)

ggplot(df.plot, aes(x = factor(mediatypes), y = meanscore, fill=mediatypes)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=cols[order(df.plot$meanscore)]) +
  labs(title = "Average Sentiment Score") + 
  xlab('Media Type') + ylab('Average Score')

# barplot of average very positive
mediatype_pos = ddply(scores, .(mediatype), summarise, mean_pos=mean(very.pos))
mediatype_pos$mediatypes <- reorder(mediatype_pos$mediatype, mediatype_pos$mean_pos)

ggplot(mediatype_pos, aes(x = factor(mediatype), y = mean_pos, fill=mediatype)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=cols[order(df.plot$meanscore)]) +
  labs(title = "Average Very Positive Sentiment Score") + 
  xlab('Media Type') + ylab('Average Score')

mediatype_neg = ddply(scores, .(mediatype), summarise, mean_neg=mean(very.neg))
mediatype_neg$mediatypes <- reorder(mediatype_neg$mediatype, mediatype_neg$mean_neg)

ggplot(mediatype_neg, aes(x = factor(mediatype), y = mean_neg, fill=mediatype)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=cols[order(df.plot$meanscore)]) +
  labs(title = "Average Very Negative Sentiment Score") + 
  xlab('Media Type') + ylab('Average Score')

