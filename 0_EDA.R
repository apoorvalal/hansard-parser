####################################################
rm(list=ls())
library(LalRUtils)

load_or_install(c('tidyverse','magrittr','rio','data.table',
  'hansard' , 'devtools', 'stm', 'SnowballC', 'tm', 'quanteda',
  'tictoc', 'topicmodels', 'tidytext', 'pushoverr'))

bidet = 23062016
set.seed(bidet)
####################################################

root = '~/HW/452/Hansard/'
inp  = file.path(root, 'input/twfy/')
out  = file.path(root, 'output/')


parl <- setDT(fread('speeches.csv'))

nrow(parl)
parl = parl[speakername != ""]
nrow(parl)

custstop = c('friend', 'hon', 'honourable', 'member', 'house', 'govern', 
             'government', 'people', 'minister', 'ministry',
             'secretariat', 'secretary', 'bill', 'debate', 'right',
             'gentleman', 'member', 'year', 'issue', 'state', 'country',
             'support', 'committee', 'member')



##we're going to work with a subset of the data
##looking at the 1e4 random speeches
#statements <- sample(as.character(parl$speech), 1e4)

# run on entire corpus

statements <- as.character(parl$speech)

###############################################################
### quanteda
###############################################################

tic()
part12 = dfm(statements, remove = c(stopwords('english'), custstop), 
            stem = T, remove_punct = T, remove_numbers =T)
trunc_dtm = dfm_trim(part12, min_termfreq = 10, min_docfreq = 10)
toc()
pushover_quiet('dtmat prep done')

docids = trunc_dtm@Dimnames[[1]]

tic()
lda_10 = LDA(convert(trunc_dtm, to = 'topicmodels'), k = 10)
toc()
pushover_quiet('topic model run done')

tops = get_terms(lda_10, 10)
print(tops)

# document topics extraction
prob_docs = tidy(lda_10, matrix = "gamma")
prob_docs %<>% mutate(docid = as.numeric(str_replace(document, "text", ""))) %>%
    select(-document)

prob_docs_wide = spread(prob_docs, topic, gamma)
colnames(prob_docs_wide) = c('docid', 't1', 't2', 't3', 't4', 't5', 
                            't6', 't7', 't8', 't9', 't10'
                            )

fwrite(prob_docs_wide, file.path(out, 'fullsamp_topics.csv'))

#prob_docs_wide %>% filter(t7 >= .4) %>% pull(docid) -> brexity_ids
#brexity_speeches = statements[brexity_ids]
#brexity_speeches[1:3]


##########################################################
### STM
##########################################################

part1<- textProcessor(statements, removestopwords = T,
                      removenumbers = T, removepunctuation = T,
                      customstopwords = custstop
                      )

##now, extracting the relevant components 
vocab<- part1$vocab
docs<- part1$documents

##getting the documents ready for analysis
out<- prepDocuments(docs, vocab)
docs2<- out$documents
vocab2<- out$vocab

##we're now ready to use stm to fit an LDA run. the syntax is 
##docs2 = documents
##vocab2 = vocabulary
##10  = number of categories
##seed = setting the seed to ensure we can replicate our results

tic()
lda_fit<- stm(docs2, vocab2, 10)
toc()

# use to search over grid of K values
# searchK

labelTopics(lda_fit)

##you can access each complaints mixture across topics with lda_fit$theta
##for example, check the mixture for the first complaint
lda_fit$theta[1,]
##which topics are particularly prevalent?


