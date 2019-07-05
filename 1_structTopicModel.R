#%%
####################################################
rm(list=ls())
library(LalRUtils)

load_or_install(c('tidyverse','magrittr','rio','data.table',
  'hansard' , 'devtools', 'stm', 'SnowballC', 'tm', 'quanteda',
  'tictoc', 'topicmodels', 'tidytext', 'pushoverr'))

bidet = 23062016
set.seed(bidet)
####################################################
#%%
# root = '~/HW/452/Hansard'
root = '~/Dropbox/0_GradSchool/1_HW/452/Hansard'
inp  = file.path(root, 'input/twfy/')
outdir  = file.path(root, 'output/')
setwd(inp)
parl = setDT(fread('speeches.csv'))
#%%
nrow(parl)
parl = parl[speakername != ""]
nrow(parl)

parl[, datebit := str_replace(id,
                'uk.org.publicwhip/debate/', '')]
parl[, session := as.Date(str_sub(datebit, 1, 10))]
parl[, `:=` (year = year(session), month = month(session))]
parl[, ym := year * 100 + month]
# subset to post election 2015
parl = parl[ym >= 201506]
nrow(parl)

custstop = c('friend', 'hon', 'honourable', 'member', 'house', 'govern',
             'government', 'people', 'minister', 'ministry',
             'secretariat', 'secretary', 'bill', 'debate', 'right',
             'gentleman', 'member', 'year', 'issue', 'state', 'country',
             'support', 'committee', 'member', 'will')

#%%
##we're going to work with a subset of the data
##looking at the 1e4 random speeches
#statements <- sample(as.character(parl$speech), 1e4)

statements <- as.character(parl$speech)

##########################################################
### STM
##########################################################

tic()
part1 <- textProcessor(statements, removestopwords = T,
                      removenumbers = T, removepunctuation = T,
                      customstopwords = custstop
                      )
##now, extracting the relevant components
vocab <- part1$vocab
docs  <- part1$documents
toc()

##getting the documents ready for analysis

tic()
out    <- prepDocuments(docs, vocab)
docs2  <- out$documents
vocab2 <- out$vocab
toc()
pushover_quiet('prep done')

##we're now ready to use stm to fit an LDA run. the syntax is
##docs2 = documents
##vocab2 = vocabulary
##10  = number of categories
##seed = setting the seed to ensure we can replicate our results

tic()
lda_fit<- stm(docs2, vocab2, 15)
toc()
pushover_quiet('STM done')

# keywords
labelTopics(lda_fit)

lda_fit$theta[1,]

##which topics are particularly prevalent?

outfile = paste0(outdir, 'stm_objs.RData')
outfile

save.image(file = outfile)
pushover_quiet('wrote to file')

