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
setwd(inp)

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

statements <- as.character(parl$speech)

###############################################################
### quanteda
###############################################################

tic()
part12 = dfm(statements, remove = c(stopwords('english'), custstop), 
            stem = T, remove_punct = T, remove_numbers =T)
trunc_dtm = dfm_trim(part12, min_termfreq = 10, min_docfreq = 10)
docids = trunc_dtm@Dimnames[[1]]
toc()
pushover_quiet('dtmat done')


tic()
lda_10 = LDA(convert(trunc_dtm, to = 'topicmodels'), k = 10)
toc()
pushover_quiet('LDA done')

kws = get_terms(lda_10, 10)
print(kws)

# document topics extraction
prob_docs = tidy(lda_10, matrix = "gamma")
prob_docs %<>% 
    mutate(docid = as.numeric(str_replace(document, "text", ""))) %>%
    select(-document)

prob_docs_wide = spread(prob_docs, topic, gamma)
colnames(prob_docs_wide) = c('docid', 't1', 't2', 't3', 't4', 't5', 
                            't6', 't7', 't8', 't9', 't10'
                            )

fwrite(prob_docs_wide, file.path(out, 'fullsamp_lda_out.csv'))

prob_docs_wide %>% filter(t10 >= .4) %>% pull(docid) -> brexity_ids
brexity_speeches = statements[brexity_ids]
brexity_speeches[1:3]

outpath = paste0(out, 'lda_objs.Rdata')
save.image(file = outpath)


