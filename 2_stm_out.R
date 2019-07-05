####################################################
rm(list=ls())
library(LalRUtils)
load_or_install(c('tidyverse','magrittr','rio','data.table',
  'hansard' , 'devtools', 'stm', 'SnowballC', 'tm', 'quanteda',
  'tictoc', 'topicmodels', 'tidytext', 'pushoverr', 'xtable', 'knitr'))
bidet = 23062016
set.seed(bidet)
####################################################
#%%

#root = '~/HW/452/Hansard'
root = '~/Dropbox/0_GradSchool/1_HW/452/Hansard'
inp  = file.path(root, 'input/twfy')
outdir  = file.path(root, 'output')

# output from 1_structTopicModel.R
#%%
outfile = paste0(outdir, 'stm_objs.RData')
load(outfile)

wordlists = labelTopics(lda_fit)

frexwords = wordlists$frex

handcoded_topics = c(
  'refugees', 'middle-east', 'health', 'scotland', 'BBC',
  'crime', 'procedural 1', 'local services', '\\textbf{brexit}',
  'enery', 'procedural 2', 'education', 'infrastructure',
  'gratitude', 'procedural 3'
)

frexwords = cbind(handcoded_topics, frexwords)

print.xtable(xtable(frexwords,
  caption = "\\label{tab:frexwords}Top 7 words by FRequency and EXclusivity from the LDA Structural Topic Model"),
  file = file.path(outdir, "tables/frexwords.tex"))


#%%
parl <- import(file.path(inp, "speeches.csv")) %>% setDT
covars <- import(file.path(inp,"tmp/speaker_info.csv")) %>% setDT
### Preparing speech information (subsetting) ---
# Extracting date from speechdat
parl <- parl[speakername != ""]
parl[, DATEBIT := str_replace(id,
                'uk.org.publicwhip/debate/', '')]
parl[, SESSION := as.Date(str_sub(DATEBIT, 1, 10))]
parl[, `:=` (YEAR = year(SESSION), MONTH = month(SESSION))]
parl[, YM := YEAR * 100 + MONTH]
# subset to post election 2015
parl <- parl[YM >= 201506]
dropped_docs1 <- part1$docs.removed
dropped_docs2 <- out$docs.removed
parl <- parl[-dropped_docs1]
parl <- parl[-dropped_docs2]
dropcols = c('speech', 'speakerid', 'V1', 'colnum', 'id',
    'speakeroffice', 'time', 'DATEBIT', 'nospeaker')
parl = parl[, (dropcols):= NULL] # drop actual speeches
nrow(parl)

# This is now the same as the lda_fit dataset!
# merge with covars, based on 2015/2017
covars <- covars %>% select(V1, person_id, on_behalf_of_id)
covars <- covars[!duplicated(covars[ , c('person_id')]),]
names(covars)[1] <- c("sp_name")
mergedf <- parl %>% left_join(covars)

# subset and construct dtm
#%%
#hist(lda_fit$theta[, 9])
brexity_topics <- apply(lda_fit$theta, 1,
                        function(x) x[9] > 0.2)
# subset on more recent stuff
brexity_topics[parl$nospeaker == FALSE] <- FALSE
# Validate threshold
#table(brexity_topics)
#sample(parl$speech[brexity_topics], 5)
#%%
### Create DTM ----------
# Function to create DTM
nwords <- length(out$vocab)
create_sparse <- function(obj){
    vec <- rep(0, nwords)
    words <- obj[1, ]
    vec[words] <- obj[2, ]
    return(vec)
}
#%%
# Mutate / prep DTM
Sys.setenv('R_MAX_VSIZE' = 32000000000)
dtm_raw <- lapply(out$documents[brexity_topics],
                  create_sparse) %>% do.call(rbind, .)

select_words <- colSums(dtm_raw) > 25
dtm_raw_trunc <- dtm_raw[, select_words]
totwords = rowSums(dtm_raw_trunc)
dtm_raw_trunc = cbind(dtm_raw_trunc, totwords)

# Kick out columns with no words; add in total words
colnames(dtm_raw_trunc) <- c(out$vocab[select_words], 'totwords')
export_dtm <- cbind(dtm_raw_trunc, mergedf[brexity_topics, ])

fwrite(export_dtm,
       file.path(outdir, "brexit_dtm2_rawcounts.csv"))
