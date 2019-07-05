rm(list=ls())
library(LalRUtils)
load_or_install(c('tidyverse','magrittr','rio','data.table',
  'tictoc', 'pushoverr', 'glmnet', 'caret', 'doMC', 'rlist'))
ncores = detectCores()
bidet = 23062016
set.seed(bidet)
theme_set(theme_bw())
#%%

root = '~/HW/452/Hansard'
# root = '~/Dropbox/0_GradSchool/1_HW/452/Hansard'
outdir  = file.path(root, 'output/')
setwd(outdir)
#%%

dtm_raw = fread(file.path("brexit_dtm2_rawcounts.csv")) %>% setDT
word_ids = 1:2619
names = colnames(dtm_raw)
kws = names[word_ids]
del_ids = names[2620:2625]
# drop unnecessary columns
dtm_raw[, (del_ids) := NULL]
# recode labour cooperative
dtm_raw %<>% mutate(on_behalf_of_id = case_when(
  on_behalf_of_id == "labourco-operative" ~ "labour",
  TRUE ~ on_behalf_of_id)) %>% 
  mutate(TORY = 1* (on_behalf_of_id == 'conservative')) %>%setDT
# subset to 2 big parties
 #dtm = dtm_raw[on_behalf_of_id %in% 
                  #c('conservative', 'labour', 'scottish-national-party')]
#
dtm = copy(dtm_raw)
(dtm[, !..word_ids] %>% colnames() -> non_word_vars)

#%% collapse word counts by speaker - party id
dtm_by_speaker = dtm[, lapply(.SD, sum, na.rm = T),
                     by = .(sp_name, on_behalf_of_id),
                     .SDcols = word_ids]
fwrite(dtm_by_speaker, 'dtm_by_speaker_lab_tory.csv')

#%%
###############################################################
## OVERALL ACCURACY
###############################################################

dtm_by_speaker = fread('dtm_by_speaker.csv') %>% setDT
dtm_by_speaker[, TORY := ifelse(on_behalf_of_id == 'conservative', 1, 0)]
dropcols = c('sp_name', 'on_behalf_of_id')
dtm_by_speaker[, (dropcols) := NULL]

TrainingDataIndex <- createDataPartition(dtm_by_speaker$TORY, 
                                         p=0.75, list = FALSE)
# Create Training Data 
trainingData <- dtm_by_speaker[TrainingDataIndex,]
testData     <- dtm_by_speaker[-TrainingDataIndex,]
TrainingParameters <- trainControl(method = "repeatedcv", 
                                   number = 10, repeats=10)

registerDoMC(cores=10)
###############################################################
### SUPPORT VECTOR MACHINE
###############################################################
tic()
SVModel <- train(as.factor(TORY) ~ ., data = trainingData,
                 method = "svmPoly",
                 trControl= TrainingParameters,
                 tuneGrid = data.frame(degree = 1, scale = 1, C = 1),
                 preProcess = c("scale","center"),
                 na.action = na.omit
)
toc()
SVMPredictions <- predict(SVModel, testData)
# Create confusion matrix
cmSVM <-confusionMatrix(SVMPredictions, as.factor(testData$TORY))
print(cmSVM)

svm_varimp = varImp(SVModel)

###############################################################
### RANDOM FOREST
###############################################################

rfModel <- train(as.factor(TORY) ~ ., data = trainingData,
                 method = "ranger",
                 trControl= TrainingParameters,
                 tuneGrid = expand.grid(
                            mtry = seq(2, 50, length.out = 10),
                            min.node.size = 5, splitrule = 'gini'),
                 preProcess = c("scale","center"),
                 na.action = na.omit,
                 importance = 'impurity',
                 num.threads = 10
)
RFPredictions <- predict(rfModel, testData)
# Create confusion matrix
cmRF <-confusionMatrix(RFPredictions, 
                        as.factor(testData$TORY))
print(cmRF)

rf_varimp = varImp(rfModel)


# save variable importance
save(rfModel, rf_varimp, SVModel, svm_varimp,
     file = file.path(outdir, 'variableImportance.Rdata'))   

###########################################
## BY YEAR 
###########################################

model_fits = function(ydat){
    TrainingDataIndex <- createDataPartition(ydat$TORY, p=0.8, list = FALSE)
    # Create Training Data 
    trainingData <- ydat[TrainingDataIndex,]
    testData     <- ydat[-TrainingDataIndex,]
    TrainingParameters <- trainControl(method = "cv", 
                                       number = 3)
    ###############################################################
    ### SUPPORT VECTOR MACHINE
    ###############################################################
    SVModel <- train(as.factor(TORY) ~ ., data = trainingData,
                     method = "svmPoly",
                     trControl= TrainingParameters,
                     tuneGrid = data.frame(degree = 1, 
                                           scale = 1, C = 1),
                     #preProcess = c("pca","scale","center"),
                     na.action = na.omit
    )
    SVMPredictions <- predict(SVModel, testData)
    # Create confusion matrix
    cmSVM <-confusionMatrix(SVMPredictions, as.factor(testData$TORY))
    print(cmSVM)
    ###############################################################
    ### RANDOM FOREST
    ###############################################################
    rfModel <- train(as.factor(TORY) ~ ., data = trainingData,
                     method = "ranger",
                     trControl= TrainingParameters,
                     tuneGrid = expand.grid(
                                mtry = seq(2, 50, length.out = 10),
                                min.node.size = 5, splitrule = 'gini'),
                     #preProcess = c("pca","scale","center"),
                     na.action = na.omit
    )
    RFPredictions <- predict(rfModel, testData)
    # Create confusion matrix
    cmRF <-confusionMatrix(RFPredictions, 
                            as.factor(testData$TORY))
    print(cmRF)
    # return confusion matrix for each year
    return(list(svm_mod = SVModel, svm_confusion = cmSVM, 
                rf_mod = rfModel, rf_confusion = cmRF))
}

# prep for year-wise fit
prep_yeardat = function(ydat){
  keepv = c(kws, 'TORY')
  ydat = ydat[, ..keepv]
  # select nonzero words
  select_words = colnames(ydat)[colSums(ydat) > 0]
  ydat = ydat[, ..select_words] 
  return(ydat)
}

# group by year too - measurement accuracy over time
dtm_by_ys = dtm[, lapply(.SD, sum, na.rm = T),
                     by = .(sp_name, TORY, YEAR),
                     .SDcols = word_ids]
by_year <- split(dtm_by_ys, dtm_by_ys$YEAR)
yeardats = lapply(by_year, prep_yeardat)


tic()
accMetrics = lapply(yeardats, model_fits)
toc()

save.image(file.path(outdir, 'supervisedWorkspace.Rdata'))   

