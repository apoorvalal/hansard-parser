#%%
rm(list=ls())
library(LalRUtils)

load_or_install(c('tidyverse','magrittr','rio','data.table',
  'tictoc', 'pushoverr', 'glmnet', 'caret', 'doMC', 'rlist',
  'knitr', 'xtable'))


theme_sv <- function(){
  theme_bw(base_size=11) %+replace%
  theme(
    panel.grid.major =  element_line(
      colour = "grey50",
      size = 0.2,
      linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey97"),
    plot.margin = unit(c(0.2, 1, 0.2, 1), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill= NULL, colour = "white", linetype = NULL),
    strip.text = element_text(colour = 'grey50', size = 9, vjust = 0.5)
  )
}


bidet = 23062016
set.seed(bidet)
theme_set(theme_bw())
#%%
# root = '~/HW/452/Hansard'
root = '~/Dropbox/0_GradSchool/1_HW/452/Hansard'
outdir  = file.path(root, 'output/')
setwd(outdir)
#%%
load(file.path(outdir, 'supervisedWorkspace2.Rdata'))
ls()
#%%
todf <- function(cm) data.frame(cbind(t(cm$overall),t(cm$byClass)))
overall_summary = data.frame(rbind(todf(cmSVM), todf(cmRF)))
(overall_fit = cbind(
  model = c('Support Vector Machine', 'Random Forest'),
  overall_summary
  ))
overall_fit %<>% select(model, Prevalence,
  Precision, Recall, F1, Accuracy)

overall_fit

print(xtable(overall_fit, type = "latex"), file = "tables/overall_fit.tex")
#%%
extract_from_acc = function(year, confusion,
  measure = 'Balanced.Accuracy'){
  todf(accMetrics[[year]][[confusion]])[, measure]
}

(svm_accuracies = c(
  extract_from_acc('2015', 'svm_confusion', 'Accuracy'),
  extract_from_acc('2016', 'svm_confusion', 'Accuracy'),
  extract_from_acc('2017', 'svm_confusion', 'Accuracy'),
  extract_from_acc('2018', 'svm_confusion', 'Accuracy'),
  extract_from_acc('2019', 'svm_confusion', 'Accuracy')
))

(rf_accuracies = c(
  extract_from_acc('2015', 'rf_confusion', 'Accuracy'),
  extract_from_acc('2016', 'rf_confusion', 'Accuracy'),
  extract_from_acc('2017', 'rf_confusion', 'Accuracy'),
  extract_from_acc('2018', 'rf_confusion', 'Accuracy'),
  extract_from_acc('2019', 'rf_confusion', 'Accuracy')
))

acc_trends = data.frame(year = 2015:2019, svm = svm_accuracies, rf = rf_accuracies)
#%%
melted_acc <- reshape2::melt(acc_trends, id = 'year')

p = ggplot(data = melted_acc, aes(x = year, y = value,
    colour = variable)) + geom_point() +
    geom_line() + theme_sv() + scale_colour_brewer(palette='Set1') +
    theme(
       legend.title = element_blank(),
        legend.position = "bottom", panel.border = element_blank()
    ) + labs(
  y = 'Accuracy'
  )

p

ggsave('figures/accuracy_trends.pdf', p,
  height = 5, width= 7, units = 'in')
