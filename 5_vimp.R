rm(list=ls())
library(LalRUtils)
load_or_install(c('tidyverse','magrittr','rio','data.table', 'caret',
  'gridExtra'))
bidet = 23062016
set.seed(bidet)
####################################################
#%%

#root = '~/HW/452/Hansard'
root = '~/Dropbox/0_GradSchool/1_HW/452/Hansard'
inp  = file.path(root, 'input/twfy')
outdir  = file.path(root, 'output')

load(file.path(outdir,'variableImportance.Rdata'))

ls()

#%%


plot_vimp = function(mod, ttle = 'Variable Importance'){
  mod$importance %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    arrange(desc(Overall)) %>%
    slice(1:15) %>%
    mutate(rowname = forcats::fct_inorder(rowname)) %>%
    ggplot()+
      geom_point(aes(x = rowname, y = Overall))+
      coord_flip()+
      theme_bw()+
      labs(title = ttle, x = 'token')
}

rf_vimp_plot = plot_vimp(rf_varimp, "Random Forests Variable Importance")
svm_vimp_plot = plot_vimp(svm_varimp, "Support Vector Machine Variable Importance")


pp = grid.arrange(rf_vimp_plot, svm_vimp_plot, nrow = 2)

#%%
ggsave(file.path(outdir, 'figures/vip_plots.pdf'), pp, device = cairo_pdf)
