# Load dependencies
library(rio)
library(factoextra)
library(broom)
library(pscl)
library(tidyverse)
library(ggrepel)

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

# Load data
votemat <- read.delim("input/publicwhip/votematrix-2017.dat", 
           header=TRUE)

# Load party IDs
speakerinfo <- import("input/twfy/tmp/speaker_info.csv", fill = TRUE)[, -1]
speakerinfo <- speakerinfo %>% rowwise %>% mutate(memberid = gsub("uk.org.publicwhip/member/", "", id) %>% as.numeric)
speakerinfo$start_date <- as.Date(speakerinfo$start_date)
mpcovars <- import("input/publicwhip/mp_covars.csv")
names(mpcovars)[1] <- "memberid"

# Subset on Brexity topics
eu_debate <- grep("Withdrawal|European Union", votemat$Bill)

# Prepare PCA
# votemat_pure <- votemat[-nrow(votemat), 6:ncol(votemat) - 1] %>% apply(., 1, as.numeric)
votemat_pure <- votemat[eu_debate, 6:ncol(votemat) - 1] %>% apply(., 1, as.numeric)

# Amend data (tellers and missing)
votemat_pure[is.na(votemat_pure) == TRUE] <- 0
votemat_pure[votemat_pure == 2] <- 1
votemat_pure[votemat_pure == 4] <- -1
votemat_pure[votemat_pure == 5] <- -1
votemat_pure[votemat_pure == 3] <- 0
votemat_pure[votemat_pure == -9] <- 0

# Create Dataframe with MP covariates
votemat_trans <- as.data.frame(votemat_pure) %>% mutate(memberid = colnames(votemat)[5:667] %>% gsub("mpid", "", .) %>% as.numeric) %>% left_join(mpcovars)

nrow(votemat_trans)
ncol(votemat_pure)

# Run PCA
pca_out <- prcomp(votemat_pure)
# fviz_eig(pca_out, ggtheme = theme_classic())

# Plot results
votemat_trans <- votemat_trans %>% mutate(PC1 = pca_out$x[, 1], PC2 = pca_out$x[, 2])
labeldf <- votemat_trans %>% filter((PC1 < 7.5 & PC1 > -8) | surname %in% c("Dodds", "Rees-Mogg", "Johnson", "Kawczynski", "Bone"))

ggplot(votemat_trans, aes(PC1, PC2)) +
    geom_point(aes(color = party)) +
    geom_text_repel(data = labeldf, aes(label = surname), size = 2.3) +
    scale_color_manual(values = c("#0087DC", "#D46A4C", "#6AB023", "#333333", "#DC231F", "#FAA61A", "#008142", "#FEF987")) +
    theme_sv() +
    theme(legend.position = "bottom") +
    labs(x = "Opposition --- Government", y = "Leave --- Remain")
ggsave("output/figures/pca_votes.pdf")
