library(rio)
library(tidyverse)
library(stm)
library(quanteda)
library(factoextra)
library(pscl)
library(data.table)
library(topicmodels)
library(tidytext)
library(lubridate)
library(RColorBrewer)
library(ggpubr)

load("output/clustering.RData")

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

#=========================================================================
# 
# #=========================================================================

load("output/stm_objs.RData")
parl <- import("input/twfy/speeches.csv") %>% setDT
covars <- import("input/twfy/tmp/speaker_info.csv") %>% setDT
load("output/clustering.RData")

### 
### Preparing data
###

### Preparing speech information (subsetting) ---
# Extracting date from speechdat
parl <- parl[speakername != ""]

parl[, datebit := str_replace(id, 
                'uk.org.publicwhip/debate/', '')]
parl[, session := as.Date(str_sub(datebit, 1, 10))]
parl[, `:=` (year = year(session), month = month(session))]
parl[, ym := year * 100 + month]

# subset to post election 2015
parl <- parl[ym >= 201506] 
dropped_docs1 <- part1$docs.removed
dropped_docs2 <- out$docs.removed
parl <- parl[-dropped_docs1]
parl <- parl[-dropped_docs2]
nrow(parl)
# This is now the same as the lda_fit dataset!

# merge with covars, based on 2015/2017
covars <- covars %>% select(V1, person_id, on_behalf_of_id) %>% filter(on_behalf_of_id != "")
covars <- covars[!duplicated(covars[ , c('person_id')]),]
names(covars)[1] <- c("sp_name")
mergedf <- parl %>% left_join(covars)
mergedf$on_behalf_of_id[mergedf$on_behalf_of_id == "labourco-operative"] <- "labour"

# Plotting prevalence of different topics
broad_topics_over_time <- lda_fit$theta %>% as.data.frame %>%
  mutate(date = as.Date(mergedf$session)) %>% 
  rename("Procedural" = V14, "Brexit" = V9, "GeneralWords" = V11, "ProceduralQuestions" = V7, "LocalAndHousing" = V8 , "Railway" = V13, "EducationAndEcon" = V12, "EnergyAndEcon" = V10, "HomeAffairs" = V6, "Scotland" = V4) %>% 
  group_by(month=floor_date(date, "month")) %>% 
  summarise_all(mean) %>% 
  gather(., topic, theta, V1:V15)

parl_topics <- ggplot(broad_topics_over_time %>% filter(topic %in% c("Brexit", "HomeAffairs", "Procedural")), 
       aes(x = month, y = theta)) +
  geom_smooth(aes(colour = topic), 
              se = FALSE,
              method = "auto",
              span = 0.25) +
  geom_vline(xintercept = as.Date("2016-06-23"), lty = "dotted") +
  geom_vline(xintercept = as.Date("2017-03-30"), lty = "dotted") +
  theme_sv() +
  labs(x = "Year", y = "Topic Proportion", title = "Prevalence of selected topics across all legislative speeches") +
  ylim(c(0, 0.2)) +
  theme(legend.position = "bottom")

ggsave("output/figures/parliament_topic_prevalence.pdf")



### Selecting Brexity topics -------------

# Distribution of "Brexit scores"
hist(lda_fit$theta[, 9])
brexity_topics <- apply(lda_fit$theta, 1, function(x) x[9] > 0.2)

# subset on more recent stuff
brexity_topics[parl$nospeaker == FALSE] <- FALSE

# Let's subset on EU being mentioned
# brexity_topics <- grepl(("European Union|European|\\sEU"), parl$speech)

# Validate threshold
table(brexity_topics)
sample(parl$speech[brexity_topics], 5)

### Create DTM ----------
# Function to create DTM
nwords <- length(out$vocab)
create_sparse <- function(obj){
    vec <- rep(0, nwords)
    words <- obj[1, ]
    vec[words] <- obj[2, ]
    return(vec)
}

# Mutate / prep DTM
Sys.setenv('R_MAX_VSIZE' = 32000000000)
dtm_raw <- lapply(out$documents[brexity_topics], create_sparse) %>% do.call(rbind, .)

select_words <- colSums(dtm_raw) > 25
dtm_raw_trunc <- dtm_raw[, select_words]

# Normalise
dtm_raw_norm <- apply(dtm_raw_trunc, 1, function(x) x / sum(x)) %>% t
dtm_raw_norm[is.na(dtm_raw_norm)] <- 0

# Kick out columns with no words
colnames(dtm_raw_norm) <- out$vocab[select_words]
export_dtm <- cbind(dtm_raw_norm, mergedf[brexity_topics, ])
write_csv(export_dtm, path = "output/brexit_dtm2.csv")


#
# PCA ON ALL SPEECHES
#

# prepare DTM
dtm_raw_norm_select <- as.data.frame(dtm_raw_norm) %>% select(-c(will))

# run PCA
pca_out <- prcomp(dtm_raw_norm_select)

pca_mat <- data.frame(PC1 = pca_out$x[, 1], PC2 = pca_out$x[, 2], speakername = export_dtm$speakername, party = export_dtm$on_behalf_of_id)

# plot results
pca1 <- ggplot(pca_mat %>% filter(party != ""), aes(PC1, PC2)) +
    geom_point(aes(color = party), alpha = 0.3) +
    scale_color_manual(values = c("#0087DC", "#D46A4C", "#6AB023", "#DC231F", "#FAA61A", "#008142", "#FEF987", "#333333", "#333333", "#333333")) +
    theme_sv() +
    labs(x = "Component 1 (3.4%)", y = "Component 2 (2.6%)") +
    ggtitle("Dimensions in Word Frequency by Speech")

ggsave("output/figures/pca1.pdf", width = 7, height = 6)

# Check word weights
fviz_pca_var(pca_out,
  repel = TRUE, geom = c("point", "text"),
  select.var = list(contrib = 20),
  ggtheme = theme_classic())

#
# PCA ON SPEECHES BY MP
#

# Aggregate DF
dtm_extra <- cbind(dtm_raw_trunc, 
                   mergedf[brexity_topics, 
                      c("speakername", "on_behalf_of_id")]) 

names(dtm_extra)[1:(ncol(dtm_extra) - 2)] <- out$vocab[select_words]

dtm_grouped <- dtm_extra %>% 
  group_by(speakername, on_behalf_of_id) %>% 
  summarise_all(sum)

dtm_grouped <- dtm_grouped %>% 
  filter(!(speakername %in% c("John Bercow", "Theresa May")) & on_behalf_of_id != "")

dtm_grouped_raw <- apply(dtm_grouped[, -c(1, 2)], 1, function(x) x / sum(x)) %>% t

# Run PCA on MPs
pca_grouped <- prcomp(dtm_grouped_raw)

pca_gr_mat <- data.frame(PC1 = pca_grouped$x[, 1], PC2 = pca_grouped$x[, 2], speakername = dtm_grouped$speakername, party = dtm_grouped$on_behalf_of_id)

# Plot results
pca2 <- ggplot(pca_gr_mat %>% filter(party != ""), aes(PC1, PC2)) +
    geom_point(aes(color = party),
               alpha = 0.3) +
    scale_color_manual(values = c("#0087DC", "#D46A4C", "#6AB023", "#DC231F", "#DC231F", "#FAA61A", "#008142", "#FEF987", "#333333", "#333333", "#333333")) +
    theme_sv() +
    xlim(c(-0.2, 0.2)) +
    ylim(c(-0.5, 0.5)) +
    labs(x = "Component 1 (8.6%)", y = "Component 2 (3.8%)") +
    ggtitle("Dimensions in Word Frequency by MP")
ggsave("output/figures/pca2.pdf", width = 7, height = 6)

# Check word weights
fviz_pca_var(pca_grouped,
  repel = TRUE, geom = c("point", "text"),
  select.var = list(contrib = 20),
  ggtheme = theme_classic())

# Plot the two together...
ggarrange(pca1, pca2, ncol = 2, 
          common.legend = TRUE,
          legend = "bottom")
ggsave("output/figures/pca_comb.pdf", width = 10, height = 5)

#
# K-MEANS --> PCA ------------
# 

# Fit kmeans
n_clust <- 6
kmeans_out <- kmeans(as.matrix(dtm_raw_norm), n_clust)

# Get most common cluster words
key_words2 <- matrix(NA, nrow=n_clust, ncol=10)
for(z in 1:n_clust){
	diff <- kmeans_out$center[z, ] - apply(kmeans_out$center[-z, ], 2, mean)
	key_words2[z,]<- colnames(dtm_raw_norm)[order(diff, decreasing=T)[1:10]]
	}
key_words2 %>% t

# Sample speeches from cluster
sample(parl$speech[brexity_topics][kmeans_out$cluster == 7], 5)

# Cluster prevalence by party
table(kmeans_out$cluster, mergedf$on_behalf_of_id[brexity_topics]) %>% apply(., 2, function(x) x / sum(x))

# Aggregate by speaker 
agg_df <- data.frame(cluster = kmeans_out$cluster, party = mergedf$on_behalf_of_id[brexity_topics], mp = mergedf$speakername[brexity_topics]) %>% group_by(mp, cluster, party) %>% summarise(count = n())

agg_df <- agg_df %>% spread(cluster, count)
agg_df[is.na(agg_df)] <- 0
agg_df$total <- rowSums(agg_df[, 3:12])
agg_df <- agg_df %>% filter(total > 10)
agg_df[, 3:12] <- apply(agg_df[, 3:12], 1, function(x) x / sum(x)) %>% t

# Run PCA on speakers
agg_out <- prcomp(agg_df[, 3:12])

agg_mat <- data.frame(PC1 = agg_out$x[, 1], PC2 = agg_out$x[, 2], speakername = agg_df$mp, party = agg_df$party)

labeldf <- agg_mat %>% filter((PC1 > 0.35 | PC1 < -0.3))

ggplot(agg_mat %>% filter(party != ""), aes(PC1, PC2)) +
    geom_jitter(aes(color = party)) +
    scale_color_manual(values = c("#0087DC", "#D46A4C", "#6AB023", "#DC231F", "#DC231F", "#FAA61A", "#008142", "#FEF987", "#333333", "#333333", "#333333")) +
    theme_sv() +
    labs(x = "First Component (45.8%)", y = "Second Component (19.8%)")
ggsave("output/figures/pca_agg.pdf")

fviz_pca_var(agg_out,
  repel = TRUE, geom = c("point", "text"),
  select.var = list(contrib = 20),
  ggtheme = theme_classic())
  ggsave("output/figures/pca_agg_weights.pdf")

#
# STM ------------
# 

add_stop <- import("input/extra_stop.txt")

# DTM process via quanteda
brexity_speeches <- parl$speech[brexity_topics]
corpus_dtm <- dfm(brexity_speeches, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = c(stopwords(source = "smart"), stopwords("english"),                  'friend', 'hon', 'honourable', 'member', 'house', 'govern', 
             'government', 'people', 'minister', 'ministry',
             'secretariat', 'secretary', 'bill', 'debate', 'right',
             'gentleman', 'member', 'year', 'issue', 'state', 'country',
             'support', 'committee', 'member', 'will'), stem = TRUE)
dtm_trim <- dfm_trim(corpus_dtm, min_docfreq = 0.01, max_docfreq = 0.90, docfreq_type = "prop")

# DTM by speaker
speaker_dtm <- dfm(brexity_speeches, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = c(stopwords(source = "smart"), stopwords("english"),                  'friend', 'hon', 'honourable', 'member', 'house', 'govern', 
             'government', 'people', 'minister', 'ministry',
             'secretariat', 'secretary', 'bill', 'debate', 'right',
             'gentleman', 'member', 'year', 'issue', 'state', 'country',
             'support', 'committee', 'member', 'will'), stem = TRUE, group = parl$speakername[brexity_topics])
dtm_speaker_trim <- dfm_trim(speaker_dtm, min_docfreq = 0.01, max_docfreq = 0.90, docfreq_type = "prop")

# Dendrogram
pres_dist_mat <- textstat_dist(dfm_weight(dtm_speaker_trim, "prop"))
# hiarchical clustering the distance object
pres_cluster <- hclust(pres_dist_mat)
# label with document names
# pres_cluster$labels <- docnames(dtm_trim)
# plot as a dendrogram
plot(pres_cluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

### K-MEANS ON DTM -----------------

# K-Means clustering
speaker_clusters <- kmeans(dfm_weight(dtm_speaker_trim, "prop"), 10)
table(speaker_clusters$cluster)

key_words2 <- matrix(NA, nrow=n_clust, ncol=10)
for(z in 1:n_clust){
	diff <- speaker_clusters$center[z, ] - apply(speaker_clusters$center[-z, ], 2, mean)
	key_words2[z,]<- colnames(dtm_speaker_trim)[order(diff, decreasing=T)[1:10]]
	}
key_words2 %>% t

# Two main clusters - about 420 MPs and 180 MPs
table(speaker_clusters$cluster)

### STM ON DTM -----------------

# run topic model on all speeches
topic_count <- 10
dtm2stm <- convert(dtm_trim, to = "stm")
stm_model <- stm(dtm2stm$documents, dtm2stm$vocab, K = topic_count)

labelTopics(stm_model)

brexit_subset <- brexity_topics
brexit_subset[brexity_topics][c(1911, 9998, 14267, 14434)] <- FALSE

#### PCA ON SPEECH THETAS

# run PCA on disaggregated thetas
test_out <- prcomp(stm_model$theta)
biplot(test_out)

# run kmeans on thetas
kmeans_out <- kmeans(stm_model$theta, 4)
table(kmeans_out$cluster)

# bring everything together
stm_thetas <- as.data.frame(stm_model$theta) %>% mutate(mp = parl$speakername[brexit_subset], party = mergedf$on_behalf_of_id[brexit_subset], PC1 = test_out$x[, 1], PC2 = test_out$x[, 2], cluster = kmeans_out$cluster)

# plot speeches
ggplot(stm_thetas, aes(PC1, PC2)) +
    geom_point(aes(color = cluster %>% as.factor), alpha = .3) +
    theme_sv() +
    labs(x = "First Component (20.8%)", y = "Second Component (14.3%)")

# check if differences by party
table(stm_thetas$party, stm_thetas$cluster) %>% prop.table(., 1)

# aggregate by MP
agg_clusters <- stm_thetas %>% group_by(mp, party) %>% 
  summarise(c1 = mean(cluster == 1), 
            c2 = mean(cluster == 2), 
            c3 = mean(cluster == 3),
            c4 = mean(cluster == 4))

# check if clusters are indicative of Brexityness
agg_clusters %>% filter(c3 != 0 & c3 != 1) %>%arrange(c3)
# No...

### PCA ON SPEAKER THETAS -----------------------

# prep for further analysis
stm_bind <- as.data.frame(stm_model$theta) %>% mutate(mp = parl$speakername[brexit_subset], party = mergedf$on_behalf_of_id[brexit_subset])

stm_agg <- stm_bind %>% group_by(mp, party) %>% summarise_all(mean)

# run PCA
stm_pca <- prcomp(stm_agg[, 3:12])

fviz_pca_var(stm_pca,
  repel = TRUE, geom = c("point", "text"),
  select.var = list(contrib = 20),
  ggtheme = theme_classic())
ggsave("output/figures/pca_after_stm_weights.pdf")

stm_mat <- data.frame(PC1 = stm_pca$x[, 1], PC2 = stm_pca$x[, 2], speakername = stm_agg$mp, party = stm_agg$party)

ggplot(stm_mat, aes(PC1, PC2)) +
    geom_point(aes(color = party), alpha = .3) +
    scale_color_manual(values = c("#0087DC", "#D46A4C", "#6AB023", "#DC231F", "#FAA61A", "#008142", "#FEF987", "#333333", "#333333", "#333333")) +
    theme_sv() +
    labs(x = "First Component (20.8%)", y = "Second Component (14.3%)")

ggsave("output/figures/pca_after_stm.pdf")

#
# TOPIC PREVALENCE OVER TIME ------------
#

# Topic prevalence over time
labelTopics(stm_model)

stm_time <- stm_bind %>% 
  mutate(date = as.Date(mergedf$session[brexit_subset])) %>% 
  rename("EUUK" = V1, "SingleMarket" = V2, "Article50" = V3, "Econ/EurATOM" = V4, "CitizensRights" = V5, "WithdrawalAgreement" = V6, "Deal/MeaningfulVote" = V7, "Fisheries" = V8, "NorthernIreland" = V9, "FreeTrade" = V10) %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarise_all(mean) %>% 
  gather(., topic, theta, EUUK:FreeTrade)

stm_plot <- ggplot(stm_time %>% filter(topic %in% c("Article50", "CitizensRights", "Deal/MeaningfulVote", "EUUK", "Fisheries", "EUUK", "Fisheries", "NorthernIreland", "WithdrawalAgreement")), aes(x = month, y = theta)) +
  geom_smooth(aes(colour = topic), se = FALSE, span = .5) +
  geom_vline(xintercept = as.Date("2016-06-23"), lty = "dotted") +
  geom_vline(xintercept = as.Date("2017-03-30"), lty = "dotted") +
  theme_sv() +
  scale_color_manual(values = c(brewer.pal(9, "Set1"), "#111111")) + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Topic Proportion", title = "Prevalence of subtopics across Brexit-related speeches")

ggsave("output/figures/brexit_topic_prevalence.pdf")

ggarrange(parl_topics, stm_plot)
ggsave("output/figures/prev_joint.pdf", width = 10, height = 6)









#####
#save.image("output/clustering.RData")