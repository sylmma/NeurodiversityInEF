# Self-organising maps                                                                                                                                                  #
# ----------------------------------------------------- Load packages ----------------------------------------------------------------------------####
#manipulation
library(dplyr);library(psych); library(kohonen);library(uwot); library(NbClust); library(ggpubr)
# -----------------------------------------------------  Read data
calm800<-read.csv('calm800_oa.csv')
clustMeasuresStd<-c('awma_digit_recall_standard', 'awma_backward_digit_standard',
                    'awma_dot_matrix_standard', 'awma_mr_x_standard',
                    'trails_number_letter_switching_scaled', 'tower_total_achievement_scaled',
                    'teach2_bark.vigil_scaled_score', 'teach2_rbbs_scaled_score','brief_initiate_t',
                    'brief_inhibit_t', 'brief_monitor_t',
                    'brief_shift_t', 'brief_organisation_t', 'brief_planning_t',
                    'brief_working_memory_t','brief_emotinol_control_t')
# ---------------------------------------------------- Imputation ------------------------------------------------------------------------------####
#For std scores---
calm800_old$sex<-as.factor(calm800_old$sex)
calm800_old$age_in_months<-as.integer(calm800_old$age_in_months)
calm800_old[,c(clustMeasuresStd)]<-as.data.frame(apply(calm800_old[,c(clustMeasuresStd)],2,as.numeric))
#Random forest imputation 
set.seed(10); calmOldImpStd<-missForest(calm800_old[,c(clustMeasuresStd,"age_in_months","sex")],maxiter=10)
calmOldImpStd$OOBerror
calm800_old[,c(clustMeasuresStd)]<-calmOldImpStd$ximp[,c(clustMeasuresStd)]
write.csv(calm800_old,file='calm800_old_after_imputation.csv', row.names=F)
# ---------------------------------------------------- Transfrom data ------------------------------------------------------------------------------####
#2 Define conversion functions to z-scores
t2zscore<-function (x, print=T) {(x-50)/10} #M=50, SD=10; Conners, Brief, Matrix reasoning, RCADS
scaled2zscore<-function (x,print=T) {(x-10)/3} #M=10, SD=3; CCC2, DKEFS
std2zscore<-function (x,print=T) {(x-100)/15} #M=100, SD=15; AWMA

#transform to z-scores
calm800_old[,c(paste("z",clustMeasuresStd[grepl("_t",clustMeasuresStd)], sep="."))]<-apply(calm800_old[,clustMeasuresStd[grepl("_t",clustMeasuresStd)]],2,t2zscore)
calm800_old[,c(paste("z",clustMeasuresStd[grepl("_standard",clustMeasuresStd)], sep="."))]<-apply(calm800_old[,clustMeasuresStd[grepl("_standard",clustMeasuresStd)]],2,std2zscore)
calm800_old[,c(paste("z",clustMeasuresStd[grepl("caled",clustMeasuresStd)], sep="."))]<-apply(calm800_old[,clustMeasuresStd[grepl("caled",clustMeasuresStd)]],2,scaled2zscore)
z.clustMeasuresStd<-paste("z",clustMeasuresStd , sep =".")
# ---------------------------------------------------- SOMs ------------------------------------------------------------------------------####
#training data
data_train_scaled <-  as.matrix(calm800_old[,c(z.clustMeasuresStd)])
grid.size <- ceiling(566 ^ (1/2.5))
dim(data_train_scaled)

#train som
set.seed(5); som_ef<- kohonen::som(data_train_scaled, 
                                   grid = kohonen::somgrid(10, 10, "hexagonal"),
                                   dist.fcts = "euclidean",
                                   rlen = 1000)

#som results
somQuality(som_ef, data_train_scaled)
plot(som_ef, type="changes")
plot(som_ef, type="count", main="Node Counts")
summary(som_ef)
# ---------------------------------------------------- Clustering ------------------------------------------------------------------------------####
set.seed(4); umapSOMweights_ef<-uwot::umap(som_ef$codes[[1]],n_neighbors = 10,metric = 'euclidean')
umapSOMweightsDf_ef<-data.frame(d1= umapSOMweights_ef[,1], d2 = umapSOMweights_ef[,2])
set.seed(20); kmeansSom_ef<- NbClust::NbClust(umapSOMweightsDf_ef,  min.nc=2, max.nc=8,  method = "average", index = c("all"))
table(kmeansSom_ef$Best.partition)
cluster_assignment <- kmeansSom_ef$Best.partition[som_ef$unit.classif]
table(cluster_assignment)
calm800_old$cluster<-cluster_assignment 

# ---------------------------------------------------- Compare clusters ------------------------------------------------------------------------------####
#variables in SOM
meltEfData<-reshape2::melt(calm800_old[,c('cluster',z.clustMeasuresStd)], id='cluster')
compareEfData<-compare_means(value ~ cluster, data = meltRadar,
                                 group.by = 'variable',
                                 p.adjust.method = 'fdr',
                                 method = "t.test")%>%
  mutate(p.adj.round = round(p.adj,3),stars= cut(p.adj, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf),
                    label=c("***", "**", "*", "ns")))

#validation measures
ValidationMeasures<-c('wiat_reading_std', 'wiat_numerical_std','rcadsc_total_anx_depression_t',
                      'ccc2_social_scaled','ccc2_interests_scaled','ccc2_gcc',
                      'conners_inattention_t','conners_hyperactivity_impulsivity_t')

meltValidation<-reshape2::melt(calm800_old[,c('cluster',ValidationMeasures)], id='cluster')

compareValidationData<-compare_means(value ~ cluster, data = meltValidation,
                             group.by = 'variable',
                             p.adjust.method = 'fdr',
                             method = "t.test")%>%
  mutate(p.adj.round = round(p.adj,3),stars= cut(p.adj, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf),
                                                 label=c("***", "**", "*", "ns")))
