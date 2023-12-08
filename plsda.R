#imaging data
# ----------------------------------------------------- Load packages ----------------------------------------------------------------------------####
#plsda
.libPaths(c(.libPaths(),'/home/sm05/x86_64-redhat-linux-gnu-library/3.6'))
library(withr,lib.loc='/home/sm05/Rlibs')
library(ggplot2,lib.loc='/home/sm05/Rlibs')
library(mixOmics,lib.loc='/home/sm05/Rlibs')
library(coda,lib.loc='/home/sm05/Rlibs')
library(MCMCpack,lib.loc='/home/sm05/Rlibs')
setwd('/home/sm05/CALM_data/Permutation')
neuralDataHubsStrenthControls<-read.csv('neuralDataHubsYeo7StrengthControls.csv')
nperm<-1000


#fit model----
x_all_HubsStrenthControls <- neuralDataHubsStrenthControls[!(colnames(neuralDataHubsStrenthControls) %in% c('cluster4'))]
y_all_HubsStrenthControls <- as.factor(neuralDataHubsStrenthControls$cluster4)
x_orig<-x_all_HubsStrenthControls
y_orig<-y_all_HubsStrenthControls

#fit
plsda.model2_HubsStrenthControls <- mixOmics::plsda(x_all_HubsStrenthControls, y_all_HubsStrenthControls,
                                                    ncomp = 2, scale =T) 
#evaluate
set.seed(12)
perf.plsda.model2_HubsStrenthControls <- perf(plsda.model2_HubsStrenthControls, validation = "Mfold", folds = 5, 
                                              progressBar = F, auc = TRUE, nrepeat = 50,
                                              cpus=32) 

#permutation----
perm_plsda.model2_HubsStrenthControls<-list()
perm_perf.plsda.model2_HubsStrenthControls<-list()
sample_for_perm<-list()
set.seed(112); for (i in 1:nperm){
  sample_for_perm[[i]] <- sample(nrow(x_orig)) 
  y_perm <- y_orig[sample_for_perm[[i]]] #Permuted data
  perm_model2_fit <- mixOmics::plsda(x_orig, y_perm, scale =T, ncomp=2)
  perf_perm_model2_fit <- perf(perm_model2_fit, validation = "Mfold", folds = 5, cpus=32,
                               progressBar = F, auc = TRUE, nrepeat = 50) 
  perm_model2_fit$loadings$X[,"comp1"] <- procrustes(as.matrix(perm_model2_fit$loadings$X[,"comp1"]),as.matrix(plsda.model2_HubsStrenthControls$loadings$X[,"comp1"]))$X.new
  perm_model2_fit$loadings$Y[,"comp1"] <- procrustes(as.matrix(perm_model2_fit$loadings$Y[,"comp1"]),as.matrix(plsda.model2_HubsStrenthControls$loadings$Y[,"comp1"]))$X.new
  perm_model2_fit$loadings$X[,"comp2"] <- procrustes(as.matrix(perm_model2_fit$loadings$X[,"comp2"]),as.matrix(plsda.model2_HubsStrenthControls$loadings$X[,"comp2"]))$X.new
  perm_model2_fit$loadings$Y[,"comp2"] <- procrustes(as.matrix(perm_model2_fit$loadings$Y[,"comp2"]),as.matrix(plsda.model2_HubsStrenthControls$loadings$Y[,"comp2"]))$X.new 
  perm_plsda.model2_HubsStrenthControls[[i]]<-perm_model2_fit
  perm_perf.plsda.model2_HubsStrenthControls[[i]]<-perf_perm_model2_fit
  cat(i)}

save(perm_plsda.model2_HubsStrenthControls, file = '/home/sm05/sm05owncloud/CALM Project 9 - EF performance vs rating/Analysis/perm_plsda.model2_HubsStrenthControls.RData')
save(perm_perf.plsda.model2_HubsStrenthControls, file = '/home/sm05/sm05owncloud/CALM Project 9 - EF performance vs rating/Analysis/perm_perf.plsda.model2_HubsStrenthControls.RData')

#results-----
#component 1: comparisons-----
#observed
orig_loading_group1_comp1_HubsStrenthControls<-plsda.model2_HubsStrenthControls$loadings$Y[1,'comp1']
orig_loading_group2_comp1_HubsStrenthControls<-plsda.model2_HubsStrenthControls$loadings$Y[2,'comp1']
orig_loading_group3_comp1_HubsStrenthControls<-plsda.model2_HubsStrenthControls$loadings$Y[3,'comp1']
orig_loading_group4_comp1_HubsStrenthControls<-plsda.model2_HubsStrenthControls$loadings$Y[4,'comp1']

#permuted
perm_loadings_group1_comp1_HubsStrenthControls<-simplify2array(lapply(perm_plsda.model2_HubsStrenthControls, function(x) x$loadings$Y[1,'comp1']))
perm_loadings_group2_comp1_HubsStrenthControls<-simplify2array(lapply(perm_plsda.model2_HubsStrenthControls, function(x) x$loadings$Y[2,'comp1']))
perm_loadings_group3_comp1_HubsStrenthControls<-simplify2array(lapply(perm_plsda.model2_HubsStrenthControls, function(x) x$loadings$Y[3,'comp1']))
perm_loadings_group4_comp1_HubsStrenthControls<-simplify2array(lapply(perm_plsda.model2_HubsStrenthControls, function(x) x$loadings$Y[4,'comp1']))


#p-values
p_comp1_1vs2_HubsStrenthControls<-sum(abs(perm_loadings_group1_comp1_HubsStrenthControls - perm_loadings_group2_comp1_HubsStrenthControls) >=
                                        abs(orig_loading_group1_comp1_HubsStrenthControls- orig_loading_group2_comp1_HubsStrenthControls))/ nperm

p_comp1_1vs3_HubsStrenthControls<-sum(abs(perm_loadings_group1_comp1_HubsStrenthControls - perm_loadings_group3_comp1_HubsStrenthControls) >=
                                        abs(orig_loading_group1_comp1_HubsStrenthControls- orig_loading_group3_comp1_HubsStrenthControls))/ nperm

p_comp1_2vs3_HubsStrenthControls<-sum(abs(perm_loadings_group2_comp1_HubsStrenthControls - perm_loadings_group3_comp1_HubsStrenthControls) >=
                                        abs(orig_loading_group2_comp1_HubsStrenthControls- orig_loading_group3_comp1_HubsStrenthControls))/ nperm

p_comp1_1vs4_HubsStrenthControls<-sum(abs(perm_loadings_group1_comp1_HubsStrenthControls - perm_loadings_group4_comp1_HubsStrenthControls) >=
                                        abs(orig_loading_group1_comp1_HubsStrenthControls- orig_loading_group4_comp1_HubsStrenthControls))/ nperm

p_comp1_2vs4_HubsStrenthControls<-sum(abs(perm_loadings_group2_comp1_HubsStrenthControls - perm_loadings_group4_comp1_HubsStrenthControls) >=
                                        abs(orig_loading_group2_comp1_HubsStrenthControls- orig_loading_group4_comp1_HubsStrenthControls))/ nperm

p_comp1_3vs4_HubsStrenthControls<-sum(abs(perm_loadings_group3_comp1_HubsStrenthControls - perm_loadings_group4_comp1_HubsStrenthControls) >=
                                        abs(orig_loading_group3_comp1_HubsStrenthControls- orig_loading_group4_comp1_HubsStrenthControls))/ nperm



paste(p_comp1_1vs2_HubsStrenthControls, 'p-value model2 comp1 1 vs 2')
paste(p_comp1_1vs3_HubsStrenthControls, 'p-value model2 comp1 1 vs 3')
paste(p_comp1_2vs3_HubsStrenthControls, 'p-value model2 comp1 2 vs 3')
paste(p_comp1_1vs4_HubsStrenthControls, 'p-value model2 comp1 1 vs 4')
paste(p_comp1_2vs4_HubsStrenthControls, 'p-value model2 comp1 2 vs 4')
paste(p_comp1_3vs4_HubsStrenthControls, 'p-value model2 comp1 3 vs 4')

#componet 2: comparisons-----
#observed
orig_loading_group1_comp2_HubsStrenthControls<-plsda.model2_HubsStrenthControls$loadings$Y[1,'comp2']
orig_loading_group2_comp2_HubsStrenthControls<-plsda.model2_HubsStrenthControls$loadings$Y[2,'comp2']
orig_loading_group3_comp2_HubsStrenthControls<-plsda.model2_HubsStrenthControls$loadings$Y[3,'comp2']
orig_loading_group4_comp2_HubsStrenthControls<-plsda.model2_HubsStrenthControls$loadings$Y[4,'comp2']

#permuted
perm_loadings_group1_comp2_HubsStrenthControls<-simplify2array(lapply(perm_plsda.model2_HubsStrenthControls, function(x) x$loadings$Y[1,'comp2']))
perm_loadings_group2_comp2_HubsStrenthControls<-simplify2array(lapply(perm_plsda.model2_HubsStrenthControls, function(x) x$loadings$Y[2,'comp2']))
perm_loadings_group3_comp2_HubsStrenthControls<-simplify2array(lapply(perm_plsda.model2_HubsStrenthControls, function(x) x$loadings$Y[3,'comp2']))
perm_loadings_group4_comp2_HubsStrenthControls<-simplify2array(lapply(perm_plsda.model2_HubsStrenthControls, function(x) x$loadings$Y[4,'comp2']))

#p-values
p_comp2_1vs2_HubsStrenthControls<-sum(abs(perm_loadings_group1_comp2_HubsStrenthControls - perm_loadings_group2_comp2_HubsStrenthControls) >=
                                        abs(orig_loading_group1_comp2_HubsStrenthControls- orig_loading_group2_comp2_HubsStrenthControls))/ nperm

p_comp2_1vs3_HubsStrenthControls<-sum(abs(perm_loadings_group1_comp2_HubsStrenthControls - perm_loadings_group3_comp2_HubsStrenthControls) >=
                                        abs(orig_loading_group1_comp2_HubsStrenthControls- orig_loading_group3_comp2_HubsStrenthControls))/ nperm

p_comp2_2vs3_HubsStrenthControls<-sum(abs(perm_loadings_group2_comp2_HubsStrenthControls - perm_loadings_group3_comp2_HubsStrenthControls) >=
                                        abs(orig_loading_group2_comp2_HubsStrenthControls- orig_loading_group3_comp2_HubsStrenthControls))/ nperm


p_comp2_1vs4_HubsStrenthControls<-sum(abs(perm_loadings_group1_comp2_HubsStrenthControls - perm_loadings_group4_comp2_HubsStrenthControls) >=
                                        abs(orig_loading_group1_comp2_HubsStrenthControls- orig_loading_group4_comp2_HubsStrenthControls))/ nperm

p_comp2_2vs4_HubsStrenthControls<-sum(abs(perm_loadings_group2_comp2_HubsStrenthControls - perm_loadings_group4_comp2_HubsStrenthControls) >=
                                        abs(orig_loading_group2_comp2_HubsStrenthControls- orig_loading_group4_comp2_HubsStrenthControls))/ nperm

p_comp2_3vs4_HubsStrenthControls<-sum(abs(perm_loadings_group3_comp2_HubsStrenthControls - perm_loadings_group4_comp2_HubsStrenthControls) >=
                                        abs(orig_loading_group3_comp2_HubsStrenthControls- orig_loading_group4_comp2_HubsStrenthControls))/ nperm



paste(p_comp2_1vs2_HubsStrenthControls, 'p value model2 comp 2 vs 2')
paste(p_comp2_1vs3_HubsStrenthControls, 'p value model2 comp 2 vs 3')
paste(p_comp2_2vs3_HubsStrenthControls, 'p value model2 comp 2 vs 3')
paste(p_comp2_1vs4_HubsStrenthControls, 'p value model2 comp 1 vs 4')
paste(p_comp2_2vs4_HubsStrenthControls, 'p value model2 comp 2 vs 4')
paste(p_comp2_3vs4_HubsStrenthControls, 'p value model2 comp 3 vs 4')


#BER
orig_BER_model2_HubsStrenthControls<-perf.plsda.model2_HubsStrenthControls$error.rate$BER[2,3]
perm_BER_model2_HubsStrenthControls<-simplify2array(lapply(perm_perf.plsda.model2_HubsStrenthControls, function(x) x$error.rate$BER[2,3]))

#p-value
p_BER_model2<-sum(abs(perm_BER_model2_HubsStrenthControls - mean(orig_BER_model2_HubsStrenthControls))
                  >= abs(mean(perm_BER_model2_HubsStrenthControls) - mean(orig_BER_model2_HubsStrenthControls))) / nperm
paste(p_BER_model2, 'p BER model 2')

#bootstrap-----
orig_fit_comp2<-plsda.model2_HubsStrenthControls
data <- cbind(x_all_Yeo7HubsStrength, y_all_Yeo7HubsStrength)


X_boot2_comp1 <- function(d,j){
  d2 <- d[j,]
  rownames(d2)<- NULL
  perm_fit<- mixOmics::plsda(d2[,1:ncol(x_orig)], d2[,(ncol(x_orig)+1):ncol(d2)],  ncomp =2, scale=TRUE)
  perm_fit$loadings$X[,'comp1'] <- procrustes(as.matrix(perm_fit$loadings$X[,'comp1']),as.matrix(orig_fit_comp2$loadings$X[,'comp1']))$X.new
  return(perm_fit$loadings$X[,'comp1'])
}

X_boot2_comp2 <- function(d,j){
  d2 <- d[j,]
  rownames(d2)<- NULL
  perm_fit<- mixOmics::plsda(d2[,1:ncol(x_orig)], d2[,(ncol(x_orig)+1):ncol(d2)],  ncomp =2, scale=TRUE)
  perm_fit$loadings$X[,'comp2'] <- procrustes(as.matrix(perm_fit$loadings$X[,'comp2']),as.matrix(orig_fit_comp2$loadings$X[,'comp2']))$X.new
  return(perm_fit$loadings$X[,'comp2'])
}


Y_boot2_comp1 <- function(d,j){
  d2 <- d[j,]
  rownames(d2)<- NULL
  perm_fit <- mixOmics::plsda(d2[,1:ncol(x_orig)], d2[,(ncol(x_orig)+1):ncol(d2)],  ncomp =2, scale=TRUE)
  perm_fit$loadings$Y[,'comp1'] <- procrustes(as.matrix(perm_fit$loadings$Y[,'comp1']),as.matrix(orig_fit_comp2$loadings$Y[,'comp1']))$X.new
  return(perm_fit$loadings$Y[,'comp1'])
}

Y_boot2_comp2 <- function(d,j){
  d2 <- d[j,]
  rownames(d2)<- NULL
  perm_fit <- mixOmics::plsda(d2[,1:ncol(x_orig)], d2[,(ncol(x_orig)+1):ncol(d2)],  ncomp =2, scale=TRUE)
  perm_fit$loadings$Y[,'comp2'] <- procrustes(as.matrix(perm_fit$loadings$Y[,'comp2']),as.matrix(orig_fit_comp2$loadings$Y[,'comp2']))$X.new
  return(perm_fit$loadings$Y[,'comp2'])
}

#run and save
#component 1
set.seed(12);X_boot2_result_comp1 <- boot(data, X_boot2_comp1, R=nboots)
save(X_boot2_result_comp1, file = '/home/sm05/sm05owncloud/CALM Project 9 - EF performance vs rating/X_boot2_result_comp1_HubsStrength.RData')
set.seed(12);Y_boot2_result_comp1 <- boot(data, Y_boot2_comp1, R=nboots)
save(Y_boot2_result_comp1, file = '/home/sm05/sm05owncloud/CALM Project 9 - EF performance vs rating/Y_boot2_result_comp1_HubsStrength.RData')

#component 2
set.seed(12);X_boot2_result_comp2 <- boot(data, X_boot2_comp2, R=nboots)
save(X_boot2_result_comp2, file = '/home/sm05/sm05owncloud/CALM Project 9 - EF performance vs rating/X_boot2_result_comp2_HubsStrength.RData')
set.seed(12);Y_boot2_result_comp2 <- boot(data, Y_boot2_comp2, R=nboots)
save(Y_boot2_result_comp2, file = '/home/sm05/sm05owncloud/CALM Project 9 - EF performance vs rating/Y_boot2_result_comp2_HubsStrength.RData')


#results TWO COMPONENTS----
#component 1
boot.ci(X_boot2_result_comp1,type=c("basic"),index=1)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=2)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=3)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=4)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=5)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=6)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=7)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=8)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=9)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=10)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=11)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=12)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=13)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=14)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=15)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=16)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=17)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=18)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=19)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=20)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=21)
boot.ci(X_boot2_result_comp1,type=c("basic"),index=22)

boot.ci(Y_boot2_result_comp1,type=c("basic"),index=1)
boot.ci(Y_boot2_result_comp1,type=c("basic"),index=2)
boot.ci(Y_boot2_result_comp1,type=c("basic"),index=3)
boot.ci(Y_boot2_result_comp1,type=c("basic"),index=4)

#component 2
boot.ci(X_boot2_result_comp2,type=c("basic"),index=1)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=2)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=3)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=4)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=5)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=6)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=7)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=8)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=9)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=10)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=11)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=12)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=13)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=14)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=15)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=16)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=17)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=18)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=19)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=20)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=21)
boot.ci(X_boot2_result_comp2,type=c("basic"),index=22)

boot.ci(Y_boot2_result_comp2,type=c("basic"),index=1)
boot.ci(Y_boot2_result_comp2,type=c("basic"),index=2)
boot.ci(Y_boot2_result_comp2,type=c("basic"),index=3)
boot.ci(Y_boot2_result_comp2,type=c("basic"),index=4)








