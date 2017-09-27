# clustering for demographic information 

# variables: 34, age_bin, A2, A3, A4, A7, A8, A9, A10, A12, use_mfp (outcome)

# libraries
library(FactoMineR)
library(factoextra)
library(ggplot2)
#library(randomForest)j



#for (i in 1:length(colnames(demo))){
#  print(table(demo[,i], useNA='ifany'))
#}


# turn all columns into factors
demo <- data.frame(lapply(analysis, factor))

# fit a random forest for imputation
# X <- demo[complete.cases(demo$C34.FP_effective),]
# set.seed(1)
# idx <- sample(1:nrow(X), 0.25*nrow(X), replace = F)
# X_train <- X[-idx,]
# X_test <- X[idx,]
# 
# set.seed(2)
# rf <- randomForest(x = X_train[,-1], y = X_train[,1],
#                    xtest = X_test[,-1], ytest = X_test[,1],
#                    data = demo, importance = T,
#                    keep.forest = T)
# print(rf$confusion)

# multiple correspondence analysis
res.mca <- MCA(demo[,-(1:2)])
res.mca$eig
summary(res.mca)
dimdesc(res.mca)

# plot confidence intervals for vars of intererest in multidim space
plotellipses(res.mca, keepvar = 1)

p <- fviz_mca_ind(res.mca, label="none", habillage=demo$use_MFP,
                  addEllipses=TRUE, ellipse.level=0.95, 
                  title = "Groups by Modern Use", palette = "Set1",
                  legend.title = "Modern Use") +
                  theme_minimal()
print(p)

q <- fviz_mca_ind(res.mca, label="none", habillage=demo$everuse_MFP,
                  addEllipses=TRUE, ellipse.level=0.95,
                  title = "Groups by Potential Modern Use", palette = "Set2",
                  legend.title = "Potential Modern Use")+
                  theme_minimal()
print(q)

require(gridExtra)
grid.arrange(p, q, ncol=2) # Produces side-by-side plots for outcomes of interest

cats <- apply(demo[,-(1:2)], 2, function(x) nlevels(as.factor(x)))
mca_vars_df <- data.frame(res.mca$var$coord, Variable = rep(names(cats), 
                                                         cats))
mca_obs_df <- data.frame(res.mca$ind$coord)

library(car)
scatter3d(mca_obs_df[,1], mca_obs_df[,2], mca_obs_df[,3], groups = demo$use_MFP,
          surface = F, ellipsoid = T)

# hierarchical clustering
set.seed(1)
res.hcpc <- HCPC(res.mca)
res.hcpc$desc.var$test.chi2
res.hcpc$desc.ind

# x2 our clusters with outcomes of interest
chisq.test(res.hcpc$data.clust$clust, demo$use_MFP)
chisq.test(res.hcpc$data.clust$clust, demo$everuse_MFP)

library(GGally)
ggparcoord(res.hcpc$data.clust, columns = c(1:10), groupColumn = 11,
           alphaLines = 0.25, scale = "std", mapping = ggplot2::aes(size = 3)) +
  ggplot2::scale_size_identity() + scale_colour_manual(values = c("1" = "lightgray",
                      "2" = "red", "3" = "lightgray")) + 
  labs(colour="Cluster", x = "", title = "Parallel Coordinates Plot for Cluster 2", y = '')+
  theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())+
  theme_minimal()

ggparcoord(res.hcpc$data.clust[res.hcpc$data.clust$clust==2,], columns = c(1:10),
           alphaLines = 0.05, scale = "uniminmax")+geom_line(size=0.5, alpha = 0.05, colour='red')#+
  #ggplot2::scale_size_identity() + scale_colour_manual(values = c("1" = "lightgray",
  #                                                                "2" = "red", "3" = "lightgray")) + 
  #labs(colour="Cluster", x = "", title = "Parallel Coordinates Plot for Cluster 2", y = '')+
  #theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())+
  #theme_minimal()

