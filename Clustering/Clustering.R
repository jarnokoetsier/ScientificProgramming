#=============================================================================#
# File: Clustering.R
# Date: October 15, 2022										                                      
# Author: Jarno Koetsier                                                      
# Data: 'dataMatrix_filtered.RData','featureInfo.Rdata','sampleInfo_filtered.RData'
#
# R version: 4.2.1 (getRversion())
# RStudio version: 2022.7.1.544 (RStudio.Version())
#=============================================================================#


###############################################################################

# 0. Preparation

###############################################################################

# Clear working environment
rm(list = ls())

# IMPORTANT: Set path to the project folder!!!!
homeDir <- "C:/Users/Gebruiker/Documents/GitHub/ScientificProgramming"

# Set working directory to the clustering folder
setwd(paste0(homeDir, "/Clustering"))

# Install and load required packages
CRANpackages <- c("tidyverse",       # Data formatting and plotting
                  "corrr",           # Calculate pairwise correlations
                  "igraph",          # Network-based clustering
                  'ggraph',          # Network and dendrogram visualizations
                  "RColorBrewer",    # Make color palettes 
                  "randomForest",    # Unsupervised random forest
                  "heatmaply",       # Make heatmaps
                  "gridExtra",       # Combine images in single plot
                  "grid",            # Add elements to the combined image
                  "ggrepel",         # Add labels in plot
                  "caret",           # Create machine learning workflow
                  "glmnet")          # Construct elastic net model

# Versions of required packages
versions <- c("1.3.2",
              "0.4.4",
              "1.3.4",
              "2.0.6",
              "1.1.3",
              "4.7.1.1",
              "1.3.0",
              "2.3",
              "4.2.1",
              "0.9.1",
              "6.0.93",
              "4.1.4")

# Install (if not yet installed) and load the required CRAN packages:
for (pkg in 1:length(CRANpackages)) {
  
  # Install package if not installed yet
  if (!requireNamespace(CRANpackages[pkg], quietly = TRUE)){
    install_version(CRANpackages[pkg], version = versions[pkg], 
                    repos = "http://cran.us.r-project.org")
  }
  # Install package if correct version is not installed yet
  if (packageVersion(CRANpackages[pkg]) != versions[pkg]){
    install_version(CRANpackages[pkg], version = versions[pkg], 
                    repos = "http://cran.us.r-project.org")
  }
  # Load package
  require(as.character(CRANpackages[pkg]), character.only = TRUE)
}

# Load data
load(paste0(homeDir, "/Pre-processing/", "dataMatrix_filtered.RData"))
load(paste0(homeDir, "/Pre-processing/", "featureInfo.Rdata"))
load(paste0(homeDir, "/Pre-processing/", "sampleInfo_filtered.RData"))

# Get Malignant samples only
all(sampleInfo_filtered$id == rownames(dataMatrix_filtered))
dataMatrix_malignant <- dataMatrix_filtered[sampleInfo_filtered$diagnosis == "Malignant",]

# Auto-scale the data ((x - mean)/sd)
dataMatrix_malignant_scaled <- t((t(dataMatrix_malignant) - rowMeans(t(dataMatrix_malignant)))/(apply(t(dataMatrix_malignant),1,sd)))


###############################################################################

# Network-based clustering (Newman-Girvan Algorithm)

###############################################################################


#******************************************************************************#
# PCA for feature selection
#******************************************************************************#


# Perform PCA
pcaList <-  prcomp(dataMatrix_malignant_scaled,        
                   retx = TRUE,
                   center = FALSE,
                   scale = FALSE)


# Calculate explained variances
explVar <- round(((pcaList$sdev^2)/sum(pcaList$sdev^2))*100,2)

# Get cumulative explained variances
cumVar <- rep(NA, length(explVar))
for (var in 1:length(explVar)) {
  if (var != 1){
    cumVar[var] <- cumVar[var - 1] + explVar[var]
  } else{
    cumVar[var] <- explVar[1]
  }
}
# Combine (cumulative) explained variance in data frame for plotting
plotVar <- data.frame(nPC = 1:length(explVar), explVar= explVar, cumVar = cumVar)

# Make explained variance plot
nPCs = 7
explVarPlot <- ggplot(plotVar, aes(x = nPC)) +
  geom_bar(aes(y = explVar, fill = explVar), stat = "identity", color = "black") +
  geom_line(aes(y = cumVar/2.8, group = 1), size = 1, color = "grey") +
  geom_point(aes(y = cumVar/2.8), size = 2) +
  geom_vline(xintercept = nPCs, color = "red", linetype = "dashed") +
  ggtitle("PCA Explained Variation") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Explained Variance (%)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*2.8, name="Cumulative Explained Variance (%)")
  ) + 
  xlab("Principal Components") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        legend.position = "none")

# save plot
ggsave(plot = explVarPlot, filename = "explVarPlot.png", width = 10, height = 8)

# After the first 7 PCs, the explained variance flattens. So, we will use the
# first 7 PCs for clustering

# Get PCA scores
PCA_scores <- as.data.frame(pcaList$x[,1:nPCs])


#******************************************************************************#
# Network construction
#******************************************************************************#

# Construct co-expression matrix
res.cor <- correlate(t(PCA_scores), diagonal = 0)
res.cor <- as.data.frame(res.cor)
row.names(res.cor) <- res.cor$term
res.cor[1] <- NULL

# Filter co-expression matrix
res.cor.filtered <- res.cor
res.cor.filtered[res.cor.filtered < 0.6] <- 0
res.cor.filtered[abs(res.cor.filtered) >= 0.6] <- 1

# Make graph
graph <- igraph::graph_from_adjacency_matrix(as.matrix(res.cor.filtered), weighted=NULL)
graph <- as.undirected(graph, mode = "collapse")

# Find communities in the network
communities <- edge.betweenness.community(graph, directed = FALSE)
clusters <- as.character(communities$membership)

# Check number of samples per cluster:
table(clusters)

# Clusters 1, 2 and 3 are the major clusters
clusters[as.numeric(clusters) > 3] <- "Other"

# Add cluster labels to graph object
V(graph)$membership <- as.character(clusters)

# Make network
plotNetwork <- ggraph(graph, layout = "stress") +
  geom_edge_link(color = "lightgrey") +
  geom_node_point(aes(color = membership), size = 3) +
  theme_void() +
  theme(legend.position = 'none') +
  scale_color_manual(values = c(brewer.pal(3, "Dark2"), "grey"))

# Save plot
ggsave(plotNetwork, file = "NetworkClusters.png", width = 8, height = 8)

# Save clusters from network-based approach
clusters_network <- data.frame(
  ID = communities$names,
  Cluster_Network = clusters
)

###############################################################################

# Unsupervised random forest + Hierarchical clustering

###############################################################################

#******************************************************************************#
# Unsupervised random forest
#******************************************************************************#

# Construct unsupervised random forest model
set.seed(123)
rf <- randomForest(x = dataMatrix_malignant,
                     importance = TRUE,
                     ntree = 1000,
                     nodesize = 10)
# proximity matrix
prox <- rf$proximity

# Convert to dissimilarity (distant) matrix
diss <- 1 - rf$proximity


#******************************************************************************#
# Hierarchical-clustering on dissimilarity martrix
#******************************************************************************#

# Perform hierarchical clustering on dissimilarity matrix
hierClust <- hclust(as.dist(diss), method = "ward.D2")

# Get clusters (k = 3)
clusters <-  cutree(hierClust, k = 3)
clusters_rf <- data.frame(
  ID = names(clusters),
  Cluster_rf = as.character(clusters)
)

# Plot dendrogram
hierPlot <- hierClust
hierPlot$labels <- clusters_rf$Cluster_rf
dendrogram <- ggraph(hierPlot,layout = 'dendrogram', circular = FALSE, height = height) +
  geom_edge_elbow() +
  geom_node_point(aes(filter = leaf, color = label), shape = 15, size = 1) +
  geom_rect(aes(xmin = 0, xmax = table(clusters_rf$Cluster_rf)[1], ymin = 0, ymax = 3),
            fill = "#1B9E77", alpha = 0.005, color = "#1B9E77", size = 1.5) +
  geom_rect(aes(xmin = table(clusters_rf$Cluster_rf)[1], 
                xmax = table(clusters_rf$Cluster_rf)[1] + table(clusters_rf$Cluster_rf)[3], 
                ymin = 0, ymax = 3), fill = "#7570B3", alpha = 0.005, color = "#7570B3", size = 1.5) +
  geom_rect(aes(xmin = table(clusters_rf$Cluster_rf)[1] + table(clusters_rf$Cluster_rf)[3], 
                xmax = table(clusters_rf$Cluster_rf)[1] + table(clusters_rf$Cluster_rf)[3] + table(clusters_rf$Cluster_rf)[2], 
                ymin = 0, ymax = 3), fill = "#D95F02", alpha = 0.005, color = "#D95F02", size = 1.5) +
  labs(color = "Cluster") +
  theme_void() +
  scale_color_brewer(palette = "Dark2")

# Save plot
ggsave(dendrogram, file = "Dendrogram.png", width = 8, height = 8)



###############################################################################

# Combined Cluster visualization

###############################################################################

# Compare network-based and URF-based clusters
table(clusters_rf$Cluster_rf, clusters_network$Cluster_Network)

# So, we see the following three main ensemble clusters:
# -Cluster 1 from the Network and cluster 1 from the URF-Hierarchical clustering
# -Cluster 2 from the Network and cluster 3 from the URF-Hierarchical clustering
# -Cluster 3 from the Network and cluster 2 from the URF-Hierarchical clustering

# Save clusters
clusterDF <- inner_join(clusters_network, clusters_rf, by = c("ID" = "ID"))
save(clusterDF, file = "clusterDF.RData")

#******************************************************************************#
# Heatmap
#******************************************************************************#

# Colors for cluster visualization
colors <- c(brewer.pal(3, "Dark2"), "grey")
levels(colors) <- c("1", "2", "3", "Other")

# Make and save heatmap as html file
heatmaply(
  prox,
  Rowv = as.dendrogram(hierClust),
  Colv = as.dendrogram(hierClust),
  key.title = "Proximity\nValue",
  col_side_colors = data.frame("URF Clusters" = factor(clusterDF$Cluster_rf),
                               "Network Clusters" = factor(clusterDF$Cluster_Network),
                               check.names = FALSE),
  col_side_palette = colorRampPalette(colors),
  showticklabels = c(FALSE, FALSE),
  file = "heatmapClusters.html"
)

#******************************************************************************#
# Principal Coordinate Analysis (PCoA)
#******************************************************************************#

# PCoA is done by performing PCA on the dissimilarity matrix

# Center data (double mean centering)
diss_centered <- diss - rowMeans(diss) - colMeans(diss) + mean(diss)

# Perform PCA
pcoaList <-  prcomp(diss_centered,        
                    retx = TRUE,
                    center = FALSE,
                    scale = FALSE)


# Explained variance
explVarPCoA <- round(((pcoaList$sdev^2)/sum(pcoaList$sdev^2))*100,2)

# Get PCoA scores
plotPCoA_scores <- as.data.frame(pcoaList$x[,1:10])
plotPCoA_scores$ID <- rownames(plotPCoA_scores)

# Combine PCoA scores with sample information
plotPCoA_scores <- inner_join(plotPCoA_scores, sampleInfo_filtered, by = c("ID" = "id"))

# Combine PCoA scores with cluster information
plotPCoA_scores <- inner_join(plotPCoA_scores, clusterDF, by = c("ID" = "ID"))

# Make PCoA score plots:

# 1) Color by URF-based clusters
PCoAurf <- ggplot(plotPCoA_scores, aes(x = PC1, y = PC2, color = Cluster_rf)) +
  geom_point(size = 2, alpha = 0.9) +
  xlab(paste0("PCo1 (", explVarPCoA[1],"%)")) +
  ylab(paste0("PCo2 (", explVarPCoA[2],"%)")) +
  ggtitle("URF-based clusters") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c(brewer.pal(3, "Dark2")))

# 2) Color by network-based clusters
PCoAnetwork <- ggplot(plotPCoA_scores, aes(x = PC1, y = PC2, color = Cluster_Network)) +
  geom_point(size = 2, alpha = 0.9) +
  xlab(paste0("PCo1 (", explVarPCoA[1],"%)")) +
  ylab(paste0("PCo2 (", explVarPCoA[2],"%)")) +
  ggtitle("Network-based clusters") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c(brewer.pal(3, "Dark2"), "grey"))

# Combine into single image and save plot
ggsave("PCoAplot.png", 
       grid.arrange(PCoAurf, 
                    PCoAnetwork, 
                    ncol = 2, 
                    nrow = 1,
                    top = textGrob("PCoA Plot",gp=gpar(fontsize=20,font=2))), 
       width = 12, height = 6)


#******************************************************************************#
# PCA
#******************************************************************************#

# Perform PCA
pcaList <-  prcomp(dataMatrix_malignant_scaled,        
                   retx = TRUE,
                   center = FALSE,
                   scale = FALSE)


# Explained variance
explVar <- round(((pcaList$sdev^2)/sum(pcaList$sdev^2))*100,2)

# collect PCA scores into a data frame
plotPCA_scores <- as.data.frame(pcaList$x)
plotPCA_scores$ID <- rownames(plotPCA_scores)

# Combine data frame with sample and cluster information
plotPCA_scores <- inner_join(plotPCA_scores, sampleInfo_filtered, by = c("ID" = "id"))
plotPCA_scores <- inner_join(plotPCA_scores, clusterDF, by = c("ID" = "ID"))

# Make PCA plots:

# Color by URF-based clusters
PCAurf <- ggplot(plotPCA_scores, aes(x = PC1, y = PC2, color = Cluster_rf)) +
  geom_point(size = 2, alpha = 0.9) +
  xlab(paste0("PC1 (", explVar[1],"%)")) +
  ylab(paste0("PC2 (", explVar[2],"%)")) +
  ggtitle("URF-based clusters") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c(brewer.pal(3, "Dark2")))

# Color by network-based clusters
PCAnetwork <- ggplot(plotPCA_scores, aes(x = PC1, y = PC2, color = Cluster_Network)) +
  geom_point(size = 2, alpha = 0.9) +
  xlab(paste0("PC1 (", explVar[1],"%)")) +
  ylab(paste0("PC2 (", explVar[2],"%)")) +
  ggtitle("Network-based clusters") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c(brewer.pal(3, "Dark2"), "grey"))

# Combine into single image and save plot
ggsave("PCAplot1.png", 
       grid.arrange(PCAurf, 
                    PCAnetwork,
                    ncol = 2, 
                    nrow = 1,
                    top = textGrob("PCA Plot",gp=gpar(fontsize=20,font=2))), 
       width = 10, height = 6)


# Combine clusters
plotPCA_scores$combined <- paste0(plotPCA_scores$Cluster_Network, "/", plotPCA_scores$Cluster_rf)

# As we saw before, the largest overlap of clusters is:
# -Cluster 1 from the Network and cluster 1 from the URF-Hierarchical clustering
# -Cluster 2 from the Network and cluster 3 from the URF-Hierarchical clustering
# -Cluster 3 from the Network and cluster 2 from the URF-Hierarchical clustering

# All other combinations will thus be set to be "Other" for the visualization:
plotPCA_scores$combined[!(plotPCA_scores$combined%in% c("1/1", "2/3", "3/2"))] <- "Other"


# Get PCA projections of beneign samples:

# 1) Get benign samples
dataMatrix_benign <- dataMatrix_filtered[!(rownames(dataMatrix_filtered) %in% rownames(dataMatrix_malignant)),]

# 2) Scale benign samples using the malignant samples
benign_scaled <-  t((t(dataMatrix_benign) - rowMeans(t(dataMatrix_malignant)))/(apply(t(dataMatrix_malignant),1,sd)))

# 3) Calculate the projections
projectBenign <- as.data.frame(as.matrix(benign_scaled) %*% as.matrix(pcaList$rotation))
projectBenign$ID <- rownames(projectBenign)
projectBenign$combined <- rep("Benign", nrow(projectBenign))

# Collect PCA loadings into a data frame
plotPCA_loadings <- data.frame(pcaList$rotation)
plotPCA_loadings$Feature <- rownames(plotPCA_loadings)
plotPCA_loadings <- inner_join(plotPCA_loadings, featureInfo, by = c("Feature" = "Name"))


# PCA plot color by combined clusters/diagnosis
PCAcombined <- ggplot(plotPCA_scores, aes(x = PC1, y = PC2, color = combined)) +
  geom_point(size = 2) +
  geom_point(data = projectBenign, aes(x = PC1, y = PC2, color = combined),alpha = 0.7) +
  xlab(paste0("PC1 (", explVar[1],"%)")) +
  ylab(paste0("PC2 (", explVar[2],"%)")) +
  ggtitle("Score Plot") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c(brewer.pal(3, "Set1"),  "grey", "black"))

# Make loading plot
loading_plot <- ggplot(plotPCA_loadings, aes(x = PC1, y = PC2, label = Name1, color = Statistic)) +
  geom_segment(data = plotPCA_loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), color = "grey") +
  geom_point() +
  geom_text_repel(size = 2.5, max.overlaps = 50) +
  ggtitle(label = "Loading Plot") +
  xlab(paste0("PC1 (", explVar[1],"%)")) +
  ylab(paste0("PC2 (", explVar[2],"%)")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_brewer(palette = "Set1")


# Combine into single image and save plot
ggsave("PCAplot2.png", 
       grid.arrange(PCAcombined, 
                    loading_plot,
                    ncol = 2, 
                    nrow = 1,
       bottom = textGrob("NOTE: The PCA model is constructed using the malignant samples only. The benign samples are projected.",gp=gpar(fontsize=10,font=3))),
       width = 10, height = 6)


###############################################################################

# Cluster evaluation

###############################################################################


#******************************************************************************#
# Find the important variables for the cluster definitions
#******************************************************************************#

# Get data from malignant samples
plotMalignant <- dataMatrix_malignant

# Add cluster labels to malignant samples
plotMalignant$ID <- rownames(plotMalignant)
plotMalignant<- inner_join(plotMalignant, clusterDF, by = c("ID" = "ID"))

# Combine the two clusters
plotMalignant$combined <- paste0(plotMalignant$Cluster_Network, "/", plotMalignant$Cluster_rf)
plotMalignant$combined[!(plotMalignant$combined%in% c("1/1", "2/3", "3/2"))] <- "Other"

# Get data from benign samples
plotBenign <- dataMatrix_filtered[!(rownames(dataMatrix_filtered) %in% rownames(dataMatrix_malignant)),]
plotBenign$ID <- rownames(plotBenign)
plotBenign$Cluster_Network <- rep(NA, nrow(plotBenign))
plotBenign$Cluster_rf <- rep(NA, nrow(plotBenign))
plotBenign$combined <- rep("Benign", nrow(plotBenign))

# combine malignant and benign samples
plotAll <- rbind.data.frame(plotMalignant, plotBenign)

# In the loading plot (PCAplot2.png), we saw that Mean Compactness and Mean Area 
# might be able to distinguish between the clusters. So, let's make a scatter 
# plot using these two variables
scatterPlot <- ggplot() +
  geom_point(data = plotAll[plotAll$combined == "Benign",], aes(x = exp(area_mean)-1, y = exp(compactness_mean)-1,  color = "Beneign"), size = 2, alpha = 0.7) +
  geom_point(data = plotAll[plotAll$combined != "Benign",], aes(x = exp(area_mean)-1, y = exp(compactness_mean)-1, color = combined), size = 3, alpha = 1) +
  labs(color = "Cluster\n(Network/URF)") +
  xlab("Mean Area") +
  ylab("Mean Compactness") +
  theme_classic() +
  scale_color_manual(values = c(brewer.pal(3, "Set1"), "grey", "black"))

# Save scatter plot
ggsave(scatterPlot, file = "ScatterPlot.png", width = 10, height = 8)

# Make box/violin plot of the Mean Area for the different clusters
boxPlot_area <- ggplot(plotAll[plotAll$combined != "Other",], aes(x = combined, y = exp(area_mean)-1)) +
  geom_violin(aes(fill = combined), alpha = 0.5) +
  geom_boxplot(width=0.1, fill="white")+
  geom_jitter(aes(color = combined),position=position_jitter(0.2)) +
  ylab("Mean Area") +
  xlab(NULL) +
  ggtitle("Mean Area") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "none") +
  scale_color_manual(values = c(brewer.pal(3, "Set1"), "grey")) +
  scale_fill_manual(values = c(brewer.pal(3, "Set1"), "grey"))

# Make box/violin plot of the Mean Compactness for the different clusters
boxPlot_compactness <- ggplot(plotAll[plotAll$combined != "Other",], aes(x = combined, y = exp(compactness_mean)-1)) +
  geom_violin(aes(fill = combined), alpha = 0.5) +
  geom_boxplot(width=0.1, fill="white")+
  geom_jitter(aes(color = combined),position=position_jitter(0.2)) +
  ylab("Mean Compactness") +
  xlab(NULL) +
  labs(color = "Cluster (Network/URF)", fill = "Cluster (Network/URF)") +
  ggtitle("Mean Compactness") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 10),
        legend.position = "bottom") +
  scale_color_manual(values = c(brewer.pal(3, "Set1"), "grey")) +
  scale_fill_manual(values = c(brewer.pal(3, "Set1"), "grey"))

# Save boxplots into a single image
ggsave("clusterEvaluation.png", 
       grid.arrange(boxPlot_area,
                    boxPlot_compactness,
                    ncol = 1, 
                    nrow = 2), 
       width = 10, height = 10)

#.............................................................................#
# Mean Area and Mean compactness indeed seems to mostly explain the differences
# between the three clusters!
#.............................................................................#

#******************************************************************************#
# Link classification performance to clusters
#******************************************************************************#

# Folder that contains the classification data
classificationFolder <- paste0(homeDir, "/Classification/")

# Load data
load(paste0(classificationFolder,"trainingData_filtered.RData"))
load(paste0(classificationFolder, "finalModel.RData"))
load(paste0(classificationFolder, "modelInfo.RData"))

# Get class probabilities of training data using the EN model
prob <- as.data.frame(predict(finalModel, trainingData_filtered, type = "response"))

# Prepare class probability data frame:

# Add sample IDs
prob$ID <- rownames(prob)

# Combine with class labels
prob <- inner_join(prob, sampleInfo_filtered, by = c("ID" = "id"))

# Select malignant samples only
prob_malignant <- prob[prob$diagnosis == "Malignant",]

# Add cluster information
prob_malignant <- inner_join(prob_malignant, clusterDF, by = c("ID" = "ID"))
prob_malignant$combined <- paste0(prob_malignant$Cluster_Network, "/", prob_malignant$Cluster_rf)
prob_malignant$combined[!(prob_malignant$combined%in% c("1/1", "2/3", "3/2"))] <- "Other"

# Make plot: class probability for each cluster
ClusterProbabilities <- ggplot() +
  geom_violin(data = prob_malignant[prob_malignant$combined != "Other",], 
              aes(x = combined, y = s0, fill = combined),alpha = 0.2, scale = "width",
              draw_quantiles = 0.5) +
  geom_jitter(data = prob_malignant[prob_malignant$combined != "Other",], 
              aes(x = combined, y = s0, color = combined),position=position_jitter(0.2),
              size = 2) +
  geom_hline(yintercept = 0.5, size = 1, color = "grey", linetype = "dashed") +
  ylab("Class Probability") +
  xlab("Cluster (Network/URF)") +
  labs(color = "Cluster\n(Network/URF)", fill = "Cluster\n(Network/URF)") +
  scale_color_manual(values = c(brewer.pal(3, "Set1"))) +
  scale_fill_manual(values = c(brewer.pal(3, "Set1"))) +
  theme_classic()

# NOTE: warning message can be safely ignored :)

# Save plot
ggsave(ClusterProbabilities, file = "ClusterProbabilities.png", width = 10, height = 8)

#.............................................................................#
# As we already saw in the PCA plot and scatter plot. The red cluster encompasses
# more extreme malignant samples that can easily be distinguished from the benign
# samples. In contrast, the blue and the green clusters are more closely related
# to the benign samples and are thus more difficult to predict.
#.............................................................................#

################################################################################

# End of the script

################################################################################