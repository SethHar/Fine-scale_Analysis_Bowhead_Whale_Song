


#### FINE SCALE ANALYSIS OF BOWHEAD WHALE SONG ####




# The following code performs all analysis involved in this study

# Each song csv contains acoustic parameters available in Raven Pro (listed in paper), alongside the following the following columns:
# - QualName: the qualitative name assigned to each unit 
# - StartFreqHz: the start frequency of unit at 5% energy (found using MATLAB code)
# - EndFreqHz: the end frequency of unit at 95% energy (found using MATLAB code)
# - Source: the unit's source file 
# - Inflections: the number of inflections found within a unit (counted manually)
# - Rendition: the song rendition (or song cycle) that a unit occurs within (multiple renditions per source recording)
# - CompleteRendition: whether the unit is part of an entire rendition, or whether part of an incomplete rendition (Y/N)

rm(list = ls())

#### 1. Load in data ####

Dec2020_song <- read.csv("Dec2020_song.csv", header = TRUE)
Dec2014_song <- read.csv("Dec2014_song.csv", header = TRUE)
Dec2014_song <- subset(Dec2014_song, Dec2014_song$Source!= "Dec07_2014_0800") # removed as multiple singers
Dec2011_song <- read.csv("Dec2011_song.csv", header = TRUE)
AllSongUnits <- rbind(Dec2020_song, Dec2014_song, Dec2011_song)

Dec2020_complete_renditions <- subset(Dec2020_song, Dec2020_song$CompleteRendition== "Y")
Dec2020_complete_renditions <- subset(Dec2020_complete_renditions,Dec2020_complete_renditions$Rendition != "19") # 3 unit cycle removed
Dec2020tv_complete_renditions <- subset(Dec2020_complete_renditions,Dec2020_complete_renditions$QualName != "N") 
Dec2014_complete_renditions <- subset(Dec2014_song, Dec2014_song$CompleteRendition== "Y")
Dec2011_complete_renditions <- subset(Dec2011_song, Dec2011_song$CompleteRendition== "Y")

#### 2. Run random forest ####

# 2.1 Run random forest within each year

# Assign columns for random forest 
RF_cols <- c("Dur90__s_", "BW90__Hz_", "Freq5__Hz_", "Freq95__Hz_", 
              "PeakFreq_Hz_", "StartFreqHz", "EndFreqHz", "Inflections") # uses the same numerical columns as PCA

# Subset data as to include only assigned columns and QualName 
Dec2014_RFdata <- Dec2014_song[, c(RF_cols, "QualName")]
Dec2011_RFdata <- Dec2011_song[, c(RF_cols, "QualName")]
Dec2020_RFdata <- Dec2020_song[, c(RF_cols, "QualName")]

# Split predictors (RF cols) and responses (QualName)
Dec2014_RFdata_pred <- Dec2014_RFdata[, -ncol(Dec2014_RFdata)]
Dec2014_RFdata_resp <- droplevels(as.factor(Dec2014_RFdata$QualName))
Dec2011_RFdata_pred <- Dec2011_RFdata[, -ncol(Dec2011_RFdata)]
Dec2011_RFdata_resp <- droplevels(as.factor(Dec2011_RFdata$QualName))
Dec2020_RFdata_pred <- Dec2020_RFdata[, -ncol(Dec2020_RFdata)]
Dec2020_RFdata_resp <- droplevels(as.factor(Dec2020_RFdata$QualName))

# Perform random forest on units per year and get initial OOB error rates
library(randomForest)

Dec2014_rf_model <- randomForest(Dec2014_RFdata_pred, Dec2014_RFdata_resp, ntree = 10000, mtry = 3)
print(Dec2014_rf_model) # OOB = 1.4%
Dec2011_rf_model <- randomForest(Dec2011_RFdata_pred, Dec2011_RFdata_resp, ntree = 10000, mtry = 3)
print(Dec2011_rf_model) # OOB = 1.4%
Dec2020_rf_model <- randomForest(Dec2020_RFdata_pred, Dec2020_RFdata_resp, ntree = 10000, mtry = 3)
print(Dec2020_rf_model) # OOB = 3.9%

# Cross validate OOB error rates by training RF on 75% of data and test on 25% of data 
library(randomForest)

# Put datasets into a list
rf_datasets <- list(
  "Dec2011" = Dec2011_RFdata,
  "Dec2014" = Dec2014_RFdata,
  "Dec2020" = Dec2020_RFdata
)

# Function to split, train, test, and return results. 
# This may struggle to run depending on how the data is split. If there is a class which is not found in BOTH the train and test subsey, this will not run.
run_rf <- function(data, ntree = 10000, mtry = 3, split = 0.75) {
  data$QualName <- as.factor(data$QualName) # Make sure response is factor
  
  # Train/test split
  n <- nrow(data)
  train_idx <- sample(seq_len(n), size = floor(split * n))
  train <- data[train_idx, ]
  test  <- data[-train_idx, ]
  
  # Fit RF
  model <- randomForest(
    QualName ~ .,
    data = train,
    ntree = ntree,
    mtry = mtry
  )
  
  # Predict on test set
  pred <- predict(model, newdata = test)
  
  # Confusion matrix + accuracy
  cm <- table(Predicted = pred, Actual = test$QualName)
  acc <- sum(diag(cm)) / sum(cm)
  
  list(
    model = model,
    confusion = cm,
    accuracy = acc,
    n_train = nrow(train),
    n_test = nrow(test)
  )
}

# Apply run_rf function to all years
rf_results <- lapply(rf_datasets, run_rf)

# Show accuracy of classification on 25% of data after training
rf_results[["Dec2014"]]$accuracy # 3.8% error rate
rf_results[["Dec2011"]]$accuracy # 0.4% error rate
rf_results[["Dec2020"]]$accuracy # 3.5% error rate


# 2.2 Run random forest across all years 

# Assign columns and subset data for random forest (same as before)
RF_cols <- c("Dur90__s_", "BW90__Hz_", "Freq5__Hz_", "Freq95__Hz_", 
              "PeakFreq_Hz_", "StartFreqHz", "EndFreqHz", "Inflections") # uses the same numerical columns as PCA
AllYear_RFdata <- AllSongUnits[, c(RF_cols, "QualName")]

# Split predictors (RF_cols) and responses (QualName)
AllYear_RFdata_pred <- AllYear_RFdata[, -ncol(AllYear_RFdata)]
AllYear_RFdata_resp <- droplevels(as.factor(AllYear_RFdata$QualName))

# Perform random forest
library(randomForest)
AllYear_rf_model <- randomForest(AllYear_RFdata_pred, AllYear_RFdata_resp, ntree = 10000, mtry = 3)
print(AllYear_rf_model) # OOB = 3.96%


# Split data for training classifier and testing classifier (75% train, 25% test)
AllYear_RFdata$QualName <- as.factor(AllYear_RFdata$QualName) # makes sure response is a factor
n <- nrow(AllYear_RFdata)
train_idx <- sample(seq_len(n), size = floor(0.75 * n))

train_all <- AllYear_RFdata[train_idx, ]
test_all  <- AllYear_RFdata[-train_idx, ]

# Fit random forest on training set
AllYear_rf_model <- randomForest(
  QualName ~ ., 
  data = train_all,
  ntree = 10000,
  mtry = 3
)

# Predict on held-out test set
pred_all <- predict(AllYear_rf_model, newdata = test_all)

# Random forest prediction accuracy for all units
confusion_all <- table(Predicted = pred_all, Actual = test_all$QualName)
accuracy_all <- sum(diag(confusion_all)) / sum(confusion_all)
cat(round(accuracy_all * 100, 2), "%\n") # 96% accuracy = 4% error rate 


#### 3. PCA on song units across all years ####

# 3.1 Data Preparation 

library(dplyr)

# NOTE: This code assumes the existence of 'datasets', 'pca_cols',
# 'all_qualnames', 'color_map', and 'dataset_shapes' objects in your environment.

# Create a list of datasets
datasets <- list(
  "Dec2011" = Dec2011_song,
  "Dec2014" = subset(Dec2014_song, Dec2014_song$QualName!= "J7"),
  "Dec2020" = Dec2020_song
)

# Create vector of all unique QualNames, sorted alphabetically
all_qualnames <- c("A", "B", "C", "D", "E1", "E2", "E3", "E4",
                   "F", "G", "H", "I", "J1", "J2", "J3", "J4", "J5", "J6", 
                   "K", "L1", "L2", "M1", "M2", "N", "O")

# Combine all datasets
combined_data <- bind_rows(lapply(names(datasets), function(label) {
  df <- datasets[[label]]
  df$Dataset <- label # add column for origin dataset of each row
  df$QualName <- factor(df$QualName, levels = all_qualnames)
  return(df)
}))

# 2.2 Run PCA on all song units across all years

# Set columns (variables) for PCA
pca_cols <- c("Dur90__s_", "BW90__Hz_", "Freq5__Hz_", "Freq95__Hz_", 
              "PeakFreq_Hz_", "StartFreqHz", "EndFreqHz", "Inflections") 

# Run PCA on all data at once and extract PC scores (while extracting relevant metadata)
pca_combined <- prcomp(combined_data[, pca_cols], scale. = TRUE)
pca_scores_combined <- as.data.frame(pca_combined$x)
pca_scores_combined$QualName <- combined_data$QualName
pca_scores_combined$Dataset <- combined_data$Dataset
pca_scores_combined$DateTime <- combined_data$Source

# 3.3 Visualise PCA plots

# 3.3.1 Create a colour map so that each QualName has a distinct colour within each year

library(RColorBrewer)

# Specify which QualNames appear in which year 
qualnames_2011 <- c("E1", "E2", "E3", "A", "B", "C", "D", "E4")
qualnames_2014 <- c("G", "F", "I", "H", "J1", "J3", "J2", "J5", "J4", "J6")
qualnames_2020 <- c("M2", "L2", "N", "M1", "K", "L1", "O")

# Use ColorBrewer Paired palette (12 colors), repeat for 26 QualNames
base_palette <- brewer.pal(12, "Paired")
color_map <- c(
  setNames(base_palette[1:length(qualnames_2011)], qualnames_2011),
  setNames(base_palette[1:length(qualnames_2014)], qualnames_2014),
  setNames(base_palette[1:length(qualnames_2020)], qualnames_2020)
)

pca_scores_combined$QualName <- factor(pca_scores_combined$QualName, levels = names(color_map))

# Create order for QualName
ordered_qual_dataset_levels <- c()

for (qn in all_qualnames) {
  for (ds in names(datasets)) {
    combo <- paste0(qn, "_", ds)
    if (combo %in% with(pca_scores_combined, paste0(QualName, "_", Dataset))) {
      ordered_qual_dataset_levels <- c(ordered_qual_dataset_levels, combo)
    }
  }
}

# Add new column to pca scores identifying the QualName and Dataset
pca_scores_combined$Qual_Dataset <- factor(
  paste0(pca_scores_combined$QualName, "_", pca_scores_combined$Dataset),
  levels = ordered_qual_dataset_levels
)

# Map colour and shape combos
combo_levels <- levels(pca_scores_combined$Qual_Dataset)

# Apply colour values to each point
color_values <- setNames(
  color_map[sapply(strsplit(combo_levels, "_"), `[`, 1)],
  combo_levels
)

# Extract QualName for legend labels
legend_labels <- sapply(strsplit(combo_levels, "_"), `[`, 1)

# 3.3.2 Run plotting for PCA of units per year

library(ggplot2)
# Define the years to loop over
target_years <- c("Dec2011", "Dec2014", "Dec2020")

# Create linetype map (2011=solid, 2014=dashed, 2020=dotdash)
linetype_map <- setNames(
  c(1, 2, 6),
  target_years
)

# Plotting function 
create_conditional_pca_plot <- function(target_year, data, color_values, combo_levels, linetype_map, label_letter) {
  
  # Prepare conditional colors, legend labels, and linewidths
  conditional_color_values <- color_values
  grey_color <- "grey90"
  levels_to_grey <- combo_levels[!grepl(target_year, combo_levels)]
  conditional_color_values[levels_to_grey] <- grey_color
  
  # Filter labels: only keep the QualName labels for the target year for the legend
  legend_combo_levels <- combo_levels[grepl(target_year, combo_levels)]
  legend_labels_subset <- sapply(strsplit(legend_combo_levels, "_"), `[`, 1)
  
  # Build the plot with 95% confidence ellipses for each unit type
  p <- ggplot(data, aes(x = PC1, y = PC2)) +
    
    # Ellipses: non-target Years (greyed-out, thin line)
    stat_ellipse(
      data = subset(data, Dataset != target_year),
      aes(color = Qual_Dataset, linetype = Dataset),
      geom = "polygon",
      fill = NA,
      level = 0.95,
      type = "t",
      linewidth = 0.6
    ) +
    
    # Ellipses: target year (coloured, thick line)
    stat_ellipse(
      data = subset(data, Dataset == target_year),
      aes(color = Qual_Dataset, linetype = Dataset),
      geom = "polygon",
      fill = NA,
      level = 0.95,
      type = "t",
      linewidth = 0.8
    ) +
    
    # Color scale: use conditional color map, only show legend items for the target year
    scale_color_manual(
      name = NULL,
      values = conditional_color_values,
      labels = setNames(legend_labels_subset, legend_combo_levels),
      breaks = legend_combo_levels
    ) +
    
    # Linetype scale: keeping data from each year in a given linetype
    scale_linetype_manual(
      name = "Dataset Source",
      values = linetype_map
    ) +
    
    # Label for panel
    annotate("text", x = -Inf, y = Inf, label = label_letter, 
             hjust = -1, vjust = 1.5, size = 4, fontface = "bold") +
    
    # Axes and Theme
    scale_x_continuous(limits = c(-6, 3), breaks = seq(-6, 2, by = 2)) +
    scale_y_continuous(limits = c(-10.5, 2.5), breaks = seq(-10, 2, by = 2)) +
    labs(x = "PC1", y = "PC2") + 
    theme_classic() +
    theme(
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.position = "right",
      legend.key.height = unit(12, "pt"),
      legend.key.width = unit(12, "pt"),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    ) +
    # Guides: hide the linetype legend (unnecessary here)
    guides(
      color = guide_legend(order = 1, ncol = 1, byrow = TRUE),
      linetype = "none" # Remove the 'Dataset Source' linetype legend
    )
  
  return(p)
}

# Run plotting function for each year 
plot_2011 <- create_conditional_pca_plot("Dec2011", pca_scores_combined, color_values, combo_levels, linetype_map, "A")
plot_2014 <- create_conditional_pca_plot("Dec2014", pca_scores_combined, color_values, combo_levels, linetype_map, "B")
plot_2020 <- create_conditional_pca_plot("Dec2020", pca_scores_combined, color_values, combo_levels, linetype_map, "C")

# Combine Plots and add panel labels

library(gridExtra)

# 2. Create the 2x2 sub-grid
lower_2x2_grid <- grid.arrange(
  plot_2011,   # B (Top Left)
  plot_2014,   # C (Top Right)
  plot_2020,   # D (Bottom Left)
  ncol = 2,
  heights = c(1, 1),
  widths = c(1, 1)
)

# Export the final combined grid
library(grid)
png("Ellipse_PCA_PerYear.png", width = 15.91, height = 14, units = "cm", res = 300)
grid.draw(lower_2x2_grid)
dev.off()






#### 4. Cost matrices for levenshtein ####

# Columns to include in cost matrix (same as PCA and RF)
pca_cols <- c("Dur90__s_", "BW90__Hz_", "Freq5__Hz_", "Freq95__Hz_", 
              "PeakFreq_Hz_", "StartFreqHz", "EndFreqHz", "Inflections")

# Prepare raw data with one row per individual unit
Dec2011_cost_data <- Dec2011_song %>% select(Sound = QualName, all_of(pca_cols))
Dec2014_cost_data <- Dec2014_song %>% select(Sound = QualName, all_of(pca_cols))
Dec2020_cost_data <- Dec2020_song %>% select(Sound = QualName, all_of(pca_cols))
AllUnits_cost_data <- rbind(Dec2011_cost_data,Dec2014_cost_data,Dec2020_cost_data)

# Write to text file
write.table(Dec2011_cost_data, file = "Dec2011_CM.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(Dec2014_cost_data, file = "Dec2014_CM.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(Dec2020_cost_data, file = "Dec2020_CM.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(AllUnits_cost_data, file = "AllUnits_CM.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Load into workspace using leven and convert to cost matrix format
library(leven)
Dec2011_CM <- cost_matrix_from_file("Dec2011_CM.txt")
Dec2014_CM <- cost_matrix_from_file("Dec2014_CM.txt")
Dec2020_CM <- cost_matrix_from_file("Dec2020_CM.txt")
AllUnits_CM <- cost_matrix_from_file("AllUnits_CM.txt")




#### 5. LSI for ALL song renditions ####

# 5.1 Run compare_songs to get LSIs, check initial clustering and save matrix
library(leven)
lsi_renditions_all <- compare_songs("lsi_rendition_all.csv", cost_matrix = NULL) # produce LSI matrix comparing unit strings per rendition
# lsi_rendition_all.csv needs to be formatted in a specific way to run
# needs to be one column with no header row
# mine follows format: year, source, SongCycleID, SongCycleID, Unit1, Unit2, Unit3, etc.
cluster(lsi_renditions_all$theme_matrix, "average")
lsi_renditions_all$lsi_matrix
write.csv(lsi_renditions_all$theme_matrix, "lsi_rendition_matrix_all.csv", row.names = TRUE)

# 5.2 Create bootstrapped dendrogram using LSI matrix

library(dendextend)
library(pvclust)

# Create dendrogram object and bootstrap with the theme matrix (here filled with renditions), average not a point estimate
lsi_all_rendition_wholestrings<-bootstrap(lsi_renditions_all$lsi_matrix, "average")
# Plot just dendrogram 
dend_lsi_all_rendition<-as.dendrogram(lsi_all_rendition_wholestrings$hclust, hang = -1) 

# CCC for all rendition dendrogram  

ccc_lsi_renditions_all <- as.dist(1-lsi_renditions_all$lsi_matrix) #create dissimilarity matrix
lsi_renditions_hc_all <- hclust(ccc_lsi_renditions_all, method="average") #vlsuter
ccc_lsi_renditions_value <- cophenetic(lsi_renditions_hc_all) #calculate cophenetic
cor(ccc_lsi_renditions_all, ccc_lsi_renditions_value) #get ccc score = 0.9654347

# Cut dendrogram into clusters and color them, these clusters match the year of song
dend_coloured <- color_branches(
  dend = dend_lsi_all_rendition,
  k = 3,
  col = c("#C39ACF", "#fd8d3c", "#6BAED6")
)

# Manually color the root 
attr(dend_coloured, "edgePar") <- list(col = "black", lwd = 1)

# Then manually colour the first two branches of the root (top-level split)
attr(dend_coloured[[1]], "edgePar") <- list(col = "black", lwd = 1)
attr(dend_coloured[[2]], "edgePar") <- list(col = "black", lwd = 1)

# Then edit branches stemming from branch 2: 
attr(dend_coloured[[2]][[1]], "edgePar") <- list(col = "black")
attr(dend_coloured[[2]][[2]], "edgePar") <- list(col = "black")

# to add the AU and BP values to the plot, first need to get coordinates of where each split occurs
n_leaves <- length(labels(dend_coloured))

get_internal_nodes_coords <- function(dend, x_start = 1) {
  if (is.leaf(dend)) {
    attr(dend, "x") <- x_start
    return(list(dend = dend, x_next = x_start + 1, nodes = list()))
  }
  
  # Left branch
  left_res <- get_internal_nodes_coords(dend[[1]], x_start)
  # Right branch, start x after left subtree
  right_res <- get_internal_nodes_coords(dend[[2]], left_res$x_next)
  
  # Internal node x = midpoint of branches x
  x_node <- (attr(left_res$dend, "x") + attr(right_res$dend, "x")) / 2
  attr(dend, "x") <- x_node
  
  # This node info: x, height
  node_info <- list(x = x_node, y = attr(dend, "height"))
  
  # Collect nodes from branches plus this node (postorder)
  nodes <- c(left_res$nodes, right_res$nodes, list(node_info))
  
  # Return updated dend and next available x coordinate
  return(list(dend = dend, x_next = right_res$x_next, nodes = nodes))
}

au_vals <- lsi_all_rendition_wholestrings$edges$au
bp_vals <- lsi_all_rendition_wholestrings$edges$bp
res <- get_internal_nodes_coords(dend_coloured)
dend_coloured <- res$dend
internal_nodes <- res$nodes  # list of nodes in merge order


# Create png of the dendrogram WITH bootstrap vals 

png("All_rendition_dendrogram_PVals.png", width = 24.6, height = 6.0, units = "cm", res = 600)
par(mar = c(0, 4, 0, 0), cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 1, 0))
plot(dend_coloured, ylab = "Height", axes=F, ylim = c(0,8))
axis(side=2,seq(0,7,1))
for (i in seq_along(internal_nodes)) {
    node <- internal_nodes[[i]]
    x <- node$x
    y <- node$y
    
    if (y < 3) next  # Skip nodes below height 3
    
    au <- au_vals[i]
    bp <- bp_vals[i]
    
    offset.x <- 3.0
    offset.y <- 0.3
    
    if (!is.na(au) && au >= 0.95) {
      points(x - offset.x, y + offset.y, pch = 19, cex = 1.1)
    }
    if (!is.na(bp) && bp >= 0.7) {
      points(x + offset.x, y + offset.y, pch = 4, cex = 1.1)
    }
  }
dev.off()


#### 6 DSI for ALL song renditions ####

# 6.1 run compare_songs to get DSIs, check initial clustering and save matrix
source('dice.R')
dsi_renditions_all <- dice("lsi_rendition_all_multicolumn.csv", format="lsi_strings") # produce LSI matrix comparing unit strings per rendition
# multicolumn is same as lsi_rendition_all.csv, but uses multiple columns
#see Dice_SI calculated matrix
dsi_renditions_all
#clustering 
cluster(dsi_renditions_all, "average")

# 6.2 create bootstrapped dendrogram using DSI matrix

library(dendextend)
library(pvclust)

# create dendrogram object and bootstrap with the theme matrix (here filled with renditions), average not a point estimate
dsi_all_rendition_bootstrapped<-bootstrap(dsi_renditions_all, "average")
# plot just dendrogram 
dend_dsi_all_rendition<-as.dendrogram(dsi_all_rendition_bootstrapped$hclust) 

# CCC for all rendition dendrogram  
ccc_dsi_renditions_all <- as.dist(1-dsi_renditions_all) #create dissimilarity matrix
dsi_renditions_hc_all <- hclust(ccc_dsi_renditions_all, method="average") #vlsuter
ccc_dsi_renditions_value <- cophenetic(dsi_renditions_hc_all) #calculate cophenetic
cor(ccc_dsi_renditions_all, ccc_dsi_renditions_value) #get ccc score

dend_coloured <- color_branches(
  dend = dend_dsi_all_rendition,
  h = 6,
  col = c("#C39ACF", "#fd8d3c", "#6BAED6")
)


# Plot with branches colored
par(cex=1.2, font=3, mar = c(1, 4, 1, 1))
plot(dend_coloured, ylab = "Height")


# manually colour branches (as in LSI plot)
attr(dend_coloured[[1]], "edgePar") <- list(col = "black", lwd = 1)
attr(dend_coloured[[2]], "edgePar") <- list(col = "black", lwd = 1)
attr(dend_coloured[[2]][[1]], "edgePar") <- list(col = "black")
attr(dend_coloured[[2]][[2]], "edgePar") <- list(col = "black")

# then add extract coordinates for later AU and BP values
n_leaves <- length(labels(dend_coloured))

get_internal_nodes_coords <- function(dend, x_start = 1) {
  if (is.leaf(dend)) {
    attr(dend, "x") <- x_start
    return(list(dend = dend, x_next = x_start + 1, nodes = list()))
  }
  
  # Left branch
  left_res <- get_internal_nodes_coords(dend[[1]], x_start)
  # Right branch, start x after left subtree
  right_res <- get_internal_nodes_coords(dend[[2]], left_res$x_next)
  
  # Internal node x = midpoint of branches x
  x_node <- (attr(left_res$dend, "x") + attr(right_res$dend, "x")) / 2
  attr(dend, "x") <- x_node
  
  # This node info: x, height
  node_info <- list(x = x_node, y = attr(dend, "height"))
  
  # Collect nodes from branches plus this node (postorder)
  nodes <- c(left_res$nodes, right_res$nodes, list(node_info))
  
  # Return updated dend and next available x coordinate
  return(list(dend = dend, x_next = right_res$x_next, nodes = nodes))
}

au_vals <- dsi_all_rendition_bootstrapped$edges$au
bp_vals <- dsi_all_rendition_bootstrapped$edges$bp
res <- get_internal_nodes_coords(dend_coloured)
dend_coloured <- res$dend
internal_nodes <- res$nodes  # list of nodes in merge order


# 8.4 Create png of the dendrogram WITH bootstrap vals 
png("DSI_All_rendition_dendrogram_PVals.png", width = 24.6, height = 6.0, units = "cm", res = 600)
par(mar = c(0, 4, 0, 0), cex.lab = 0.9, cex.axis = 0.8, mgp = c(2.5, 1, 0))
plot(dend_coloured, ylab = "Height", axes=F,  ylim = c(0,13))
axis(side=2,seq(0,12,2))
for (i in seq_along(internal_nodes)) {
  node <- internal_nodes[[i]]
  x <- node$x
  y <- node$y
  
  if (y < 6) next  # Skip nodes below height 3
  
  au <- au_vals[i]
  bp <- bp_vals[i]
  
  offset.x <- 3.3
  offset.y <- 0.45
  
  if (!is.na(au) && au >= 0.95) {
    points(x - offset.x, y + offset.y, pch = 19, cex = 1.1)
  }
  if (!is.na(bp) && bp >= 0.7) {
    points(x + offset.x, y + offset.y, pch = 4, cex = 1.1)
  }
}
dev.off()















#### 7 Perform Levenshtein and Dice's on Dec2011 song #### 

# 7.1 Levenshtein

library(leven)
lsi_renditions_Dec2011 <- compare_songs("lsi_rendition_2011.csv", cost_matrix = Dec2011_CM) # produce LSI matrix using WEIGHTED LEVENSHTEIN
cluster(lsi_renditions_Dec2011$theme_matrix, "average") # initial insight into clustering

## IMPORTANT NOTE ##
# Here WEIGHTED LSIs are being calculated, meaning that unit substitutions are penalised based on acoustic similarity
# In determining the penalty of a substitution, wLSIs use mean values of eight acoustic properties (previously used during random forest analysis and PCA) 
# from each unit type (n = 25). These values were then standardised using z-scores, after which the normalised Euclidean distance between unit types was 
# calculated to quantify acoustic similarity. Euclidian distance is normalised to the maximum pairwise distance, and therefore 
# falls between 0 and 1, where 1 is the largest distance (or highest dissimilarity) between unit types in n-dimensional space (n = 8 here as there were 
# eight variables; check Garland et al., 2017, Devil in detail for full run through).


# bootstrap theme matrix (here filled with renditions), average not a point estimate
library(dendextend)
library(pvclust)
Dec2011boot_theme_mat_lsi<-bootstrap(lsi_renditions_Dec2011$theme_matrix, "average")
# create dendrogram with bootstrapped theme matrix  
Dec2011_dend_lsi<-as.dendrogram(Dec2011boot_theme_mat_lsi$hclust) 


# 7.2 Dice's

source('dice.R')
Dec2011_rendition_dsi <- dice("lsi_rendition_2011_multicolumn.csv", format="lsi_strings")
cluster(Dec2011_rendition_dsi, "average") # initial insight into clustering

#bootstrap dice analysis (euclidean and 1,000 times; uses theme matrix as with LSI)
library(dendextend)
library(pvclust)
Dec2011boot_theme_mat_dsi<- bootstrap(Dec2011_rendition_dsi, "average")
# create dendrogram with bootstrapped theme matrix  
Dec2011_dend_dsi<-as.dendrogram(Dec2011boot_theme_mat_dsi$hclust)

# 7.3 code to produce dendrograms of Dec 2011 song coloured by recording (for export)

Dec2011_rendition_df <- read.csv("lsi_rendition_2011_multicolumn_wh.csv")

# change format of source labels for figure legend
library(tidyverse)
Dec2011_rendition_df <- Dec2011_rendition_df %>%
  mutate(V2 = recode(V2,
                     "Dec03_2011_1200" = "3 December 12:00",
                     "Dec03_2011_1500" = "3 December 15:00",

  ))

dend_lsi_labels <- labels(Dec2011_dend_lsi)
dend_dice_labels <- labels(Dec2011_dend_dsi)
rendition_to_source <- setNames(as.character(Dec2011_rendition_df$V2), trimws(as.character(Dec2011_rendition_df$V3)))
lsi_label_sources <- rendition_to_source[dend_lsi_labels]
dsi_label_sources <- rendition_to_source[trimws(labels(Dec2011_dend_dsi))]

# Create a dataframe for ordering the sources and assigning each source a colour
source_df <- data.frame(source = unique(lsi_label_sources), stringsAsFactors = FALSE) %>%
  mutate(datetime = as.POSIXct(source, format = "%d %B %H:%M", tz = "UTC"))
# Order chronologically
source_df <- source_df %>% arrange(datetime)
ordered_sources <- source_df$source
# Assign colours based on chronological order
source_colours <- rainbow(length(ordered_sources))
names(source_colours) <- ordered_sources
# Match labels to colours
lsi_label_colors <- source_colours[lsi_label_sources] 
dsi_label_colors <- source_colours[dsi_label_sources]
# Apply new colours to dendrogram
lsi_dend_coloured <- Dec2011_dend_lsi %>% set("labels_col", lsi_label_colors)
dsi_dend_coloured <- Dec2011_dend_dsi %>% set("labels_col", dsi_label_colors)


# Check LSI and DSI dendrograms are correct
plot(lsi_dend_coloured, ylab = "Height")
legend("topright", legend = ordered_sources, fill = source_colours)
plot(dsi_dend_coloured, ylab = "Height")
legend("topright", legend = ordered_sources, fill = source_colours)

# get the number of labels in dendrogram
n_leaves <- length(labels(lsi_dend_coloured))

# get branch coordinates for applying AU and BP P-value indicators later
get_internal_nodes_coords <- function(dend, x_start = 1) {
  if (is.leaf(dend)) {
    attr(dend, "x") <- x_start
    return(list(dend = dend, x_next = x_start + 1, nodes = list()))
  }
  
  # Left branch
  left_res <- get_internal_nodes_coords(dend[[1]], x_start)
  # Right branch, start x after left subtree
  right_res <- get_internal_nodes_coords(dend[[2]], left_res$x_next)
  
  # Internal node x = midpoint of branches x
  x_node <- (attr(left_res$dend, "x") + attr(right_res$dend, "x")) / 2
  attr(dend, "x") <- x_node
  
  # This node info: x, height
  node_info <- list(x = x_node, y = attr(dend, "height"))
  
  # Collect nodes from branches plus this node (postorder)
  nodes <- c(left_res$nodes, right_res$nodes, list(node_info))
  
  # Return updated dend and next available x coordinate
  return(list(dend = dend, x_next = right_res$x_next, nodes = nodes))
}

lsi_au_vals <- Dec2011boot_theme_mat_lsi$edges$au
lsi_bp_vals <- Dec2011boot_theme_mat_lsi$edges$bp
lsi_res <- get_internal_nodes_coords(lsi_dend_coloured)
lsi_dend_coloured <- lsi_res$dend
lsi_internal_nodes <- lsi_res$nodes  # list of nodes in merge order

dsi_au_vals <- Dec2011boot_theme_mat_lsi$edges$au
dsi_bp_vals <- Dec2011boot_theme_mat_lsi$edges$bp
dsi_res <- get_internal_nodes_coords(dsi_dend_coloured)
dsi_dend_coloured <- dsi_res$dend
dsi_internal_nodes <- dsi_res$nodes  # list of nodes in merge order

# 7.4 Export pngs of LSI and DSI dendrograms 

png("Dec2011_LSI_dendrogram.png", width = 10.8, height = 21.6, units = "cm", res = 600)
par(cex = 0.9, cex.lab = 1, cex.axis = 0.9, font = 1, mar = c(1, 4, 0, 0))
plot(lsi_dend_coloured, ylab = "Height" , ylim = c(0,2))
axis(side=2,seq(0,2,0.5))
for (i in seq_along(lsi_internal_nodes)) {
  node <- lsi_internal_nodes[[i]]
  x <- node$x
  y <- node$y
  
  au <- lsi_au_vals[i]
  bp <- lsi_bp_vals[i]
  
  offset.x <- 0.25  # can tweak these values depending on output
  offset.y <- 0  
  
  for (i in seq_along(lsi_internal_nodes)) {
    node <- lsi_internal_nodes[[i]]
    x <- node$x
    y <- node$y
    
    au <- lsi_au_vals[i]
    bp <- lsi_bp_vals[i]
    
    if (!is.na(au) && au >= 0.95) {
      points(x - offset.x, y + offset.y, pch = 19, cex = 1.2)
    }
    if (!is.na(bp) && bp >= 0.7) {
      points(x + offset.x, y + offset.y, pch = 4, cex = 1.2)
    }
  }
  
}
legend("topright", legend = ordered_sources, fill = source_colours, box.lty = 0, cex = 1.4)
dev.off()

png("Dec2011_DSI_dendrogram.png", width = 10.8, height = 21.6, units = "cm", res = 600)
par(cex = 0.9, cex.lab = 1, cex.axis = 0.9, font = 1, mar = c(1, 4, 0, 0))
plot(dsi_dend_coloured, ylab = "Height", ylim = c(0, 1))
for (i in seq_along(dsi_internal_nodes)) {
  node <- dsi_internal_nodes[[i]]
  x <- node$x
  y <- node$y
  
  au <- dsi_au_vals[i]
  bp <- dsi_bp_vals[i]
  
  offset.x <- 0.25  # tweak this value as you like
  offset.y <- 0  # tweak this value as you like
  
  for (i in seq_along(dsi_internal_nodes)) {
    node <- dsi_internal_nodes[[i]]
    x <- node$x
    y <- node$y
    
    au <- dsi_au_vals[i]
    bp <- dsi_bp_vals[i]
    
    if (!is.na(au) && au >= 0.95) {
      points(x - offset.x, y + offset.y, pch = 19, cex = 1)
    }
    if (!is.na(bp) && bp >= 0.7) {
      points(x + offset.x, y + offset.y, pch = 4, cex = 1)
    }
  }
  
}
legend("topright", legend = ordered_sources, fill = source_colours, box.lty = 0, cex = 1.4)
dev.off()

# 7.5 CCC for Dec 2011 dendrograms 
ccc_lsi_renditions_2011 <- as.dist(1-lsi_renditions_Dec2011$lsi_matrix) #create dissimilarity matrix
lsi_renditions_hc_2011 <- hclust(ccc_lsi_renditions_2011, method="average") #vlsuter
ccc_lsi_renditions_value <- cophenetic(lsi_renditions_hc_2011) #calculate cophenetic
cor(ccc_lsi_renditions_2011, ccc_lsi_renditions_value) #get ccc score

ccc_dsi_renditions_2011 <- as.dist(1-Dec2011_rendition_dsi) #create dissimilarity matrix
dsi_renditions_hc_2011 <- hclust(ccc_dsi_renditions_2011, method="average") #vlsuter
ccc_dsi_renditions_value <- cophenetic(dsi_renditions_hc_2011) #calculate cophenetic
cor(ccc_dsi_renditions_2011, ccc_dsi_renditions_value) #get ccc score





#### 8 Perform Levenshtein and Dice's on Dec2014 song #### 

# 8.1 Levenshtein

library(leven)
lsi_renditions_Dec2014 <- compare_songs("lsi_rendition_2014.csv", cost_matrix = Dec2014_CM) # produce LSI matrix using WEIGHTED LEVENSHTEIN
cluster(lsi_renditions_Dec2014$theme_matrix, "average") # initial insight into clustering

# bootstrap theme matrix (here filled with renditions), average not a point estimate
library(dendextend)
library(pvclust)
Dec2014boot_theme_mat_lsi<-bootstrap(lsi_renditions_Dec2014$theme_matrix, "average")
# create dendrogram with bootstrapped theme matrix  
Dec2014_dend_lsi<-as.dendrogram(Dec2014boot_theme_mat_lsi$hclust) 


# 8.2 Dice's

source('dice.R')
Dec2014_rendition_dsi <- dice("lsi_rendition_2014_multicolumn.csv", format="lsi_strings")
cluster(Dec2014_rendition_dsi, "average") # initial insight into clustering

#bootstrap dice analysis (euclidean and 1,000 times; uses theme matrix as with LSI)
library(dendextend)
library(pvclust)
Dec2014boot_theme_mat_dsi<- bootstrap(Dec2014_rendition_dsi, "average")
# create dendrogram with bootstrapped theme matrix  
Dec2014_dend_dsi<-as.dendrogram(Dec2014boot_theme_mat_dsi$hclust)

# 8.3 code to produce dendrograms of Dec 2014 song coloured by recording (for export)

Dec2014_rendition_df <- read.csv("lsi_rendition_2014_multicolumn_wh.csv")

# change format of source labels for figure legend
Dec2014_rendition_df <- Dec2014_rendition_df %>%
  mutate(V2 = recode(V2,
                     "Dec03_2014_2000" = "3 December 20:00",
                     "Dec06_2014_1400" = "6 December 14:00",
                     "Dec06_2014_1500" = "6 December 15:00",
                     "Dec06_2014_1600" = "6 December 16:00",
                     "Dec16_2014_1700" = "16 December 17:00",
                     "Dec16_2014_1800" = "16 December 18:00",
                     "Dec26_2014_1000" = "26 December 10:00",
                     "Dec26_2014_1100" = "26 December 11:00",
                     "Dec26_2014_1200" = "26 December 12:00",
                     "Dec26_2014_1300" = "26 December 13:00",
                     "Dec26_2014_1400" = "26 December 14:00",
                     "Dec29_2014_1600" = "29 December 16:00",
  ))

dend_lsi_labels <- labels(Dec2014_dend_lsi)
dend_dice_labels <- labels(Dec2014_dend_dsi)
rendition_to_source <- setNames(as.character(Dec2014_rendition_df$V2), trimws(as.character(Dec2014_rendition_df$V3)))
lsi_label_sources <- rendition_to_source[dend_lsi_labels]
dsi_label_sources <- rendition_to_source[trimws(labels(Dec2014_dend_dsi))]

# Create a dataframe for ordering the sources and assigning each source a colour
source_df <- data.frame(source = unique(lsi_label_sources), stringsAsFactors = FALSE) %>%
  mutate(datetime = as.POSIXct(source, format = "%d %B %H:%M", tz = "UTC"))
# Order chronologically
source_df <- source_df %>% arrange(datetime)
ordered_sources <- source_df$source
# Assign colours based on chronological order
source_colours <- rainbow(length(ordered_sources))
names(source_colours) <- ordered_sources
# Match labels to colours
lsi_label_colors <- source_colours[lsi_label_sources] 
dsi_label_colors <- source_colours[dsi_label_sources]
# Apply new colours to dendrogram
lsi_dend_coloured <- Dec2014_dend_lsi %>% set("labels_col", lsi_label_colors)
dsi_dend_coloured <- Dec2014_dend_dsi %>% set("labels_col", dsi_label_colors)


# Check LSI and DSI dendrograms
plot(lsi_dend_coloured, ylab = "Height")
legend("topright", legend = ordered_sources, fill = source_colours)
plot(dsi_dend_coloured, ylab = "Height")
legend("topright", legend = ordered_sources, fill = source_colours)

# get the number of labels in dendrogram
n_leaves <- length(labels(lsi_dend_coloured))

# get branch coordinates for applying AU and BP P-value indicators later
get_internal_nodes_coords <- function(dend, x_start = 1) {
  if (is.leaf(dend)) {
    attr(dend, "x") <- x_start
    return(list(dend = dend, x_next = x_start + 1, nodes = list()))
  }
  
  # Left branch
  left_res <- get_internal_nodes_coords(dend[[1]], x_start)
  # Right branch, start x after left subtree
  right_res <- get_internal_nodes_coords(dend[[2]], left_res$x_next)
  
  # Internal node x = midpoint of branches x
  x_node <- (attr(left_res$dend, "x") + attr(right_res$dend, "x")) / 2
  attr(dend, "x") <- x_node
  
  # This node info: x, height
  node_info <- list(x = x_node, y = attr(dend, "height"))
  
  # Collect nodes from branches plus this node (postorder)
  nodes <- c(left_res$nodes, right_res$nodes, list(node_info))
  
  # Return updated dend and next available x coordinate
  return(list(dend = dend, x_next = right_res$x_next, nodes = nodes))
}

lsi_au_vals <- Dec2014boot_theme_mat_lsi$edges$au
lsi_bp_vals <- Dec2014boot_theme_mat_lsi$edges$bp
lsi_res <- get_internal_nodes_coords(lsi_dend_coloured)
lsi_dend_coloured <- lsi_res$dend
lsi_internal_nodes <- lsi_res$nodes  # list of nodes in merge order

dsi_au_vals <- Dec2014boot_theme_mat_lsi$edges$au
dsi_bp_vals <- Dec2014boot_theme_mat_lsi$edges$bp
dsi_res <- get_internal_nodes_coords(dsi_dend_coloured)
dsi_dend_coloured <- dsi_res$dend
dsi_internal_nodes <- dsi_res$nodes  # list of nodes in merge order

# 8.4 Export pngs of LSI and DSI dendrograms 

png("Dec2014_LSI_dendrogram.png", width = 49.2, height = 24.0, units = "cm", res = 600)
par(cex = 1.15, cex.lab = 1.4, cex.axis = 1.3, font = 1, mar = c(1, 4, 0, 0))
plot(lsi_dend_coloured, ylab = "Height", axes=F, ylim = c(0, 4.2))
axis(side=2,seq(0,4,1))
for (i in seq_along(lsi_internal_nodes)) {
  node <- lsi_internal_nodes[[i]]
  x <- node$x
  y <- node$y
  
  au <- lsi_au_vals[i]
  bp <- lsi_bp_vals[i]
  
  offset.x <- 0.5  # can tweak these values depending on output
  offset.y <- 0  
  
  for (i in seq_along(lsi_internal_nodes)) {
    node <- lsi_internal_nodes[[i]]
    x <- node$x
    y <- node$y
    
    au <- lsi_au_vals[i]
    bp <- lsi_bp_vals[i]
    
    if (!is.na(au) && au >= 0.95) {
      points(x - offset.x, y + offset.y, pch = 19, cex = 1.2)
    }
    if (!is.na(bp) && bp >= 0.7) {
      points(x + offset.x, y + offset.y, pch = 4, cex = 1.2)
    }
  }
  
}
legend("topright", legend = ordered_sources, fill = source_colours, box.lty = 0, cex = 1.4)
dev.off()

png("Dec2014_DSI_dendrogram.png", width = 49.2, height = 24.0, units = "cm", res = 600)
par(cex = 1.15, cex.lab = 1.4, cex.axis = 1.3, font = 1, mar = c(1, 4, 0, 0))
plot(dsi_dend_coloured, ylab = "Height", ylim = c(0, 6))
axis(side=2,seq(0,6,1))
for (i in seq_along(dsi_internal_nodes)) {
  node <- dsi_internal_nodes[[i]]
  x <- node$x
  y <- node$y
  
  au <- dsi_au_vals[i]
  bp <- dsi_bp_vals[i]
  
  offset.x <- 0.5  # tweak this value as you like
  offset.y <- 0  # tweak this value as you like
  
  for (i in seq_along(dsi_internal_nodes)) {
    node <- dsi_internal_nodes[[i]]
    x <- node$x
    y <- node$y
    
    au <- dsi_au_vals[i]
    bp <- dsi_bp_vals[i]
    
    if (!is.na(au) && au >= 0.95) {
      points(x - offset.x, y + offset.y, pch = 19, cex = 1)
    }
    if (!is.na(bp) && bp >= 0.7) {
      points(x + offset.x, y + offset.y, pch = 4, cex = 1)
    }
  }
  
}
legend("topright", legend = ordered_sources, fill = source_colours, box.lty = 0, cex = 1.4)
dev.off()

# 8.5 CCC for Dec 2014 dendrograms 
ccc_lsi_renditions_2014 <- as.dist(1-lsi_renditions_Dec2014$lsi_matrix) #create dissimilarity matrix
lsi_renditions_hc_2014 <- hclust(ccc_lsi_renditions_2014, method="average") #vlsuter
ccc_lsi_renditions_value <- cophenetic(lsi_renditions_hc_2014) #calculate cophenetic
cor(ccc_lsi_renditions_2014, ccc_lsi_renditions_value) #get ccc score

ccc_dsi_renditions_2014 <- as.dist(1-Dec2014_rendition_dsi) #create dissimilarity matrix
dsi_renditions_hc_2014 <- hclust(ccc_dsi_renditions_2014, method="average") #vlsuter
ccc_dsi_renditions_value <- cophenetic(dsi_renditions_hc_2014) #calculate cophenetic
cor(ccc_dsi_renditions_2014, ccc_dsi_renditions_value) #get ccc score


#### 9 Perform Levenshtein and Dice's on Dec2020 song #### 

# 9.1 Levenshtein

library(leven)
lsi_renditions_Dec2020 <- compare_songs("lsi_rendition_2020.csv", cost_matrix = Dec2020_CM) # produce LSI matrix using WEIGHTED LEVENSHTEIN
cluster(lsi_renditions_Dec2020$theme_matrix, "average") # initial insight into clustering

# bootstrap theme matrix (here filled with renditions), average not a point estimate
library(dendextend)
library(pvclust)
Dec2020boot_theme_mat_lsi<-bootstrap(lsi_renditions_Dec2020$theme_matrix, "average")
# create dendrogram with bootstrapped theme matrix  
Dec2020_dend_lsi<-as.dendrogram(Dec2020boot_theme_mat_lsi$hclust) 


# 9.2 Dice's

source('dice.R')
Dec2020_rendition_dsi <- dice("lsi_rendition_2020_multicolumn.csv", format="lsi_strings")
cluster(Dec2020_rendition_dsi, "average") # initial insight into clustering

#bootstrap dice analysis (euclidean and 1,000 times; uses theme matrix as with LSI)
library(dendextend)
library(pvclust)
Dec2020boot_theme_mat_dsi<- bootstrap(Dec2020_rendition_dsi, "average")
# create dendrogram with bootstrapped theme matrix  
Dec2020_dend_dsi<-as.dendrogram(Dec2020boot_theme_mat_dsi$hclust)

# 9.3 code to produce dendrograms of Dec 2020 song coloured by recording (for export)

Dec2020_rendition_df <- read.csv("lsi_rendition_2020_multicolumn_wh.csv")

# change format of source labels for figure legend
# change format of source labels for plotting
Dec2020_rendition_df <- Dec2020_rendition_df %>%
  mutate(V2 = recode(V2,
                     "Dec08_2020_1500" = "8 December 15:00",
                     "Dec11_2020_0400" = "11 December 04:00",
                     "Dec26_2020_1200" = "26 December 12:00",
                     "Dec26_2020_1300" = "26 December 13:00",
                     "Dec26_2020_1400" = "26 December 14:00",
                     "Dec26_2020_1500" = "26 December 15:00",
                     "Dec26_2020_1600" = "26 December 16:00",
                     "Dec29_2020_0400" = "29 December 04:00",
  ))

dend_lsi_labels <- labels(Dec2020_dend_lsi)
dend_dice_labels <- labels(Dec2020_dend_dsi)
rendition_to_source <- setNames(as.character(Dec2020_rendition_df$V2), trimws(as.character(Dec2020_rendition_df$V3)))
lsi_label_sources <- rendition_to_source[dend_lsi_labels]
dsi_label_sources <- rendition_to_source[trimws(labels(Dec2020_dend_dsi))]

# Create a dataframe for ordering the sources and assigning each source a colour
source_df <- data.frame(source = unique(lsi_label_sources), stringsAsFactors = FALSE) %>%
  mutate(datetime = as.POSIXct(source, format = "%d %B %H:%M", tz = "UTC"))
# Order chronologically
source_df <- source_df %>% arrange(datetime)
ordered_sources <- source_df$source
# Assign colours based on chronological order
source_colours <- rainbow(length(ordered_sources))
names(source_colours) <- ordered_sources
# Match labels to colours
lsi_label_colors <- source_colours[lsi_label_sources] 
dsi_label_colors <- source_colours[dsi_label_sources]

# Apply new colours to dendrogram
lsi_dend_coloured <- Dec2020_dend_lsi %>% set("labels_col", lsi_label_colors)
dsi_dend_coloured <- Dec2020_dend_dsi %>% set("labels_col", dsi_label_colors)

# Check LSI and DSI dendrograms
plot(lsi_dend_coloured, ylab = "Height")
legend("topright", legend = ordered_sources, fill = source_colours)
plot(dsi_dend_coloured, ylab = "Height")
legend("topright", legend = ordered_sources, fill = source_colours)

# get the number of labels in dendrogram
n_leaves <- length(labels(lsi_dend_coloured))

# get branch coordinates for applying AU and BP P-value indicators later
get_internal_nodes_coords <- function(dend, x_start = 1) {
  if (is.leaf(dend)) {
    attr(dend, "x") <- x_start
    return(list(dend = dend, x_next = x_start + 1, nodes = list()))
  }
  
  # Left branch
  left_res <- get_internal_nodes_coords(dend[[1]], x_start)
  # Right branch, start x after left subtree
  right_res <- get_internal_nodes_coords(dend[[2]], left_res$x_next)
  
  # Internal node x = midpoint of branches x
  x_node <- (attr(left_res$dend, "x") + attr(right_res$dend, "x")) / 2
  attr(dend, "x") <- x_node
  
  # This node info: x, height
  node_info <- list(x = x_node, y = attr(dend, "height"))
  
  # Collect nodes from branches plus this node (postorder)
  nodes <- c(left_res$nodes, right_res$nodes, list(node_info))
  
  # Return updated dend and next available x coordinate
  return(list(dend = dend, x_next = right_res$x_next, nodes = nodes))
}

lsi_au_vals <- Dec2020boot_theme_mat_lsi$edges$au
lsi_bp_vals <- Dec2020boot_theme_mat_lsi$edges$bp
lsi_res <- get_internal_nodes_coords(lsi_dend_coloured)
lsi_dend_coloured <- lsi_res$dend
lsi_internal_nodes <- lsi_res$nodes  # list of nodes in merge order

dsi_au_vals <- Dec2020boot_theme_mat_lsi$edges$au
dsi_bp_vals <- Dec2020boot_theme_mat_lsi$edges$bp
dsi_res <- get_internal_nodes_coords(dsi_dend_coloured)
dsi_dend_coloured <- dsi_res$dend
dsi_internal_nodes <- dsi_res$nodes  # list of nodes in merge order

# 9.4 Export pngs of LSI and DSI dendrograms 

png("Dec2020_LSI_dendrogram.png", width = 49.2, height = 24.0, units = "cm", res = 600)
par(cex = 1.15, cex.lab = 1.4, cex.axis = 1.3, font = 1, mar = c(1, 4, 0, 0))
plot(lsi_dend_coloured, ylab = "Height", ylim = c(0, 2))
for (i in seq_along(lsi_internal_nodes)) {
  node <- lsi_internal_nodes[[i]]
  x <- node$x
  y <- node$y
  
  au <- lsi_au_vals[i]
  bp <- lsi_bp_vals[i]
  
  offset.x <- 0.5  # can tweak these values depending on output
  offset.y <- 0  
  
  for (i in seq_along(lsi_internal_nodes)) {
    node <- lsi_internal_nodes[[i]]
    x <- node$x
    y <- node$y
    
    au <- lsi_au_vals[i]
    bp <- lsi_bp_vals[i]
    
    if (!is.na(au) && au >= 0.95) {
      points(x - offset.x, y + offset.y, pch = 19, cex = 1.2)
    }
    if (!is.na(bp) && bp >= 0.7) {
      points(x + offset.x, y + offset.y, pch = 4, cex = 1.2)
    }
  }
  
}
legend("topright", legend = ordered_sources, fill = source_colours, box.lty = 0, cex = 1.4)
dev.off()

png("Dec2020_DSI_dendrogram.png", width = 49.2, height = 24.0, units = "cm", res = 600)
par(cex = 1.15, cex.lab = 1.4, cex.axis = 1.3, font = 1, mar = c(1, 4, 0, 0))
plot(dsi_dend_coloured, ylab = "Height", ylim = c(0, 2.2))
for (i in seq_along(dsi_internal_nodes)) {
  node <- dsi_internal_nodes[[i]]
  x <- node$x
  y <- node$y
  
  au <- dsi_au_vals[i]
  bp <- dsi_bp_vals[i]
  
  offset.x <- 0.5  # tweak this value as you like
  offset.y <- 0  # tweak this value as you like
  
  for (i in seq_along(dsi_internal_nodes)) {
    node <- dsi_internal_nodes[[i]]
    x <- node$x
    y <- node$y
    
    au <- dsi_au_vals[i]
    bp <- dsi_bp_vals[i]
    
    if (!is.na(au) && au >= 0.95) {
      points(x - offset.x, y + offset.y, pch = 19, cex = 1)
    }
    if (!is.na(bp) && bp >= 0.7) {
      points(x + offset.x, y + offset.y, pch = 4, cex = 1)
    }
  }
  
}
legend("topright", legend = ordered_sources, fill = source_colours, box.lty = 0, cex = 1.4)
dev.off()

# 9.5 CCC for Dec 2020 dendrograms 
ccc_lsi_renditions_2020 <- as.dist(1-lsi_renditions_Dec2020$lsi_matrix) #create dissimilarity matrix
lsi_renditions_hc_2020 <- hclust(ccc_lsi_renditions_2020, method="average") #vlsuter
ccc_lsi_renditions_value <- cophenetic(lsi_renditions_hc_2020) #calculate cophenetic
cor(ccc_lsi_renditions_2020, ccc_lsi_renditions_value) #get ccc score

ccc_dsi_renditions_2020 <- as.dist(1-Dec2020_rendition_dsi) #create dissimilarity matrix
dsi_renditions_hc_2020 <- hclust(ccc_dsi_renditions_2020, method="average") #vlsuter
ccc_dsi_renditions_value <- cophenetic(dsi_renditions_hc_2020) #calculate cophenetic
cor(ccc_dsi_renditions_2020, ccc_dsi_renditions_value) #get ccc score





#### 10. Perform Levenshtein on Dec2020tv song #### 

# 10.1 Levenshtein

library(leven)
lsi_renditions_Dec2020tv <- compare_songs("lsi_rendition_2020tv.csv", cost_matrix = Dec2020_CM) # produce LSI matrix using WEIGHTED LEVENSHTEIN
cluster(lsi_renditions_Dec2020tv$theme_matrix, "average") # initial insight into clustering

# bootstrap theme matrix (here filled with renditions), average not a point estimate
library(dendextend)
library(pvclust)
Dec2020tvboot_theme_mat_lsi<-bootstrap(lsi_renditions_Dec2020tv$theme_matrix, "average")
# create dendrogram with bootstrapped theme matrix  
Dec2020tv_dend_lsi<-as.dendrogram(Dec2020tvboot_theme_mat_lsi$hclust) 

# 10.2 code to produce dendrograms of Dec 2020tv song coloured by recording (for export)

Dec2020tv_rendition_df <- read.csv("lsi_rendition_2020tv_multicolumn_wh.csv")

# change format of source labels for figure legend
# change format of source labels for plotting
Dec2020tv_rendition_df <- Dec2020tv_rendition_df %>%
  mutate(V2 = recode(V2,
                     "Dec08_2020_1500" = "8 December 15:00",
                     "Dec11_2020_0400" = "11 December 04:00",
                     "Dec26_2020_1200" = "26 December 12:00",
                     "Dec26_2020_1300" = "26 December 13:00",
                     "Dec26_2020_1400" = "26 December 14:00",
                     "Dec26_2020_1500" = "26 December 15:00",
                     "Dec26_2020_1600" = "26 December 16:00",
                     "Dec29_2020_0400" = "29 December 04:00",
  ))

dend_lsi_labels <- labels(Dec2020tv_dend_lsi)
rendition_to_source <- setNames(Dec2020tv_rendition_df$V2, Dec2020tv_rendition_df$V3)  # Named vector: names = rendition IDs
lsi_label_sources <- rendition_to_source[dend_lsi_labels]

# Create a dataframe for ordering the sources and assigning each source a colour
source_df <- data.frame(source = unique(lsi_label_sources), stringsAsFactors = FALSE) %>%
  mutate(datetime = as.POSIXct(source, format = "%d %B %H:%M", tz = "UTC"))
# Order chronologically
source_df <- source_df %>% arrange(datetime)
ordered_sources <- source_df$source
# Assign colours based on chronological order
source_colours <- rainbow(length(ordered_sources))
names(source_colours) <- ordered_sources
# Match labels to colours
label_colors <- source_colours[lsi_label_sources] 
# Apply new colours to dendrogram
lsi_dend_coloured <- Dec2020tv_dend_lsi %>% set("labels_col", label_colors)

# Check LSI and DSI dendrograms
plot(lsi_dend_coloured, ylab = "Height")
legend("topright", legend = ordered_sources, fill = source_colours)

# get the number of labels in dendrogram
n_leaves <- length(labels(lsi_dend_coloured))

# get branch coordinates for applying AU and BP P-value indicators later
get_internal_nodes_coords <- function(dend, x_start = 1) {
  if (is.leaf(dend)) {
    attr(dend, "x") <- x_start
    return(list(dend = dend, x_next = x_start + 1, nodes = list()))
  }
  
  # Left branch
  left_res <- get_internal_nodes_coords(dend[[1]], x_start)
  # Right branch, start x after left subtree
  right_res <- get_internal_nodes_coords(dend[[2]], left_res$x_next)
  
  # Internal node x = midpoint of branches x
  x_node <- (attr(left_res$dend, "x") + attr(right_res$dend, "x")) / 2
  attr(dend, "x") <- x_node
  
  # This node info: x, height
  node_info <- list(x = x_node, y = attr(dend, "height"))
  
  # Collect nodes from branches plus this node (postorder)
  nodes <- c(left_res$nodes, right_res$nodes, list(node_info))
  
  # Return updated dend and next available x coordinate
  return(list(dend = dend, x_next = right_res$x_next, nodes = nodes))
}

lsi_au_vals <- Dec2020tvboot_theme_mat_lsi$edges$au
lsi_bp_vals <- Dec2020tvboot_theme_mat_lsi$edges$bp
lsi_res <- get_internal_nodes_coords(lsi_dend_coloured)
lsi_dend_coloured <- lsi_res$dend
lsi_internal_nodes <- lsi_res$nodes  # list of nodes in merge order

dsi_au_vals <- Dec2020tvboot_theme_mat_lsi$edges$au
dsi_bp_vals <- Dec2020tvboot_theme_mat_lsi$edges$bp
dsi_res <- get_internal_nodes_coords(dsi_dend_coloured)
dsi_dend_coloured <- dsi_res$dend
dsi_internal_nodes <- dsi_res$nodes  # list of nodes in merge order

# 10.3 Export pngs of LSI and DSI dendrograms 

png("Dec2020tv_LSI_dendrogram.png", width = 49.2, height = 24.0, units = "cm", res = 600)
par(cex = 1.15, cex.lab = 1.4, cex.axis = 1.3, font = 1, mar = c(1, 4, 0, 0))
plot(lsi_dend_coloured, ylab = "Height", ylim = c(0, 2))
for (i in seq_along(lsi_internal_nodes)) {
  node <- lsi_internal_nodes[[i]]
  x <- node$x
  y <- node$y
  
  au <- lsi_au_vals[i]
  bp <- lsi_bp_vals[i]
  
  offset.x <- 0.5  # can tweak these values depending on output
  offset.y <- 0  
  
  for (i in seq_along(lsi_internal_nodes)) {
    node <- lsi_internal_nodes[[i]]
    x <- node$x
    y <- node$y
    
    au <- lsi_au_vals[i]
    bp <- lsi_bp_vals[i]
    
    if (!is.na(au) && au >= 0.95) {
      points(x - offset.x, y + offset.y, pch = 19, cex = 1.2)
    }
    if (!is.na(bp) && bp >= 0.7) {
      points(x + offset.x, y + offset.y, pch = 4, cex = 1.2)
    }
  }
  
}
legend("topright", legend = ordered_sources, fill = source_colours, box.lty = 0, cex = 1.4)
dev.off()


# 10.4 CCC for Dec 2020tv dendrograms 
ccc_lsi_renditions_2020tv <- as.dist(1-lsi_renditions_Dec2020tv$lsi_matrix) #create dissimilarity matrix
lsi_renditions_hc_2020tv <- hclust(ccc_lsi_renditions_2020tv, method="average") #vlsuter
ccc_lsi_renditions_value <- cophenetic(lsi_renditions_hc_2020tv) #calculate cophenetic
cor(ccc_lsi_renditions_2020tv, ccc_lsi_renditions_value) #get ccc score











#### 11. Comparing LSIs at various scales #### 

# 11.1 Load in csv containing all song renditions

library(tidyverse)

all_rendition_strings <- read.csv("lsi_rendition_all.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(all_rendition_strings) <- "line"

# 11.2 Create dataframe with metadata extracted from csv

rendition_strings_and_metadata <- all_rendition_strings %>%
  mutate(
    year = str_sub(line, 1, 4),
    recording_id = str_split(line, ",\\s*") %>% map_chr(~ .x[2]),
    rendition_id = str_split(line, ",\\s*") %>% map_chr(~ .x[3]),
    day = str_extract(recording_id, "^[A-Za-z]+\\d{2}"),
    day_year = paste0(day, "_", year)
  )


# 11.3 Function for extracting lsi values from each matrix 

extract_lsi_values <- function(file_path, labels, cost_matrix) {
  result <- compare_songs(file_path, cost_matrix = cost_matrix)
  lsi_matrix <- result$lsi_matrix
  
  n <- length(labels)
  indices <- which(lower.tri(lsi_matrix), arr.ind = TRUE)
  
  tibble(
    pair_1 = labels[indices[, 1]],
    pair_2 = labels[indices[, 2]],
    lsi = lsi_matrix[lower.tri(lsi_matrix)]
  )
}

# 11.4 Function to perform levenshtein within specified groups 

run_compare_by_group <- function(data, group_var, scale_label, cost_matrix) {
  if (group_var == "day_year") {
    data <- data %>%
      group_by(day_year) %>%
      ungroup()
  }
  
  data %>%
    group_by(!!sym(group_var)) %>%
    group_split() %>%
    map_df(function(group_df) {
      temp_file <- tempfile(fileext = ".csv")
      labels <- group_df$rendition_id
      write_csv(group_df %>% select(line), temp_file, col_names = FALSE)
      
      extract_lsi_values(temp_file, labels, cost_matrix)
    }) %>%
    mutate(scale = scale_label)
}


# Define years to loop over (based on your data)
years <- unique(rendition_strings_and_metadata$year)

# Initialize an empty tibble
all_lsi_data <- tibble()

# Loop over each year to compute LSI data
for (y in years) {
  year_data <- rendition_strings_and_metadata %>% filter(year == y)
  
  # Dynamically get the cost matrix for this year
  cost_matrix_name <- paste0("Dec", y, "_CM")
  cost_matrix <- get(cost_matrix_name)
  
  recording_lsi <- run_compare_by_group(year_data, "recording_id", "Recording", cost_matrix)
  day_lsi       <- run_compare_by_group(year_data, "day_year", "Day", cost_matrix)
  year_lsi      <- run_compare_by_group(year_data, "year", "Year", cost_matrix)
  
  combined_lsi <- bind_rows(recording_lsi, day_lsi, year_lsi) %>%
    mutate(year = y)
  
  all_lsi_data <- bind_rows(all_lsi_data, combined_lsi)
}


# 11.6 save combined data (so all the levenshtein doesn't need to be reran)

write_csv(all_lsi_data, "all_lsi_data.csv")

# 11.7 Build metadata directly from pair IDs 
# Extract all unique rendition IDs
all_renditions <- unique(c(all_lsi_data$pair_1, all_lsi_data$pair_2))

# Parse IDs into metadata
rendition_strings_and_metadata <- tibble(rendition_id = all_renditions) %>%
  mutate(
    year = str_extract(rendition_id, "_(\\d{4})_") %>% str_remove_all("_"),
    # recording_id = everything up to last underscore
    recording_id = str_replace(rendition_id, "_[^_]+$", ""),
    # day = first part like "Dec03"
    day = str_extract(rendition_id, "^[A-Za-z]+\\d{2}"),
    day_year = paste0(day, "_", year)
  )

# 11.8 Utility functions ahead of Mantel tests

library(vegan)

# Function to produce similarity matrix (LSI) 
make_similarity_matrix <- function(long_df, fill_missing = 0, id_col1 = "pair_1", id_col2 = "pair_2", value_col = "lsi") {
  
  # CHECK FOR MISSING LSI VALUES 
  if (any(is.na(long_df[[value_col]]))) {
    message("NA values detected in the LSI column. Removing these rows before building the matrix.")
    long_df <- long_df %>% filter(!is.na(.data[[value_col]]))
  }
  
  ids <- sort(unique(c(long_df[[id_col1]], long_df[[id_col2]])))
  n <- length(ids)
  mat <- matrix(fill_missing, nrow = n, ncol = n, dimnames = list(ids, ids))
  
  for (i in seq_len(nrow(long_df))) {
    id1 <- long_df[[id_col1]][i]
    id2 <- long_df[[id_col2]][i]
    val <- long_df[[value_col]][i]
    mat[id1, id2] <- val
    mat[id2, id1] <- val
  }
  diag(mat) <- 1
  return(as.dist(mat)) 
}

# Function to create temporal simmilarity matrix
make_temporal_similarity <- function(meta_df, id_col = "rendition_id", time_var = "recording_id") {
  ids <- meta_df[[id_col]]
  n <- length(ids)
  # 1 indicates same group (Similarity)
  mat <- matrix(0, n, n, dimnames = list(ids, ids))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      # Mat[i, j] = 1 if same group
      if (meta_df[[time_var]][i] == meta_df[[time_var]][j]) mat[i, j] <- 1
    }
  }
  diag(mat) <- 1 # Set diagonal to 1 (perfect similarity)
  return(as.dist(mat)) # Changed: Now returns the Temporal SIMILARITY matrix
}

run_mantel <- function(xdist, ydist, method = "pearson", permutations = 9999) {
  if (any(is.na(xdist)) || any(is.na(ydist))) {
    return(tibble(statistic = NA_real_, pvalue = NA_real_, note = "NA present"))
  }
  if (var(as.vector(ydist)) == 0) {
    return(tibble(statistic = NA_real_, pvalue = NA_real_, note = "constant ydist"))
  }
  m <- mantel(xdist, ydist, method = method, permutations = permutations)
  tibble(statistic = as.numeric(m$statistic), pvalue = as.numeric(m$signif), note = "ok")
}

# Function aligns similarity matrix 
align_matrix <- function(xmat, meta_ids) {
  mat_x <- as.matrix(xmat)
  mat_x_sub <- mat_x[meta_ids, meta_ids]
  return(as.dist(mat_x_sub)) # Returns the aligned similarity matrix
}

# 11.9 Run Mantel tests
years <- sort(unique(all_lsi_data$year))
mantel_results_list <- list()

# Function runs mantel within each year
for (y in years) {
  message("Processing year: ", y)
  
  meta_y <- rendition_strings_and_metadata %>% filter(year == as.character(y))
  lsi_y_long <- all_lsi_data %>% filter(year == y) %>% select(pair_1, pair_2, lsi)
  
  if (nrow(lsi_y_long) == 0) next
  
  # Now creates LSI Similarity matrix
  lsi_y_sim <- make_similarity_matrix(lsi_y_long, fill_missing = 0) 
  
  ids_y <- meta_y$rendition_id
  common_ids <- intersect(ids_y, attr(lsi_y_sim, "Labels"))
  if (length(common_ids) < 2) next
  
  # Uses alignment function
  lsi_y_sim <- align_matrix(lsi_y_sim, common_ids) 
  meta_y <- meta_y %>% filter(rendition_id %in% common_ids) %>% arrange(match(rendition_id, common_ids))
  
  # Creates Temporal Similarity matrices
  rec_sim <- make_temporal_similarity(meta_y, "rendition_id", "recording_id")
  day_sim <- make_temporal_similarity(meta_y, "rendition_id", "day_year")
  
  # Mantel test uses Similarity matrices
  res_rec <- run_mantel(lsi_y_sim, rec_sim) 
  res_day <- run_mantel(lsi_y_sim, day_sim)
  
  mantel_results_list[[as.character(y)]] <- bind_rows(
    tibble(year = y, scale = "Recording", r = res_rec$statistic, p = res_rec$pvalue, note = res_rec$note),
    tibble(year = y, scale = "Day",       r = res_day$statistic, p = res_day$pvalue, note = res_day$note)
  )
}

# Combine per-year results into a summary
mantel_yearly_summary <- bind_rows(mantel_results_list)

# Across-year test 
# Now creates LSI Similarity matrix
lsi_all_sim <- make_similarity_matrix(all_lsi_data, fill_missing = 0) 
meta_all_sub <- rendition_strings_and_metadata %>% filter(rendition_id %in% attr(lsi_all_sim, "Labels"))
# Uses alignment function
lsi_all_sim <- align_matrix(lsi_all_sim, meta_all_sub$rendition_id) 
# Now creates Temporal Similarity matrix
year_sim_all <- make_temporal_similarity(meta_all_sub, "rendition_id", "year") 
# Mantel test uses Similarity matrices
res_year_all <- run_mantel(lsi_all_sim, year_sim_all) 

mantel_yearly_summary <- mantel_yearly_summary %>%
  mutate(year = as.character(year))

mantel_yearly_summary <- bind_rows(
  mantel_yearly_summary,
  tibble(
    year = "All",
    scale = "Year",
    r = res_year_all$statistic,
    p = res_year_all$pvalue,
    note = res_year_all$note
  )
)

# 20.5 Save and print
print(mantel_yearly_summary)
write_csv(mantel_yearly_summary, "mantel_summary_table.csv")










#### 12. Run PCAs on each unit to check for clustering ####

library(dplyr)
library(ggplot2)

# Month abbreviation to number
month_map <- c(Dec = "12")

# Function to reformat Source
recode_source <- function(src) {
  m <- regmatches(src, regexec("^([A-Za-z]{3})([0-9]{2})_([0-9]{4})_([0-9]{4})$", src))[[1]]
  if (length(m) == 5) {
    month_num <- month_map[m[2]]
    day <- m[3]
    year <- m[4]
    time <- m[5]
    hour <- substr(time, 1, 2)
    minute <- substr(time, 3, 4)
    return(paste0(day, "/", month_num, "/", year, " ", hour, ":", minute))
  } else {
    return(src) # fallback if pattern doesn't match
  }
}


# Define numeric columns for PCA
pca_cols <- c("Dur90__s_", "BW90__Hz_", "Freq5__Hz_", "Freq95__Hz_", 
              "PeakFreq_Hz_", "StartFreqHz", "EndFreqHz", "Inflections")

# Keep only desired QualNames
keep_qualnames <- c("F", "G", "I",  "J4", "K","L1","L2","M1","M2","N")
qualnames <- intersect(keep_qualnames, unique(AllSongUnits$QualName))

# Letter labels
letters_vec <- LETTERS[seq_along(qualnames)]

# Loop through each QualName
for (i in seq_along(qualnames)) {
  qn <- qualnames[i]
  label_letter <- letters_vec[i]
  
  subset_data <- AllSongUnits %>% filter(QualName == qn)
  
  if (nrow(subset_data) < 3) {
    message(paste("Skipping", qn, "- not enough data for PCA"))
    next
  }
  
  # Keep numeric columns with variance > 0
  numeric_data <- subset_data[, pca_cols]
  numeric_data <- numeric_data[, sapply(numeric_data, function(x) var(x, na.rm = TRUE) > 0)]
  
  # PCA
  pca_result <- prcomp(numeric_data, scale. = TRUE)
  scores <- as.data.frame(pca_result$x)
  
  # Apply recode to Source
  scores$Source <- sapply(subset_data$Source, recode_source)
  
  scores$Selection <- subset_data$Selection
  
  # Plot with letter in top-left
  p <- ggplot(scores, aes(x = PC1, y = PC2, color = Source)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(x = "PC1", y = "PC2", color = "Source") +
    annotate("text", x = min(scores$PC1), y = max(scores$PC2),
             label = label_letter, hjust = -0.2, vjust = 1.2,
             size = 6) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.text = element_text(size = 11),
      plot.title = element_blank(),
      legend.title = element_blank(),
      legend.key.height = unit(15, "pt")
    )
  
  # Save PNG
  ggsave(
    filename = paste0("PCA_", qn, ".png"),
    plot = p,
    width = 15.9 / 2.54,
    height = 8 / 2.54,
    dpi = 300
  )
}
