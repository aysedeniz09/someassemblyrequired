Supplementary Material: Some Assembly Required: Unpacking the Content
and Spread of Wayfair Conspiracy Theory on Reddit and Twitter
================
22 May, 2025

Citation: Walter, D., Lokmanoglu, A., Ophir, Y., & Fabregat, E. (2025).
Some Assembly Required: Unpacking the Content and Spread of Wayfair
Conspiracy Theory on Reddit and Twitter. *New Media and Society*.
<http://dx.doi.org/10.1177/14614448251341497>

## 1. Setup & Libraries

Load libraries and set global options

``` r
### File I/O and Data Handling
library(readr)      # Reading delimited text files (CSV, TSV, etc.)
library(foreign)    # Import/export of data files from other stats packages (e.g., SPSS, Stata)
# library(xlsx)     # Reading/writing Excel files — may be problematic on macOS

### Text and Language Processing
library(quanteda)   # Text analysis and corpus management
library(tidytext)   # Tokenization and dictionary-based text processing
library(cld2)       # Language detection
library(stringi)    # String processing (fast and Unicode-aware)
library(stringr)    # String manipulation (part of the tidyverse)
library(rvest)      # Web scraping and HTML parsing

### Topic Modeling and NLP
library(topicmodels)  # Latent Dirichlet Allocation (LDA) and other topic models
library(ldatuning)    # Determining the optimal number of topics (K)
library(RNewsflow)    # NLP utilities for temporal and textual similarity

### Data Manipulation and Visualization
library(tidyverse)  # Core tidy tools: ggplot2, dplyr, tidyr, etc.
library(dplyr)      # Data manipulation (also part of tidyverse)
library(ggplot2)    # Plotting system based on the Grammar of Graphics
library(scales)     # Formatting and scaling axes in ggplot2
library(ggthemes)   # Additional themes and styles for ggplot2

### Date and Time Handling
library(lubridate)  # Simplifies date and time manipulation

### Parallel and Efficient Computing
library(doParallel) # Parallel processing support

### Time Series Analysis and Modeling
library(forecast)   # Forecasting time series data
library(tseries)    # Time series analysis and statistical tests
library(vars)       # Vector autoregression (VAR) models
library(dynlm)      # Dynamic linear regression models

### Networks and Graphs
library(igraph)     # Network analysis and graph theory tools

### Model Summary and Reporting
library(modelsummary) # Create regression tables and model summaries

###  Notes
#### library(xlsx)` is commented out due to compatibility issues on macOS.
#### `options(scipen = 99999)` is used to disable scientific notation in printed numbers.
```

## 2. Data Import & Integration

Load Reddit and Twitter datasets, clean, merge, and save/load combined
dataset

``` r
# Load collected Reddit and Twitter datasets
load("Twitter.Wayfair.clean.TM.Rdata")
load("Reddit.Wayfair.clean.TM.Rdata")

# Assign row indices for merging
reddit$index <- 1:nrow(reddit)
tweets$index <- 1:nrow(tweets)

# Select relevant columns and standardize names
reddit2 <- reddit %>% select(c(author, date, text, score, index))
reddit2$source <- "reddit"

tweets2 <- tweets %>% select(c(screen_name, created_at, text, favorite_count, index))
tweets2$source <- "twit"
colnames(tweets2) <- colnames(reddit2)

# Combine both platforms into one dataset
data <- rbind(reddit2, tweets2)

# Optionally: read saved RDS version
# saveRDS(data, "TW+RED.rds")
data <- readRDS("TW+RED.rds")
```

## 3. Text Preprocessing & Corpus Creation

``` r
# Check basic source distribution and date range
data %>% count(source)
min(data$date)
max(data$date)

# Backup original text column
data$textBU <- data$text

# Convert text to lowercase
data$text <- tolower(data$text)

# Detect language and filter for English using cld2
data$textcatcld2 <- cld2::detect_language(data$text)
data <- data[data$textcatcld2 == "en", ]

# Create row index for reference
data$index2 <- 1:nrow(data)

# Remove URLs and common platform-specific text artifacts (OLD method)
data$text <- gsub("http[^[:space:]]*", "", data$text)
data$text <- gsub("pic\\.[^[:space:]]+", "", data$text)
data$text <- gsub("[^[:space:]]*\\.com[^[:space:]]*", "", data$text)
data$text <- gsub("www\\.[^[:space:]]+", "", data$text)
data$text <- gsub("&amp", " ", data$text)
data$text <- gsub("RT", "", data$text)
data$text <- gsub("@[^[:space:]]*", "", data$text)

# Newer regex clean-up for mentions and links
data$text <- gsub("@[A-Za-z0-9_]{1,20}", "", data$text)
data$text <- gsub("https?\\S{1,200}", "", data$text)

# Remove extremely short or meaningless posts
data$textCOMP <- gsub("[^A-Za-z]", "", data$text)
data2 <- subset(data, nchar(data$textCOMP) >= 10)

# Remove duplicate content based on cleaned text
data3 <- data2[!duplicated(data2$textCOMP), ]

# Optional: check min/max dates for Reddit/Twitter subsets
data3Reddit <- data3 %>% filter(source == "reddit")
data3Twitter <- data3 %>% filter(source == "twit")
min(data3Reddit$date); max(data3Reddit$date)
min(data3Twitter$date); max(data3Twitter$date)

# Create corpus object for quanteda
mycorpus <- corpus(data3)

# Tokenize and remove short tokens
toks <- tokens(mycorpus)
toks <- tokens_select(toks, min_nchar = 3)

# Define a custom stopword list (including project-specific terms)
stopwords_and_single <- unique(c(
  tm::stopwords("en"), stopwords("en"), "|", "rt", "amp", "&amp", "=", "+", ">", "&", "$", "<", 
  "just", "can", "really", "got", "also", "will", 
  "wayfair", "#wayfair"
))

# Create the Document-Feature Matrix (DFM)
dfm_counts <- dfm(
  toks,
  tolower = TRUE,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove = stopwords_and_single,
  stem = FALSE,
  remove_separators = TRUE,
  include_docvars = TRUE,
  remove_url = TRUE
)

# Track alignment with original data
docnames(dfm_counts) <- data3$index2

# Initial DFM dimensions
dim(dfm_counts)

# Trim infrequent and overly common terms
dfm_counts2 <- dfm_trim(
  dfm_counts,
  max_docfreq = nrow(data3) * 0.5,
  min_docfreq = nrow(data3) * 0.00005,
  docfreq_type = "count"
)

# Post-trimming dimensions
dim(dfm_counts2)

# Diagnostic check: visualize word frequencies
quanteda.textplots::textplot_wordcloud(dfm_counts2, max_words = 300)

# Convert DFM to topicmodels format (DTM)
dtm_lda <- convert(dfm_counts2, to = "topicmodels")
```

## 4. Hyperparameter Tuning: Choosing Number of Topics (SearchK)

This section runs a cross-validated grid search over different values of
`K` (number of topics) and `alpha` (Dirichlet prior) using Gibbs
sampling and parallel processing.

``` r
# Close any open parallel connections
closeAllConnections()

# Define number of parallel cores to use
mycores <- detectCores() - 2  # Leave some cores free

# Define candidate alpha and K values
candidate_alpha <- c(25, 10, 5, 2, 1)              # Dirichlet priors
candidate_k <- c(2, seq(5, 100, by = 5))           # Number of topics to test
folds <- 5                                         # 5-fold cross-validation
n <- nrow(dtm_lda)                                 # Number of documents

# Create random fold assignments
splitfolds <- sample(1:folds, n, replace = TRUE)

# Create an empty queue for model validation jobs
validationQueueDF <- data.frame(k = numeric(0), alpha = numeric(0), fold = numeric(0))

# Generate combinations of k, alpha, and folds
for (val_alpha in candidate_alpha) {
  for (val_k in candidate_k) {
    for (val_i in 1:folds) {
      validationQueueDF <- rbind(validationQueueDF, data.frame(
        k = val_k,
        alpha = val_alpha / val_k,
        fold = val_i
      ))
    }
  }
}

# Sort queue to run larger K values first (may take longer)
validationQueueDF <- validationQueueDF[order(-validationQueueDF$k), ]
```

### 4.1.1 Parallel Validation Function

``` r
# Main function to perform parallel validation
validation <- function() {
  print(Sys.time())
  print(paste("Starting validation of", nrow(validationQueueDF), "models!"))
  
  # Initialize parallel cluster
  cluster <- makeCluster(mycores, outfile = "")
  registerDoParallel(cluster)
  clusterEvalQ(cluster, { library(topicmodels) })
  clusterExport(cluster, c("dtm_lda", "validationQueueDF", "splitfolds"))
  
  # Run models in parallel
  results <- foreach(j = 1:nrow(validationQueueDF), .combine = rbind) %dopar% {
    model <- as.numeric(as.vector(validationQueueDF[j, ]))
    model_k <- model[1]
    model_alpha <- model[2]
    model_fold <- model[3]
    
    train_set <- dtm_lda[splitfolds != model_fold, ]
    valid_set <- dtm_lda[splitfolds == model_fold, ]
    
    fitted <- LDA(train_set, k = model_k, method = "Gibbs", control = list(alpha = model_alpha))
    
    result_1model <- data.frame(
      k = model_k,
      alpha = model_alpha,
      fold = model_fold,
      perplexity = perplexity(fitted, newdata = valid_set)
    )
    return(result_1model)
  }
  
  stopCluster(cluster)
  print("Validation complete!")
  print(Sys.time())
  
  return(results)
}

# Run the full grid search
val_results <- validation()
```

### 4.1.2 Save and Plot Results

``` r
# Optional: save environment
# save.image("Wayfair_data_POST_searchk.rdata")

# Create summary variables
val_results$kalpha <- paste0(val_results$k, val_results$alpha)
val_results$newalpha <- as.numeric(val_results$alpha * val_results$k)

# Boxplot of perplexity by K
ggplot(val_results) +
  geom_boxplot(aes(x = k, y = perplexity, group = kalpha, color = factor(newalpha))) +
  ggtitle("Cross-Validation of Topic Models") +
  labs(x = "Number of Topics (K)", y = "Perplexity (Lower is better)")
```

**Note:** You can refine `mycores`, `candidate_k`, and `candidate_alpha`
depending on compute resources and model stability.

### 4.2 Topic Coherence Matrices with alpha (`FindTopicsNumber`)

This section uses the `ldatuning` package to evaluate multiple topic
coherence metrics across candidate values of `K`. These include:

- **Griffiths2004**: log-likelihood (higher is better)
- **CaoJuan2009**: topic similarity (lower is better)
- **Arun2010**: divergence metric (lower is better)
- **Deveaud2014**: pointwise mutual information (higher is better)

``` r
# Set seed and alpha (if you haven’t earlier)
myalpha=2/30

# Run topic number diagnostics
Sys.time()

FTN_result <- FindTopicsNumber(
  dtm_lda,
  topics = c(2, seq(5, 100, by = 5)),  # Candidate values of K
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(alpha = myalpha, seed = 6723),  # Reproducibility
  mc.cores = 70L,  # Number of cores available (adjust as needed)
  verbose = TRUE
)

Sys.time()

# Plot the results
FindTopicsNumber_plot(FTN_result)
```

**Notes:** - This method is often faster than cross-validation and
provides interpretable plots of model quality. - You can use this output
to **triangulate your choice of `K`** based on agreement across metrics.

## 5. Fitting the Final Topic Model

This section runs the final topic model using `foreach` and parallel
processing. The number of topics (`K`) is defined in `runsdf`, and
models are fit with symmetric Dirichlet prior (`alpha = myalpha / K`).

``` r
# Define number of topics to run (can include multiple values)
runsdf <- data.frame(myk = c(35))   # Replace with your selected K(s)

# Set alpha (used in symmetric prior: alpha / K)
myalpha <- 2

# Initialize model list
mymodels <- list()

# Create parallel cluster, leaving spare cores for safety
cluster <- makeCluster(detectCores(logical = TRUE) - 6)
registerDoParallel(cluster)

# Load required libraries in each cluster node
clusterEvalQ(cluster, {
  library(topicmodels)
})

# Export required objects to cluster
clusterExport(cluster, c("dtm_lda", "runsdf", "myalpha"))

# Run model(s) in parallel using foreach
system.time({
  mymodels <- foreach(j = 1:nrow(runsdf)) %dopar% {
    k_run <- runsdf[j, 1]
    
    fitted <- LDA(
      dtm_lda,
      k = k_run,
      method = "Gibbs",
      control = list(alpha = myalpha / k_run, seed = 527)
    )
    
    return(fitted)
  }
})

# Stop the parallel cluster
stopCluster(cluster)

# Store results in LDAlist for consistency
LDAlist <- mymodels

# Optional: save to .RData file for later reuse
# save.image("Wayfair_data_POST_Model.rdata")
```

**Notes:** - You can expand `runsdf` to run multiple topic values in one
go. - `LDAlist[[1]]` will contain your final model if only one value is
used. - This structure allows reproducibility and scalability across
systems.

## 6. Topic Interpretation and Export of Top Words & Texts

This section extracts: - Top words by **raw probability** (`beta`) - Top
words by **FREX-like exclusivity** - Top texts per topic by **highest
gamma** Each is saved as an `.xlsx`

``` r
 file for interpretation and documentation.

```r
LDAlist <- mymodels
datacolnum <- which(colnames(data3) == "text")
projectname <- "Wayfair_MODEL"

for (eachLDA in LDAlist) {
  LDAfit <- eachLDA
  
  # Extract and exponentiate beta matrix
  mybeta <- data.frame(LDAfit@beta)
  colnames(mybeta) <- LDAfit@terms
  mybeta <- t(mybeta)
  colnames(mybeta) <- seq(1:ncol(mybeta))
  mybeta <- exp(mybeta)
  
  # Top words by beta (raw probability)
  nwords <- 50
  topwords <- mybeta[1:nwords, ]
  for (i in 1:LDAfit@k) {
    tempframe <- mybeta[order(-mybeta[, i]), ]
    tempframe <- tempframe[1:nwords, ]
    tempvec <- as.vector(rownames(tempframe))
    topwords[, i] <- tempvec
  }
  rownames(topwords) <- 1:nwords
  kalpha <- paste0(LDAfit@k, "_", gsub("\\.", "", LDAfit@alpha))
  write.xlsx(topwords, paste0(kalpha, projectname, "_Topwords.xlsx"))
  
  ### FREX-like exclusivity ranking
  myw <- 0.3
  word_beta_sums <- rowSums(mybeta)
  my_beta_for_frex <- mybeta
  for (m in 1:ncol(my_beta_for_frex)) {
    for (n in 1:nrow(my_beta_for_frex)) {
      my_beta_for_frex[n, m] <- 1 / (
        myw / (my_beta_for_frex[n, m] / word_beta_sums[n]) +
        (1 - myw) / my_beta_for_frex[n, m]
      )
    }
    print(m)
  }
  
  topwords <- my_beta_for_frex[1:nwords, ]
  for (i in 1:LDAfit@k) {
    tempframe <- my_beta_for_frex[order(-my_beta_for_frex[, i]), ]
    tempframe <- tempframe[1:nwords, ]
    tempvec <- as.vector(rownames(tempframe))
    topwords[, i] <- tempvec
  }
  rownames(topwords) <- 1:nwords
  write.xlsx(topwords, paste0(kalpha, projectname, "_TopFREX.xlsx"))
  
  ### Top texts per topic by highest gamma
  data33 <- data3
  deleted_lda_texts <- setdiff(as.character(data3$index2), as.character(LDAfit@documents))
  `%!in%` <- function(x, y) !x %in% y
  data33 <- data33[data33$index2 %!in% deleted_lda_texts, ]
  metadf <- data33
  
  meta_theta_df <- cbind(metadf[datacolnum], LDAfit@gamma)
  ntext <- 100
  toptexts <- mybeta[1:ntext, ]
  
  for (i in 1:LDAfit@k) {
    print(i)
    tempframe <- meta_theta_df[order(-meta_theta_df[, i + 1]), ]
    tempframe <- tempframe[1:ntext, ]
    tempvec <- as.vector(tempframe[, 1])
    toptexts[, i] <- tempvec
  }
  rownames(toptexts) <- 1:ntext
  write.xlsx(toptexts, paste0(kalpha, projectname, "_TopTexts.xlsx"))
}
```

**Notes:** - You can interpret “Topwords” and “TopFREX” side-by-side for
robustness. - “TopTexts” is especially useful for qualitative validation
of each topic. - Outputs are automatically named using `K_alpha`.

## 7. ANTMN: Topic Network Construction and Louvain-Based Theming

This section uses the
[`ANTMN`](http://dx.doi.org/10.1080/19312458.2019.1639145) (Walter &
Ophir, 2019) to create a network of topics based on their co-occurrence
in documents. The Louvain method is used for community detection, and
the results are visualized.

``` r
##################
#### ANTMNING ####
##################

# Step 1: Filter out any documents excluded from LDA model
data33 <- data3
data33$index2 <- as.character(data33$index2)
deleted_lda_texts <- setdiff(data33$index2, LDAfit@documents)
`%!in%` <- function(x, y) !('%in%'(x, y))
data33 <- data33[data33$index2 %!in% deleted_lda_texts, ]

# Step 2: Create the combined topic+metadata dataframe
metadf <- data33
meta_theta_df <- cbind(metadf, LDAfit@gamma)

# Step 3: Rejoin removed duplicated texts, if any
removed_df2 <- inner_join(removed_df, meta_theta_df, by = "text")
removed_df2 <- removed_df2[, -c(11:19)]  # Clean extra columns
colnames(removed_df2) <- gsub("\\.x", "", colnames(removed_df2))
removed_df2$index2 <- as.character(removed_df2$index2)
meta_theta_df2 <- bind_rows(meta_theta_df, removed_df2)

# Step 4: Compute topic sizes for network node weighting
topic_size <- colSums(meta_theta_df2[, c(11:45)])  # Adjust if K ≠ 35
```

### 7.1 Define ANTMN Function: `network_from_LDA()`

``` r
network_from_LDA <- function(LDAobject, deleted_topics = c(), topic_names = c(), save_filename = "", topic_size = c(), bbone = FALSE) {
  require(lsa)
  require(dplyr)
  require(igraph)
  require(corpustools)
  
  print("Importing model")
  theta <- LDAobject@gamma
  colnames(theta) <- 1:LDAobject@k
  
  mycosine <- cosine(as.matrix(theta))
  colnames(mycosine) <- colnames(theta)
  rownames(mycosine) <- colnames(theta)
  
  print("Creating graph")
  topmodnet <- graph.adjacency(mycosine, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = "label")
  
  if (length(topic_names) > 0) {
    print("Topic names added")
    V(topmodnet)$name <- topic_names
  }
  
  if (length(topic_size) > 0) {
    print("Topic sizes added")
    V(topmodnet)$topic_size <- topic_size
  }
  
  newg <- topmodnet
  
  if (length(deleted_topics) > 0) {
    print("Deleting requested topics")
    newg <- delete_vertices(topmodnet, deleted_topics)
  }
  
  if (bbone == TRUE) {
    print("Backboning")
    nnodesBASE <- length(V(newg))
    for (bbonelvl in rev(seq(0, 1, by = 0.05))) {
      nnodes <- length(V(backbone_filter(newg, alpha = bbonelvl)))
      if (nnodes >= nnodesBASE) {
        bbonelvl = bbonelvl
      } else {
        break
      }
      oldbbone <- bbonelvl
    }
    newg <- backbone_filter(newg, alpha = oldbbone)
  }
  
  print("Calculating communities")
  V(newg)$louvain <- cluster_louvain(newg)$membership
  V(newg)$walktrap <- cluster_walktrap(newg)$membership
  V(newg)$spinglass <- cluster_spinglass(newg)$membership
  V(newg)$fastgreed <- cluster_fast_greedy(newg)$membership
  V(newg)$eigen <- cluster_leading_eigen(newg)$membership
  
  if (nchar(save_filename) > 0) {
    print("Writing graph")
    write.graph(newg, paste0(save_filename, ".graphml"), format = "graphml")
  }
  
  return(newg)
}
```

### 7.2 Construct Topic Network and Apply Louvain Clustering

``` r
# Load manually labeled topics
topic_labels <- openxlsx::read.xlsx("YOLabels.xlsx", 1)
topic_labels <- colnames(topic_labels)[-1]

# Build network with all topics
mynewnet <- network_from_LDA(
  LDAobject = LDAfit,
  save_filename = "WF_ALL_35",
  topic_names = topic_labels,
  topic_size = topic_size,
  bbone = TRUE
)

# Remove 'junk' topics (marked DELETE)
mynewnet_nojunk <- network_from_LDA(
  LDAobject = LDAfit,
  deleted_topics = grep("DELETE", topic_labels),
  save_filename = "WF_NOJUNK_35",
  topic_names = topic_labels,
  topic_size = topic_size,
  bbone = TRUE
)

# Remove 'junk' and 'unrelated' topics
mynewnet_nojunkplus <- network_from_LDA(
  LDAobject = LDAfit,
  deleted_topics = grep("DELETE|UNRELATED", topic_labels),
  save_filename = "WF_NOJUNKPLUS_35",
  topic_names = topic_labels,
  topic_size = topic_size,
  bbone = TRUE
)

# Use the final cleaned network
finalnet <- mynewnet_nojunkplus
meta_theta_df_comm <- meta_theta_df2
```

### 7.3 Create Thematic Aggregates from Louvain Clusters

``` r
# Aggregating topics by Louvain communities into themes
meta_theta_df_comm$Ltheme1 <- rowSums(meta_theta_df_comm[, (as.numeric(V(finalnet)$label[which(V(finalnet)$louvain == 1)])) + 10])  # Green - Moral Outrage
meta_theta_df_comm$Ltheme2 <- rowSums(meta_theta_df_comm[, (as.numeric(V(finalnet)$label[which(V(finalnet)$louvain == 2)])) + 10])  # Gray - Debates and Evidence
meta_theta_df_comm$Ltheme3alt <- rowSums(meta_theta_df_comm[, (as.numeric(V(finalnet)$label[which(V(finalnet)$louvain == 3)]))[-4] + 10])  # GoldALT - Dismissive only
meta_theta_df_comm$Ltheme3 <- rowSums(meta_theta_df_comm[, (as.numeric(V(finalnet)$label[which(V(finalnet)$louvain == 3)])) + 10])  # Gold - Dismissive and anti-fact-checking
meta_theta_df_comm$Ltheme4 <- rowSums(meta_theta_df_comm[, (as.numeric(V(finalnet)$label[which(V(finalnet)$louvain == 4)])) + 10])  # Purple - Other (emotions and news)
```

**Notes:** - `finalnet` is saved to `.graphml` and can be visualized in
[Gephi](https://gephi.org/). - The `Ltheme` columns are your core
variables for time series, platform comparisons, and narrative
construction. - You can edit `grep("DELETE|UNRELATED", topic_labels)` to
exclude other label types if needed.

## 8. Platform Comparison: Reddit vs. Twitter

This section compares each Louvain theme’s prominence between Reddit and
Twitter posts using both **t-tests** and **Wilcoxon rank-sum tests**.
The results are exported to Excel.

``` r
# Create a working copy of the data
data_for_corrWILCOX <- meta_theta_df_comm

# Extract Ltheme columns (46–50) by platform
wilc_reddit <- data.frame(data_for_corrWILCOX[data_for_corrWILCOX$source == "reddit", 46:50])
wilc_twitter <- data.frame(data_for_corrWILCOX[data_for_corrWILCOX$source == "twit", 46:50])
```

### 8.1 T-Test Comparison

``` r
ttestnum <- c()
ttest.sig <- c()
ttest.mean <- c()

for (i in 1:ncol(wilc_reddit)) {
  print(i)
  ttestnum <- c(ttestnum, i)
  mytest <- t.test(wilc_reddit[, i], wilc_twitter[, i])
  ttest.sig <- c(ttest.sig, mytest$p.value)
  ttest.mean <- c(ttest.mean, mytest$estimate[1] - mytest$estimate[2])
}

ttest.df <- data.frame(ttestnum, ttest.sig, ttest.mean)
```

### 8.2 Wilcoxon Rank-Sum Test (Nonparametric)

``` r
wtestnum <- c()
wtest.sig <- c()
wtest.mean <- c()

for (i in 1:ncol(wilc_reddit)) {
  print(i)
  wtestnum <- c(wtestnum, i)
  mytest <- wilcox.test(wilc_reddit[, i], wilc_twitter[, i], paired = FALSE)
  wtest.sig <- c(wtest.sig, mytest$p.value)
  wtest.mean <- c(wtest.mean, mean(wilc_reddit[, i]) - mean(wilc_twitter[, i]))
}

wilcox.df <- data.frame(wtestnum, wtest.sig, wtest.mean)
```

### 8.3 Merge and Export Results

``` r
# Combine t-test and Wilcoxon results
test_of_diffs <- bind_cols(ttest.df, wilcox.df)
test_of_diffs$topicnum <- colnames(wilc_reddit)

# Optional: include topic names if available
# test_of_diffs$topicname <- topic_labels

# Save to Excel
xlsx::write.xlsx(test_of_diffs, "Wayfair VS Twitter themes.xlsx")
```

**Notes:** - `ttest.mean` and `wtest.mean` capture Reddit - Twitter
differences. - Significant values indicate that Reddit and Twitter vary
in how prominently a given theme appears.

------------------------------------------------------------------------

## 9. Time Series Analysis: VAR Models and Granger Causality

This section performs: - Stationarity testing and differencing - Vector
Autoregression (VAR) model fitting - Granger causality tests - Export of
model summaries and Granger test results

### 9.1 Stationarity and Differencing (Volume)

``` r
ts_data <- data |> 
  mutate(forcount = 1,
         hour = lubridate::floor_date(date, unit = "hour")) |> 
  group_by(source, hour) |> 
  summarise(postcount = sum(forcount)) |> 
  pivot_wider(names_from = "source", values_from = "postcount")

# Fill NA with 0 and difference both series
ts_data[is.na(ts_data)] <- 0
ts_data$reddit_diff <- c(NA, diff(ts_data$reddit))
ts_data$twit_diff <- c(NA, diff(ts_data$twit))

# Drop NA and convert to time series object
dat_comm1.1 <- ts_data |>  
  dplyr::select(reddit_diff, twit_diff) |> 
  drop_na()
dat_comm1.1 <- ts(dat_comm1.1)
```

### 9.2 Lag Selection, Cointegration, and ADF Tests

``` r
# Lag selection
lagselect <- VARselect(dat_comm1.1, lag.max = 48, type = "both")

# ACF check
Acf(dat_comm1.1[, "reddit_diff"])
Acf(dat_comm1.1[, "twit_diff"])

# ADF test for stationarity
adf.test(dat_comm1.1[, "reddit_diff"], k = 4) 
adf.test(dat_comm1.1[, "twit_diff"], k = 4)

# Cointegration test via residual ADF
coint1.1 <- dynlm(reddit_diff ~ twit_diff, data = dat_comm1.1)
ehat1.1 <- resid(coint1.1)
johtest_1.1 <- adf.test(ehat1.1, k = 4)
```

### 9.3 VAR Estimation and Granger Causality (Volume)

``` r
fitvarcomm_vol <- VAR(dat_comm1.1, p = 4, type = "both")
summary(fitvarcomm_vol)

gc_redvolume <- causality(fitvarcomm_vol, cause = "reddit_diff")
gc_twittervol <- causality(fitvarcomm_vol, cause = "twit_diff")
```

------------------------------------------------------------------------

### 9.4 Time Series on Louvain Themes

``` r
columns_to_select <- c("Ltheme1", "Ltheme2", "Ltheme3alt", "Ltheme3", "Ltheme4")

wrangle_data <- function(input_df, columns_to_select) {
  data_ts <- input_df |> 
    dplyr::select(date, source, !!!columns_to_select) |> 
    mutate(hour = lubridate::floor_date(date, unit = "hour")) |> 
    group_by(hour, source) |> 
    summarise(across(all_of(columns_to_select), ~ sum(.))) |>
    ungroup()

  data_ts2 <- data_ts |> 
    pivot_wider(names_from = "source", values_from = all_of(columns_to_select))

  data_ts2[is.na(data_ts2)] <- 0
  return(data_ts2)
}

data_ts2 <- wrangle_data(meta_theta_df_comm, columns_to_select)

# Apply differencing to all columns (except hour)
for (col in names(data_ts2)[-1]) {
  data_ts2[[paste0(col, "_diff")]] <- c(NA, diff(data_ts2[[col]]))
  data_ts2[[col]] <- NULL
}
```

------------------------------------------------------------------------

### 9.5 Function for Time Series Modeling (Themes)

``` r
timeseries_func <- function(input_df, version_number, column1, column2) {
  selected_columns <- c(column1, column2)
  dat_comm <- input_df |> dplyr::select(all_of(selected_columns)) |> drop_na()
  dat_comm <- ts(dat_comm)

  SC <- VARselect(dat_comm, lag.max = 24, type = "both")[["selection"]][["SC(n)"]]
  assign("SC", SC, envir = .GlobalEnv)

  adf_test_column1 <- adf.test(dat_comm[, column1], k = SC)
  adf_test_column2 <- adf.test(dat_comm[, column2], k = SC)

  formula <- as.formula(paste(column1, "~", column2))
  coint <- dynlm(formula, data = dat_comm)
  ehat <- resid(coint)
  johtest <- adf.test(ehat, k = SC)

  fitvarcomm <- VAR(dat_comm, p = SC, type = "both")
  gc_column1 <- causality(fitvarcomm, cause = column1)
  gc_column2 <- causality(fitvarcomm, cause = column2)

  results <- list(
    dat_comm = dat_comm,
    SC = SC,
    adf_test_column1 = adf_test_column1,
    adf_test_column2 = adf_test_column2,
    johtest = johtest,
    fitvarcomm = fitvarcomm,
    gc_column1 = gc_column1,
    gc_column2 = gc_column2
  )

  assign(paste("results_v", version_number, sep = ""), results, envir = .GlobalEnv)
  return(results)
}
```

------------------------------------------------------------------------

### 9.6 Apply Time Series Function to Louvain Themes

``` r
timeseries_func(data_ts2, "1.1", "Ltheme1_reddit_diff", "Ltheme1_twit_diff")
timeseries_func(data_ts2, "1.2", "Ltheme2_reddit_diff", "Ltheme2_twit_diff")
timeseries_func(data_ts2, "1.3", "Ltheme3_reddit_diff", "Ltheme3_twit_diff")
timeseries_func(data_ts2, "1.3_alt", "Ltheme3alt_reddit_diff", "Ltheme3alt_twit_diff")
timeseries_func(data_ts2, "1.4", "Ltheme4_reddit_diff", "Ltheme4_twit_diff")
```

------------------------------------------------------------------------

### 10. Figures and Tables

### 10.1 Figure and Table Reference Map

The following labels correspond to the figures and tables presented in
the **main article**. Supplementary figures and tables will be
documented in a separate markdown file.

| Label & Caption |
|----|
| **Figure 1.** *Reddit and Twitter Hourly Volume* |
| **Figure 3 (Panels A–E).** *IRF Graphs Impulse = Reddit // Response = Twitter. The IRF graphs illustrate the standard deviation (SD) over time (in hours) for Twitter response (red line) to impulse Reddit.* |
| **Figure 4 (Panels A–B).** *IRF Graphs Impulse = Twitter // Response = Reddit. The IRF graphs illustrate the standard deviation (SD) over time (in hours) for Reddit response (red line) to impulse Twitter.* |
| **Tables 1 and 2.** *Granger Causality Results for Reddit on Twitter / Twitter on Reddit. Includes F-Test, Granger p-values, and significance for volume and themes.* |

#### Table 1. Granger Causality Results for Reddit on Twitter

#### Table 2. Granger Causality Results for Twitter on Reddit

``` r
granger_tests <- data.frame(
  Response = c("Reddit on Twitter", 
               "Twitter on Reddit",
               "Louvain Theme 1 Reddit on Twitter", 
               "Louvain Theme 1 Twitter on Reddit", 
               "Louvain Theme 2 Reddit on Twitter", 
               "Louvain Theme 2 Twitter on Reddit", 
               "Louvain Theme 3 Reddit on Twitter", 
               "Louvain Theme 3 Twitter on Reddit",
               "Louvain Theme 4 Reddit on Reddit", 
               "Louvain Theme 4 Twitter on Twitter"),
  granger.p.value = c(
    gc_redvolume$Granger$p.value,
    gc_twittervol$Granger$p.value,
    results_v1.1$gc_column1$Granger$p.value, results_v1.1$gc_column2$Granger$p.value,
    results_v1.2$gc_column1$Granger$p.value, results_v1.2$gc_column2$Granger$p.value,
    results_v1.3$gc_column1$Granger$p.value, results_v1.3$gc_column2$Granger$p.value,
    results_v1.4$gc_column1$Granger$p.value, results_v1.4$gc_column2$Granger$p.value
  )
)

granger_tests$granger.p.value <- round(granger_tests$granger.p.value, 4)
openxlsx::write.xlsx(granger_tests, file = "Granger_Results.xlsx", rownames = FALSE)
```

#### Figure 1: Reddit and Twitter Hourly Volume

``` r
data |> 
  mutate(forcount = 1,
         hour = lubridate::floor_date(date, unit = "hour")) |> 
  group_by(hour, source) |> 
  summarise(sum = sum(forcount)) |> 
  mutate(source = recode(source,
                         'reddit' = 'Reddit',
                         'twit' = 'Twitter')) |> 
  ggplot(aes(x = ymd_hms(hour), y = sum)) + 
  geom_line() + 
  facet_wrap(~source,
             nrow = 2,
             scales = "free_y") + 
  scale_y_continuous(labels = comma) +
  scale_x_datetime(labels = scales::label_date("%Y-%m-%d %H:%M:%S"), 
                   date_breaks = "6 hour") +
  xlab("Hour") +
  ylab("Post Volume") +
  ggtitle("Hourly Post Volume in Reddit & Twitter") +
  theme_hc() +
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(vjust = -0.25, size = 12),
        axis.title.y = element_text(vjust = -0.50, size = 12),
        legend.position = "bottom", 
        legend.box = "vertical", 
        legend.title = element_blank(),
        legend.margin = margin(),
        legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA),
        legend.box.background = element_blank())

ggsave("Figure_Reddit_Twit_overtime.jpeg", 
       bg = "white", width = 14, height = 8, dpi = 300)
```

### Figure 3: IRF Graphs — Impulse = Reddit, Response = Twitter

``` r
volume_twitter <- getIRFPlotData("reddit_diff", "twit_diff", irf_comm_twitter_vol)

LVtheme1_T <- getIRFPlotData("Ltheme1_reddit_diff", "Ltheme1_twit_diff", results_v1.1$irf_col2_response_twitter)
LVtheme2_T <- getIRFPlotData("Ltheme2_reddit_diff", "Ltheme2_twit_diff", results_v1.2$irf_col2_response_twitter)
LVtheme3_T <- getIRFPlotData("Ltheme3_reddit_diff", "Ltheme3_twit_diff", results_v1.3$irf_col2_response_twitter)
LVtheme4_T <- getIRFPlotData("Ltheme4_reddit_diff", "Ltheme4_twit_diff", results_v1.4$irf_col2_response_twitter)

plot_twitter <- rbind(volume_twitter, LVtheme1_T, LVtheme2_T, LVtheme3_T, LVtheme4_T)

plot_twitter <- plot_twitter |> mutate(group = paste0(Impulse, Response))
plot_twitter <- as.data.frame(plot_twitter)

plot_twitter$Response <- recode(plot_twitter$Response, 
                               twit_diff = "Twitter",
                               Ltheme1_twit_diff = "(1) Moral outrage: Twitter",
                               Ltheme2_twit_diff = "(2) Debates & evidence: Twitter",
                               Ltheme3_twit_diff = "(3) Dismissive and anti-fact-checking: Twitter",
                               Ltheme4_twit_diff = "(4) Emotional reaction: Twitter")

plot_twitter$group <- factor(plot_twitter$group, 
                             levels = c("reddit_difftwit_diff",
                                        "Ltheme1_reddit_diffLtheme1_twit_diff",
                                        "Ltheme2_reddit_diffLtheme2_twit_diff",
                                        "Ltheme3_reddit_diffLtheme3_twit_diff",
                                        "Ltheme4_reddit_diffLtheme4_twit_diff"))

labels_twitter <- as_labeller(c(
  "reddit_difftwit_diff" = "Panel A: Volume Impulse: Reddit // Response: Twitter", 
  "Ltheme1_reddit_diffLtheme1_twit_diff" = "Panel B: (1) Moral outrage: Impulse = Reddit // Response = Twitter",
  "Ltheme2_reddit_diffLtheme2_twit_diff" = "Panel C: (2) Debates & evidence Impulse = Reddit // Response = Twitter",
  "Ltheme3_reddit_diffLtheme3_twit_diff" = "Panel D: (3) Dismissive and anti-fact-checking Impulse = Reddit // Response = Twitter",
  "Ltheme4_reddit_diffLtheme4_twit_diff" = "Panel E: (4) Emotional reaction Impulse = Reddit // Response = Twitter"
))

ggplot(plot_twitter, aes(x = Theme, y = irf)) +
  geom_line(color = "red2", size = 1.5) +
  geom_line(aes(y = Upper), linetype = "dashed") +
  geom_line(aes(y = Lower), linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid", color = "blue2") +
  scale_x_continuous(breaks = 0:10) +
  facet_wrap(~group, labeller = labels_twitter, nrow = 5, scales = "free_y") +
  labs(x = "time(hours)", y = "Volume") +
  ggtitle("") +
  theme_hc() +
  theme(text = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(angle = 12, hjust = 1, family = "sans"),
        axis.text.y = element_text(family = "sans"),
        axis.title.x = element_text(vjust = -0.25, size = 12, family = "sans"),
        axis.title.y = element_text(vjust = -0.25, size = 12, family = "sans"),
        legend.position = "none",
        legend.box = "vertical",
        legend.margin = margin(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12, family = "sans"))

ggsave("IRF_Response_Twitter.jpeg", bg = "white", width = 8, height = 6, dpi = 300)
```

#### Figure 4: IRF Graphs — Impulse = Twitter, Response = Reddit

``` r
plot_all_twit <- rbind(LVtheme2_T, LVtheme3_T)

plot_all_twit <- plot_all_twit |> mutate(group = paste0(Impulse, Response))
plot_all_twit <- as.data.frame(plot_all_twit)

plot_all_twit$Response <- recode(plot_all_twit$Response, 
                                 Ltheme2_reddit_diff = "(2) Debates & Evidence: Reddit",
                                 Ltheme3_reddit_diff = "(3) Dismissive and anti-fact-checking: Reddit")

labels_twit <- as_labeller(c(
  "Ltheme2_twit_diffLtheme2_reddit_diff" = "Panel A: Debates & Evidence Impulse = Twitter // Response = Reddit",
  "Ltheme3_twit_diffLtheme3_reddit_diff" = "Panel B: Dismissive and anti-fact-checking Impulse = Twitter // Response = Reddit"
))

ggplot(plot_all_twit, aes(x = Theme, y = irf)) +
  geom_line(color = "red2", size = 1.5) +
  geom_line(aes(y = Upper), linetype = "dashed") +
  geom_line(aes(y = Lower), linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid", color = "blue2") +
  facet_wrap(~group, labeller = labels_twit, nrow = 4, scales = "free") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "time(hour)", y = "Theme Prominence on Reddit") +
  theme_hc() +
  theme(text = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(angle = 12, hjust = 1, family = "sans"),
        axis.text.y = element_text(family = "sans"),
        axis.title.x = element_text(vjust = -0.25, size = 12, family = "sans"),
        axis.title.y = element_text(vjust = -0.25, size = 12, family = "sans"),
        legend.position = "none",
        legend.box = "vertical",
        legend.margin = margin(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 12, family = "sans"))

ggsave("IRF_Twitter_Themes.jpeg", bg = "white", width = 8, height = 6, dpi = 300)
```
