[![Static Badge](https://img.shields.io/badge/R-blue)](https://github.com/aysedeniz09/someassemblyrequired)
[![Static Badge](https://img.shields.io/badge/DOI:10.1177/14614448251341497-yellow)](http://dx.doi.org/10.1177/14614448251341497)


### Supplementary Code for:
### Some Assembly Required: Unpacking the Content and Spread of Wayfair Conspiracy Theory on Reddit and Twitter
#### Authors: [Dror Walter, PhD](https://cas.gsu.edu/profile/dror-walter/), [Ayse D. Lokmanoglu, PhD](https://aysedeniz09.github.io/); [Yotam Ophir, PhD](https://ophiryotam.com/); and [Eduard Fabregat, PhD](https://scholar.google.com/citations?user=QSTQ3TAAAAAJ&hl=en)
#### New Media and Society, 2025
#### <http://dx.doi.org/10.1177/14614448251341497>

Citation: Walter, D., Lokmanoglu, A., Ophir, Y., & Fabregat, E. (2025).  Some Assembly Required: Unpacking the Content and Spread of Wayfair Conspiracy Theory on Reddit and Twitter. *New Media and Society*. http://dx.doi.org/10.1177/14614448251341497


This repository contains the data and code used for the analysis and visualizations presented in the project. The original posts are not included in the dataset due to privacy considerations. Please contact the corresponding author to request access to the full set of posts.

## Code

The R source code is located in the [`code/`](code/) directory:

- **[Main Analysis](wayfair_github.md)**
   - Includes topic modeling, ANTMN clustering, time series analysis, and IRF plots.

- **[Supplementary Materials](wayfair-online-supplement-github.md)**
   - Includes cross-validation, full VAR coefficients, robustness checks and additional figures.

## [Data](https://github.com/aysedeniz09/someassemblyrequired/tree/main/data):

### metadata: `meta_theta_df_comm`

Below are the descriptions of the columns included in the `meta_theta_df_comm` dataframe.

| **Column Name** | **Type**    | **Description** |
|------------------|-------------|------------------|
| `date`           | POSIXct     | Timestamp of the post or comment. |
| `score`          | numeric     | Score or upvote count (e.g., Reddit upvotes). |
| `index`          | integer     | Unique integer identifier for each row. |
| `source`         | character   | Platform source (e.g., "reddit"). |
| `textcatcld2`    | character   | Language detected using the `cld2` package (e.g., "en"). |
| `index2`         | character   | String version of the row index (used for joining or tracking). |
| `1` to `35`      | numeric     | Topic proportions from a topic model (LDA). Each column represents the relevance of a specific topic to the document. |
| `Ltheme1`        | numeric     | **Moral Outrage (Green)** – Sum of topic proportions from Louvain cluster 1 in ANTMN. |
| `Ltheme2`        | numeric     | **Debates and Evidence (Gray)** – Sum of topic proportions from Louvain cluster 2. |
| `Ltheme3alt`     | numeric     | **Dismissive Only (Gold ALT)** – Alternate version of Louvain cluster 3 with one topic excluded. |
| `Ltheme3`        | numeric     | **Dismissive and Anti-Fact-Checking (Gold)** – Sum of topic proportions from full Louvain cluster 3. |
| `Ltheme4`        | numeric     | **Other: Emotions and News (Purple)** – Sum of topic proportions from Louvain cluster 4. |

### metadata: `data`

| **Column Name** | **Type**    | **Description** |
|------------------|-------------|------------------|
| `date`           | POSIXct     | Timestamp of the post or comment. |
| `score`          | numeric     | Score or upvote count (e.g., Reddit upvotes). |
| `index`          | integer     | Unique integer identifier for each row. |
| `source`         | character   | Platform source (e.g., "reddit"). |
| `textcatcld2`    | character   | Language detected using the `cld2` package (e.g., "en"). |
| `index2`         | integer     | Duplicate or alternative version of the index column; used for merging. |
