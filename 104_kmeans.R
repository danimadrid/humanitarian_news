library(pacman)
p_load(cluster, factoextra, tidyverse, NbClust)

##### Data loading and filtering #####

# Load embeddings for each humanitarian imaginary dimensions

df1 <- readRDS("data/output/cos_simsQ1_allcntries.rds")
df2 <- readRDS("data/output/cos_simsQ2_allcntries.rds")
df3 <- readRDS("data/output/cos_simsQ3_allcntries.rds")

# Load counts of stories per country
df_counts <- readxl::read_xlsx("data/output/cntcounts.xlsx") %>%
  select(seqvar = source.country,
         count)

# Filter out countries with less than 10k articles in corpus
df <- left_join(df1, df2, by = "seqvar") %>%
  left_join(df3, by = "seqvar") %>%
  left_join(df_counts, by = "seqvar") %>%
  arrange(-count) %>%
  filter(count > 10000)
rownames(df) <- df$seqvar

# Scale and center values
df <- df %>%
  select(-seqvar) %>%
  scale()

rownames(df) <- c("United Kingdom", "USA", "Russia", "Canada", "India", "France", "Pakistan", "China", 
                  "Philippines", "Australia", "Nigeria", "UAE", "Iran", "Qatar", "South Africa", 
                  "Saudi Arabia", "Israel", "Germany", "Ireland", "Turkey", "New Zealand", "Egypt", 
                  "Singapore", "South Korea", "Malaysia")

# Determine ideal k for k means
NbClust(df, method = 'complete', index = 'all')$Best.nc

k3 <- kmeans(df, centers = 3, nstart = 100, algorithm = "Forgy")

# Recode the values using a simple mapping function
clusters <- k3$cluster

recode_clusters <- sapply(clusters, function(x) {
  if (x == 1) {
    return(2)
  } else if (x == 2) {
    return(1)
  } else {
    return(x)  # if there are any other values, keep them as they are
  }
})

# Assign the recoded values back to the k3 list
k3$cluster <- recode_clusters

fviz_cluster(k3, data = df, geom = "text",
             main = "",
             label.rectangle = TRUE,
             labelsize = 30,
             legend = "bottom",
             legend.title = "Cluster",
             font.legend = c(22, "plain", "black"),
             font.x = c(22, "plain", "black"),
             font.y = c(22, "plain", "black"),
             ylim = c(-3.5, 3.5),
             xlim = c(-3.5, 3.5),
             ggtheme = theme_minimal())

ggsave("plots/kmeans3_new.png",
       width=580, height = 730,
       dpi=300, units="mm", bg = "white")

