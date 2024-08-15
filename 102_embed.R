library(tidyverse)
library(quanteda)
library(text2vec)
library(conText)
library(fst)

humdat <- read_fst("data/raw/_FinalData20210913.fst", as.data.table = T)

# head(humdat)
# str(humdat)

# ================================ choice parameters
# ================================
WINDOW_SIZE <- 6
DIM <- 300
ITERS <- 100
COUNT_MIN <- 1000 # words appear with min. frequency 1000 in local transform

# ================================ create vocab ================================
# tokens <- space_tokenizer(humdat$full_text)

# tokenize corpus removing unnecessary (i.e. semantically uninformative) elements
toks <- tokens(humdat$full_text, remove_punct=T, 
               remove_symbols=T, remove_numbers=T, 
               remove_separators=T)

saveRDS(toks, file = "data/embedding/humdat_toks.rds")

# clean out stopwords and words with 2 or fewer characters
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), 
                             selection = "remove", min_nchar=3)

saveRDS(toks_nostop, file = "data/embedding/humdat_toks_nostop.rds")

# only use features that appear at least 50 times in the corpus
toks_nostop <- readRDS("data/embedding/humdat_toks_nostop.rds")
# feats <- dfm(toks_nostop, tolower=T, verbose = FALSE) %>% 
#   dfm_trim(min_termfreq = 100) %>% 
#   featnames()
# 
# saveRDS(feats, file = "data/embedding/humdat_feats.rds")
# 
# # better to leave padding so non-adjacent terms don't appear together
# toks_nostop_feats <- tokens_select(toks_nostop, 
#                                    feats, padding = FALSE)
# 
# saveRDS(toks_nostop_feats, file = "data/embedding/humdat_toks_nostop_feats.rds")

#save some space
rm(humdat)
rm(toks)

# prepare tokens for embedding estimation
# toks_list <- space_tokenizer(toks_nostop)
toks_list <- word_tokenizer(tolower(toks_nostop))
rm(toks_nostop)
it <- itoken(toks_list, progressbar = TRUE)
vocab <- create_vocabulary(it)
saveRDS(vocab, file = "data/embedding/humdat_vocab.rds")
rm(toks_list)

# keep only words that meet count threshold
vocab_pruned <- prune_vocabulary(vocab, term_count_min = COUNT_MIN)
rm(vocab) #save some space
# ================================ create term co-occurrence matrix
# ================================
vectorizer <- vocab_vectorizer(vocab_pruned)

tcm <- create_tcm(it, vectorizer, skip_grams_window = WINDOW_SIZE,
                  skip_grams_window_context = "symmetric",
                  weights = rep(1, WINDOW_SIZE))

saveRDS(tcm, file = "data/embedding/humdat_tcm.rds")

# ================================ set model parameters
# ================================
glove <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)

# ================================ fit model ================================
word_vectors_main <- glove$fit_transform(tcm, n_iter = ITERS, convergence_tol = 0.001,
                                         n_threads = RcppParallel::defaultNumThreads())
 
# ================================ get output ================================
word_vectors_context <- glove$components
local_glove <- word_vectors_main + t(word_vectors_context)  

# word vectors
# ================================ save ================================

# toks_nostop_feats <- readRDS("data/embedding/humdat_toks_nostop_feats.rds")

# # construct the feature co-occurrence matrix for our toks_nostop_feats object (see above)
# toks_fcm <- fcm(toks_nostop_feats, context = "window",
#                 window = 6, count = "frequency",
#                 tri = FALSE) # important to set tri = FALSE
# 
# saveRDS(toks_fcm, file = "data/embedding/humdat_fcm.rds")
# 
# estimate glove model using text2vec
# glove <- GlobalVectors$new(rank = 300, 
#                            x_max = 10,
#                            learning_rate = 0.05)
# 
# wv_main <- glove$fit_transform(toks_fcm, n_iter = 10,
#                                convergence_tol = 1e-3, 
#                                n_threads = parallel::detectCores()) # set to 'parallel::detectCores()' to use all available cores
# 
# wv_context <- glove$components
# local_glove <- wv_main + t(wv_context) # word vectors

saveRDS(local_glove, file = "data/embedding/humdat_local_glove.rds")

#---------------------------------
# compute local transform
#---------------------------------
# the higher the threshold specified in the weighting arg, the faster the code

rm(list = ls())

tcm <- readRDS("data/embedding/humdat_tcm.rds")
local_glove <- readRDS("data/embedding/humdat_local_glove.rds")
vocab <- readRDS("data/embedding/humdat_vocab.rds")
COUNT_MIN <- 1000
vocab_pruned <- prune_vocabulary(vocab, term_count_min = COUNT_MIN)
rm(vocab)

local_transform <- compute_transform(x = tcm, pre_trained = local_glove,
                                     weighting = 1000)

local_transform <- compute_transform(x = tcm, 
                                     pre_trained = local_glove, 
                                     weighting = 'log')

saveRDS(local_transform, file = "data/embedding/humdat_local_transform.rds")