# File Structure for `humanitarian_news` Repository

This document describes the structure of the `humanitarian_news/` folder containing the replication materials for the associated research paper.

Files shown with ~~strikethrough~~ are **not included** in the replication archive due to copyright or licensing restrictions.

```
humanitarian_news/
│
├── 00_scraper.R
├── 00b_google_news_scraper.R
├── 01_newsapi.R
├── 02_nexis_parser.R
├── 02b_nexis_numbers.R
├── 03_factiva_parser.R
├── 03b_factiva_numbers.R
├── 04_serpapi.R
├── 05_gdelt.R
├── 06_merging.R
├── 100_process_text.R
├── 101_process_survey.R
├── 102_embed.R
├── 103_plot_embed.R
├── 104_kmeans.R
├── 105_processplot.R
├── 106_digital.R
├── 107_corpus_export.R
├── 108_similarityanalysis.R
│
├── 📁 data/
│   ├── 📁 embedding/
│   │   ├── humdat_feats.rds
│   │   ├── humdat_local_glove.rds
│   │   ├── humdat_local_transform.rds
│   │   ├── humdat_tcm.rds
│   │   ├── humdat_toks_nostop_feats.rds
│   │   ├── humdat_toks_nostop.rds
│   │   ├── humdat_toks.rds
│   │   └── humdat_vocab.rds
│   │
│   ├── 📁 pretrained/
│   │   ├── glove.rds
│   │   └── khodakA.rds
│   │
│   ├── 📁 output/
│   │   ├── all_similarities.xlsx
│   │   ├── cntcodes.rds
│   │   ├── cntcounts.xlsx
│   │   ├── cos_countries.csv
│   │   ├── cos_countries.xlsx
│   │   ├── cos_simsQ1_allntries.rds
│   │   ├── cos_simsQ1_digital.rds
│   │   ├── cos_simsQ2_allntries.rds
│   │   ├── cos_simsQ2_digital.rds
│   │   ├── cos_simsQ3_allntries.rds
│   │   ├── cos_simsQ3_digital.rds
│   │   ├── regions.csv
│   │   ├── ~~sample_10k.rds~~
│   │   ├── ~~sample_10k.txt~~
│   │   ├── ~~sample_79k.rds~~
│   │   ├── ~~sample_79k.txt~~
│   │   ├── similarities_agencies_25countries.xlsx
│   │   ├── similarity_analysis.xlsx
│   │   └── sources_corpus.xlsx
│   │
│   ├── 📁 qualtrics/
│   │   ├── emerg_imag_form_December 19, 2021_05.57.csv
│   │   ├── Q1seed.rds
│   │   ├── Q1seeds.rds
│   │   ├── Q2seed.rds
│   │   └── Q3seed.rds
│   │
│   ├── 📁 raw/
│   │   ├── ~~_FinalData20210913.fst~~
│   │   ├── ~~_FinalData20210913.rds~~
│   │   ├── DataExtractionByCountry.xlsx
│   │   ├── Emergency imaginary words v2.xlsx
│   │   ├── Emergency imaginary words.xlsx
│   │   └── FullCorpusMetadata_revised.xlsx
│
├── 📁 RR/
│   ├── dfmat_digital.RDS
│   ├── dfmat_nodigital.RDS
│   ├── toks_digital.RDS
│   ├── toks_nodigital_other.RDS
│   ├── toks_nodigital_uk.RDS
│   └── toks_nodigital_us.RDS
```

## ⚠️ Note on Omitted Files

Files shown in ~~strikethrough~~ are **excluded** from the downloadable replication archive due to copyright or licensing restrictions. These include sensitive or proprietary datasets. For access, please contact the corresponding author.
