
library(dplyr)
library(cranlogs)
library(purrr)

# identify last available day of data
date_avail <- cran_downloads("dplyr", "last-day") |> pull(date)

# test small batch
# few_pkgs <- c("dplyr", "tidyr")
pkgs_by_day <- cran_downloads(packages = few_pkgs,
      from = paste(date_avail - 364), to = paste(date_avail))

# for fun... see if it matches what I calculate
# top100 <- cran_top_downloads(when = 'last-month', count = 100)

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
avail_pkgs <- available.packages("https://cran.rstudio.com/src/contrib")[,1]

all_pkgs_by_day <- map_dfr(avail_pkgs, ~ cran_downloads(packages = .x,
                           from = date_avail - 89, to = date_avail))

length(unique(all_pkgs_by_day$date)) # 90 days
length(unique(all_pkgs_by_day$package)) # 19,715
head(all_pkgs_by_day) # preview

# save workspace since it took a while to run
# save.image(file='./community_meetings/2023-06-27/downloads.RData')

all_date_summary <- all_pkgs_by_day %>%
  group_by(package) %>%
  summarize(start_date = min(date),
            end_date = max(date),
            downloads = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(downloads)

head(all_date_summary) # preview

all_date_summary %>% nrow # confirmed same: # 19,715


# Distribution
hist(all_date_summary$downloads[0:19000], breaks = 1000)

# Only 13 pkgs have had zero downloads
all_date_summary %>%
  filter(downloads == 0) %>%
  nrow

# 1,780 pkgs have had 10k downloads or more
all_date_summary %>%
  filter(downloads >= 10000) %>%
  nrow


################################
# 3,000 Most downloaded
top <- all_date_summary %>%
  top_n(3000) %>%
  arrange()

head(top) # preview - lowest (of top 1000) has 33,955 downloads. 120,806 for top 500.
tail(top) # preview - the most has 9,810,263 downloads
View(top) # browse

# 1,000 pkgs is ~ the top 5% of cran from a downloads perspective
top_pkgs <- top$package

# to run these packages through the {riskassessment} app would take ~12 seconds
# per pkg, or about 3 hours, 20 minutes to load them all into the database.
# However, there may be other things happening there, and it calculates the
# scores using weights, which we know has a current bug attached to it. So,
# we'll calculate them locally.
  
  
    # Quick test run on some popular pkgs
    library(dplyr)
    library(riskmetric)
    packageVersion("riskmetric")
    
    # > [1] ‘0.2.1’
    
    assessed <- c("dplyr", "tidyr") %>%
      pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
      as_tibble() %>%
      pkg_assess()
    
    initial_scoring <- assessed %>% pkg_score()
    # initial_scoring$pkg_score %>% round(2)
    
    # riskmetric doesn't appear to be picking up certain metrics
    # so we'll set their weights to zero here.
    metric_scores <- initial_scoring %>%
      select(-c(package, version, pkg_ref)) %>%
      t

# define weights
metric_weights <- ifelse(is.na(metric_scores[,1]), 0, 1)


# Now that we have agreement on what weights to use for the "pkg_cran_remote"
# reference, do it for all the packages, scoring the pkg explicitly setting NAs
# with a new weight = 0

# Note there are caveats to how this was calculated (using the pkg_ref I used)

st <- Sys.time()
scored_top3k <- top_pkgs %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess() %>%
  pkg_score(weights = metric_weights)
end <- Sys.time()
end - st # took 33 minutes for 1k pkgs

# save workspace since it took a while to run
# save.image(file='./community_meetings/2023-06-27/downloads_and_scores.RData') # top 1k
# save.image(file='./community_meetings/2023-06-27/downloads_and_scores_top3k.RData') # top 3k

# load a workspace
# load('./community_meetings/2023-06-27/downloads_and_scores_top3k.RData')

# Choose to analyze a data frame
# scored <- scored_top1k # top 1k
# scored <- scored_top3k # top 3k
# scored <- scored_pv # scored pharmaverse
# scored <- scored_tv # scored tidyverse

scored %>%
  pull("pkg_score") %>%
  round(2)

scores_n_dwnlds <- scored %>%
  left_join(all_date_summary) %>%
  arrange(desc(downloads)) %>%
  mutate(rank_dwnlds = row_number())  %>%
  arrange(pkg_score) %>%
  mutate(rank_score = row_number())  %>%
  select(package, rank_dwnlds, rank_score, downloads, pkg_score, everything())

head(scores_n_dwnlds)
View(scores_n_dwnlds)

# top 3k
hist(scores_n_dwnlds %>% pull(pkg_score),
     main = "{riskmetric} Scores for 3k Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")

scores_n_dwnlds %>%
  filter(pkg_score >= .5)

# top 2k
hist(scores_n_dwnlds %>% top_n(2000, downloads) %>% pull(pkg_score),
     main = "{riskmetric} Scores for 2k Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")

scores_n_dwnlds %>%
  top_n(2000, downloads) %>%
  filter(pkg_score >= .5)

# top 1k
hist(scores_n_dwnlds %>% top_n(1000, downloads) %>% pull(pkg_score),
     main = "{riskmetric} Scores for 1k Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")

scores_n_dwnlds %>%
  top_n(1000, downloads) %>%
  filter(pkg_score >= .5)

# top 500
hist(scores_n_dwnlds %>% top_n(500, downloads) %>% pull(pkg_score),
     main = "{riskmetric} Scores for 500 Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")

scores_n_dwnlds %>%
  top_n(500, downloads) %>%
  filter(pkg_score >= .5)

# top 250
hist(scores_n_dwnlds %>% top_n(250, downloads) %>% pull(pkg_score),
     main = "{riskmetric} Scores for 250 Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")

scores_n_dwnlds %>%
  top_n(250, downloads) %>%
  filter(pkg_score >= .5)

# top 100
hist(scores_n_dwnlds %>% top_n(100, downloads) %>% pull(pkg_score),
     main = "{riskmetric} Scores for 100 Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")

scores_n_dwnlds %>%
  top_n(100, downloads) %>%
  filter(pkg_score >= .5)







####### Score all of CRAN

# Now that we have agreement on what weights to use for the "pkg_cran_remote"
# reference, do it for all the packages, scoring the pkg explicitly setting NAs
# with a new weight = 0

# Note there are caviats to how this was calculated (using the pkg_ref I used)

st <- Sys.time()
scored_cran <- avail_pkgs %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess() %>%
  pkg_score(weights = metric_weights)
end <- Sys.time()
end - st # took 33 minutes for 1k pkgs

# save workspace since it took a while to run
save.image(file='./community_meetings/2023-06-27/downloads_and_scores_cran.RData')



scored %>%
  pull("pkg_score") %>%
  round(2)

scores_n_dwnlds <- scored_cran %>%
  left_join(all_date_summary) %>%
  arrange(desc(downloads)) %>%
  mutate(rank_dwnlds = row_number())  %>%
  arrange(pkg_score) %>%
  mutate(rank_score = row_number())  %>%
  select(package, rank_dwnlds, rank_score, downloads, pkg_score, everything())

head(scores_n_dwnlds)
View(scores_n_dwnlds)

hist(scores_n_dwnlds %>% pull(pkg_score),
     main = "{riskmetric} Scores for Pkgs on CRAN",
     xlab = "Risk score")


hist(scores_n_dwnlds %>% top_n(10000, downloads) %>% pull(pkg_score),
     main = "{riskmetric} Scores for 10k Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")


hist(scores_n_dwnlds %>% top_n(5000, downloads) %>% pull(pkg_score),
     main = "{riskmetric} Scores for 5k Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")


hist(scores_n_dwnlds %>% top_n(3000, downloads) %>% pull(pkg_score),
     main = "{riskmetric} Scores for 3k Most Downloaded Pkgs on CRAN",
     xlab = "Risk score")







#########################
# Can I easily identify the tidyverse pkgs and extract their scores?


tidyverse_pkgs <- tidyverse::tidyverse_packages()

st <- Sys.time()
scored_tv <- tidyverse_pkgs %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess() %>%
  pkg_score(weights = metric_weights)
end <- Sys.time()
end - st


scores_n_dwnlds_tv <- scored_tv %>%
  left_join(all_date_summary) %>%
  arrange(desc(downloads)) %>%
  mutate(rank_dwnlds = row_number())  %>%
  arrange(pkg_score) %>%
  mutate(rank_score = row_number())  %>%
  select(package, rank_dwnlds, rank_score, downloads, pkg_score, everything())

# save workspace since it took a while to run
save.image(file='./community_meetings/2023-06-27/downloads_and_scores_top3k.RData') # top 3k

hist(scores_n_dwnlds_tv %>% pull(pkg_score),
     main = "{riskmetric} Scores for {tidyverse} Pkgs on CRAN",
     xlab = "Risk score")

scores_n_dwnlds_tv %>%
  filter(pkg_score >= .45)








#########################
# Can I easily identify the pharmaverse pkgs and extract their scores?
pharmaverse <- c("admiral","tidytlg","tern","teal.slice","admiralvaccine","scda","scda.2022",
"sdtmcecks","ggsurvfit","logrx","tfrmt","riskassessment","xportr","rtables",
"teal.widgets","teal.logger","teal.data","admiralonco","admiralophtha",
"teal.code","teal.transform","teal","teal.reporter","riskmetric","covtracer",
"envsetup","pkglite","tidyCDISC","datacutr","covr","metatools","metacore",
"Tplyr","visR","synthetic.cdisc.data","valtools","respectables","pharmaRTF")


st <- Sys.time()
scored_ <- pharmaverse %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess() %>%
  pkg_score(weights = metric_weights)
end <- Sys.time()
end - st 

scored_pv <- scored_ %>% filter(!is.na(version))


scores_n_dwnlds_pv <- scored_pv %>%
  left_join(all_date_summary) %>%
  arrange(desc(downloads)) %>%
  mutate(rank_dwnlds = row_number())  %>%
  arrange(pkg_score) %>%
  mutate(rank_score = row_number())  %>%
  select(package, rank_dwnlds, rank_score, downloads, pkg_score, everything())

hist(scores_n_dwnlds_pv %>% pull(pkg_score),
     main = "{riskmetric} Scores for {pharmaverse} Pkgs on CRAN",
     xlab = "Risk score")

scores_n_dwnlds_pv %>%
  filter(pkg_score >= .45)

# save workspace since it took a while to run
save.image(file='./community_meetings/2023-06-27/downloads_and_scores_top3k.RData') # top 3k







#########################
##### Create a combined density plot

library(ggplot2)

# gather groups into one dataframe
grp_dat <- scores_n_dwnlds %>%
  anti_join(scores_n_dwnlds_tv, by = "package") %>%
  anti_join(scores_n_dwnlds_pv, by = "package") %>%
  # recalc now that those pkgs are removed
  arrange(desc(downloads)) %>%
  mutate(rank_dwnlds = row_number())  %>%
  arrange(pkg_score) %>%
  mutate(rank_score = row_number()) %>%
  mutate(Group = case_when(
    rank_dwnlds > 10000 ~ "Top 10k to 19,175",
    rank_dwnlds > 5000 ~ "Top 5k to 10k",
    rank_dwnlds > 500 ~ "Top 500 to 5k",
    rank_dwnlds > 100 ~ "Top 100 to 500",
    TRUE ~ "Top 100 Dwnlds"
  )) %>%
  union(scores_n_dwnlds_tv %>% mutate(Group = "{tidyverse}")) %>%
  union(scores_n_dwnlds_pv %>% mutate(Group = "{pharmaverse}")) %>%
  mutate(Group = factor(Group, levels = rev(c(
    "Top 10k to 19,175", "Top 5k to 10k", "Top 500 to 5k", "Top 100 to 500",
    "Top 100 Dwnlds", "{pharmaverse}", "{tidyverse}"
  )))) %>%
  select(package, Group, rank_dwnlds, rank_score, downloads, pkg_score, everything())
  

# cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

# Basic density plot in ggplot2
ggplot2::ggplot(grp_dat, ggplot2::aes(x = pkg_score, fill = Group)) +
  ggplot2::geom_density(alpha = 0.4) + 
  ggplot2::ggtitle("{riskmetric} Scores by Package Group") +
  ggplot2::xlab("{riskmetric} Package Score")

  


# prime groups
prime <- grp_dat %>%
  filter(Group %in% c('{tidyverse}', '{pharmaverse}') | rank_dwnlds <= 500)
ggplot2::ggplot(prime, ggplot2::aes(x = pkg_score, fill = Group)) +
  ggplot2::geom_density(alpha = 0.4) + 
  ggplot2::ggtitle("{riskmetric} Scores: Reputable Package Groups or Freqently Downloaded") +
  ggplot2::xlab("{riskmetric} Package Score")

prime %>% filter(pkg_score >= .5) # 86 pkgs > .5


# losers
losers <- grp_dat %>%
  filter(!(Group %in% c('{tidyverse}', '{pharmaverse}') | rank_dwnlds <= 500))
ggplot2::ggplot(losers, ggplot2::aes(x = pkg_score, fill = Group)) +
  ggplot2::geom_density(alpha = 0.4) + 
  ggplot2::ggtitle("{riskmetric} Scores: Groups with Fewer Downloads") +
  ggplot2::xlab("{riskmetric} Package Score")


losers %>% filter(pkg_score < .5) # 8,539 pkgs < .5


