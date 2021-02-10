library(tidyverse)
library(readxl)
library(pbmcapply)

source(here::here("R/utilities.R"))

data_files <- list.files(here::here("data-raw"),
                         pattern="^1.*xls",
                         recursive = TRUE,
                         full.names = TRUE)

# read_psykinematix is slow, so we read it in in parallel to reduce
# processing time.
data <- pbmclapply(data_files,
                   read_psykinematix,
                   mc.cores = parallel::detectCores())
names(data) <-  basename(data_files)

df <- lapply(data, extract_table, table = "trials") %>%
  bind_rows(.id = "file") %>%
  type_convert() %>%
  rename(task = table) %>%
  as_tibble() %>%
  separate(file, c("subject_id", "wave_code", NA)) %>%
  mutate(project_id = "S2C",
         task = str_remove_all(task, "trial_|trial|^x|_|triak"),
         task = str_remove_all(task, "trail|^test"),
         wave_code = parse_number(wave_code)) %>%
  group_by(subject_id, project_id, wave_code) %>%
  mutate(paradigm_counter = row_number()) %>%
  ungroup() %>%
  select(subject_id, project_id, wave_code, paradigm_counter, everything()) %>%
  mutate(across(where(is.numeric), round, digits = 4))

df %>%
  select(1:3, task) %>%
  distinct() %>%
  filter(!grepl("^[1-6]|retest", task)) %>%
  write_tsv(here::here("data/trial_names_fixing.tsv"))

df %>%
  filter(grepl("^[1-6]|retest", task)) %>%
  mutate(filename = glue::glue("{here::here('data/psykinematix_timeseries')}/{subject_id}_{tolower(project_id)}_0{wave_code}_mic_timeseries.tsv")) %>%
  group_split(filename) %>%
  map(~ write_tsv(select(., -filename), unique(.$filename)))


# Task summaries ----
# Still a repeated table, but now with a single line
# per task, so subject x wave x task number
coh <- lapply(data, extract_table, table = "coherence") %>%
  bind_rows(.id = "file") %>%
  as_tibble() %>%
  rename(task = table,
         coherence_mean = mean,
         coherence_std = std) %>%
  as_tibble() %>%
  separate(file, c("subject_id", "wave_code", NA)) %>%
  mutate(project_id = "S2C",
         task = str_remove_all(task, "trial_|trial|^x|_|triak"),
         task = str_remove_all(task, "trail|^test"),
         wave_code = parse_number(wave_code))

df_summary <- df %>%
  group_by(subject_id, project_id, wave_code, task) %>%
  summarise(
    reversals = max(number_of_reversals),
    threshold = last(threshold),
    scoring_percent = last(scoring_percent),
    hits = sum(ifelse(response == "Hit", 1, 0)),
    misses = sum(ifelse(response != "Hit", 1, 0)),
    n_trials = n(),
    rt_mean = mean(rt),
    rt_se = mean(rt)/sqrt(n()),
    accuracy_mean = mean(ifelse(response == "Hit", 1, 0)),
    accuracy_se = mean(ifelse(response == "Hit", 1, 0))/sqrt(n()),
  ) %>%
  left_join(coh) %>%
  filter(grepl("^[1-6]|retest", task))
write_tables(df_summary,
             here::here("data/psykinematix_task_summaries"))


jsonlite::write_json(data.frame(table_type = "repeated"), pretty = TRUE,
                     here::here("data/psykinematix_task_summaries/_noas.json"))
