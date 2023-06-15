# # IMPORT DATA -------------------------------------------------------------

# add the fish_ids to the metadata list
meta <- read_excel("data/pilot/7 days/06062023 salbutamol pilot fish metadata.xlsx",
                   sheet = "7 days") %>%

  # tidy up colnames
  mutate(fish_id = as.character(fish_id),
         treatment = factor(treatment, levels = c("untreated",
                                                  "20 µM Salbutamol",
                                                  "100 µM Salbutamol")),
         sex = as.factor(sex),
         ymazePosition = as.character(ymazePosition),
         genotype = factor(genotype, levels = c("het", "hom"))

  )

# define the filenames
file_list <- list.files("data/pilot/7 days/raw_data/distances",
           pattern = "*.csv",
           full.names = TRUE)

# make an object containing all the raw data files.

df <- tibble(data_file_distances = file_list) %>%  #Import data as a tibble with nested lists
  mutate(data = map(file_list, ~ read_csv(.x )),

         # cleanup the filename to match what is in the meta sheet
         data_file_distances = str_remove(data_file_distances, pattern = "data/pilot/7 days/raw_data/distances/"))


# converto to a tibble rather than a list
df %<>%
  unnest(data)

# cleanup the Arena zone columns to match the meta column
df <- df %>%
  gather(key = "temp", value = "value", starts_with("A")) %>%
  mutate(ymazePosition = str_remove(temp, pattern = "_Z[:digit:]"),
         ymazePosition = str_remove(ymazePosition, pattern = "A"),
         zone = str_remove(temp, pattern = "A*._"),
         zone = str_remove(zone, pattern = "Z")
  ) %>%
  dplyr::select(-temp, -TIME) %>% # unselect unnessary and problematic columns
  left_join(meta,
            by = join_by(data_file_distances, ymazePosition)
            ) %>%
  pivot_wider( names_from = ENDPOINT,
               values_from = value)











