# First task
df <- read.csv("Lab1/lab1_e1.csv")
# df <- read.csv("lab1_e1.csv")
fix_data <- function(df) {
  change <- function(x) {
    vec <- as.numeric(gsub(" ", "", x))
    ifelse(is.na(vec), x, vec)
  }
  data.frame(lapply(df, change))
}

new_df <- fix_data(df)
print(new_df)

# Second task
# load("Lab1/lab1_e2.Rdata")
load("lab1_e2.Rdata")
get_id <- function(df_lst) {
  tmp_df <- Reduce(function(x, y) merge(x, y, by = "id"), df_lst)
  data.frame(id = tmp_df[, 1], mean_temp = rowMeans(tmp_df[, 2:8]))
}

another_new_df <- get_id(all_data)
print(another_new_df)
