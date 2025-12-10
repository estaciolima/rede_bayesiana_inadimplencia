# Ler o arquivo
set.seed(123)

# Preparar o ambiente renv
install.packages("renv")
renv::init()

renv::snapshot()

renv::restore()

df <- data.table::fread("data/size/accepted_2007_to_2018Q4.csv")
df_sample <- df[sample(.N, 250000)]

arrow::write_parquet(
  x = df_sample,
  sink = "data/amostra250k.parquet"
)

rm(df, df_sample)

