# Test data generation

df <- read.csv(here::here("SQL Stuff/test_data_generation.csv"),
               col.names = c('training_id',
                             'training_type',
                             'training_topic',
                             'training_description',
                             'training_date',
                             'training_start_time',
                             'training_end_time',
                             'training_officer',
                             'training_delete'
                             ),
               header = FALSE)

df |>
  mutate(insert_statement = paste0("(",
                                   training_id, ", ",
                                   training_type, ", ",
                                   training_topic, ", ",
                                   '"', training_description, '", ',
                                   '"', training_date, '", ',
                                   training_start_time, ", ",
                                   training_end_time, ", ",
                                   training_officer, ", ",
                                   training_delete, "),"
                                   )
         ) |>
  select(insert_statement) |>
  write.table(here::here("SQL Stuff/test_data_generation_insert_statement.txt"),
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE
              )
