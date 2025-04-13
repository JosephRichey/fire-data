# For fields where the settings are stable, define a static list (things like training categories)
# More reactve fiels (like firefighter) will be handled in the server.

box::use(
  dplyr[...],
)

box::use(
  ./functions,
  ./app_data,
)

# Define training categories and types
training_categories <- functions$GetSetting('training', group = 'training_category')

GetTrainingTopics <- function(category) {
  # Get the training topics for a specific category
  training_topics <- functions$GetSetting('training', key = category)
  return(training_topics)
}

#' @export
Training_Dictionary <- purrr::set_names(training_categories) |>
  purrr::map(~ GetTrainingTopics(.))
