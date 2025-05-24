# For fields where the settings are stable, define a static list (things like training categories)
# More reactve fiels (like firefighter) will be handled in the server.

box::use(
  dplyr[...],
)

box::use(
  ./functions,
  ./app_data,
)

# Pull in training categories and topics

