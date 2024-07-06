box::use(
  testthat[...],
  DBI[dbConnect, dbGetQuery],
)
box::use(
  ../../app/logic/functions[FixColNames]
)

test_that("Fix Column Names functions works", {
  data.frame(
    training_id = 1,
    training_name = "Test Training"
  )
  
  FixColNames(data.frame(
    training_id = 1,
    training_name = "Test Training"
  ))
  
  FixColNames(data.frame(
    training_id = 1,
    training_name = "Test Training"
  )) |> 
    expect_is("data.frame")
  
  
  
})
