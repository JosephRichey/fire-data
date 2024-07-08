# Generate Sample Data
# Set the start and end dates for the year 2023
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-13")

# Generate a sequence of dates for all Tuesdays in 2023
tuesday_dates <- seq.Date(from = start_date, to = end_date, by = "1 day")[weekdays(seq.Date(from = start_date, to = end_date, by = "1 day")) == "Tuesday"]

# Format the dates in yyyy-mm-dd format
formatted_dates <- format(tuesday_dates, "%Y-%m-%d")

Trainings <- data.frame(training_id = c(1:50),
                        training_type = sample(c("EMS", "Fire", "Wildland"), 50, TRUE, prob = c(0.4,0.4,0.2)),
                        topic = NA,
                        training_length = rep(2,50),
                        description = "We trained",
                        date = formatted_dates,
                        delete = FALSE
) |> 
  mutate(topic = if_else(training_type == "EMS", "Medical", "Fire Supression"))


Roster <- data.frame(firefighter_id = c(1:6),
                     first_name = c("Bill S.", "Ted Theodore", "Frodo", "Sam", "Jack", "Davy"),
                     last_name = c("Preston", "Logan", "Baggins", "Gamgee", "Sparrow", "Jones"),
                     start_date = as.Date(rep("2000-01-01",6)),
                     active_status = TRUE
                     )

Attendance <- data.frame(firefighter_id = sample(c(1:6), 250, TRUE, c(0.18,0.18,0.18,0.18,0.18,0.1)),
                         training_id = sample(c(1:50), 250, TRUE))

board |> pin_write(Trainings, "trainings", "rds")
board |> pin_write(Roster, "roster", "rds")
board |> pin_write(Attendance, "attendance", "rds")
