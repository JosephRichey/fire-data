CREATE TABLE cfddb.training (
	training_id int PRIMARY KEY,
    training_type varchar(255),
    training_topic varchar(255),
    training_length double,
    training_description varchar(1000),
    training_date date,
    training_delete date
);

CREATE TABLE cfddb.firefighter (
	firefighter_id int PRIMARY KEY,
    firefighter_first_name varchar(255),
    firefighter_last_name varchar(255),
    firefighter_full_name varchar(511),
    firefighter_start_date date,
    firefighter_term_date boolean
);

-- Creat table using R to match datatype for datetime
-- CREATE TABLE cfddb.attendance (
-- 	attendance_id int PRIMARY KEY,
--     firefighter_id int NOT NULL,
--     training_id int NOT NULL,
--     check_in datetime,
--     check_out datetime
-- );

