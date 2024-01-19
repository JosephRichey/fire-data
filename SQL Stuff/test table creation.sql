DROP TABLE IF EXISTS cfddb.training_TEST;
CREATE TABLE cfddb.training_TEST (
	training_id int PRIMARY KEY AUTO_INCREMENT,
    training_type varchar(255),
    training_topic varchar(255),
    training_description varchar(1000),
    training_date text,
    training_start_time text,
    training_end_time text,
    training_officer int,
    training_delete text
);

DROP TABLE IF EXISTS cfddb.firefighter_TEST;
CREATE TABLE cfddb.firefighter_TEST (
	firefighter_id int PRIMARY KEY AUTO_INCREMENT,
    firefighter_first_name varchar(255),
    firefighter_last_name varchar(255),
    firefighter_full_name varchar(511),
    firefighter_start_date text,
    firefighter_officer boolean,
    firefighter_deactive_date text
);

DROP TABLE IF EXISTS cfddb.attendance_TEST;
CREATE TABLE cfddb.attendance_TEST (
	attendance_id int PRIMARY KEY AUTO_INCREMENT,
    firefighter_id int NOT NULL,
    training_id int NOT NULL,
    check_in text,
    check_out text
);