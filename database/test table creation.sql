DROP TABLE IF EXISTS cfddb.training_TEST;
CREATE TABLE cfddb.training_TEST (
	training_id int PRIMARY KEY AUTO_INCREMENT,
    training_type varchar(255),
    training_topic varchar(255),
    training_description varchar(1000),
    training_start_time text,
    training_end_time text,
    training_trainer int,
    training_delete text
);

DROP TABLE IF EXISTS cfddb.firefighter_TEST;
CREATE TABLE cfddb.firefighter_TEST (
	firefighter_id int PRIMARY KEY AUTO_INCREMENT,
    firefighter_first_name varchar(255),
    firefighter_last_name varchar(255),
    firefighter_full_name varchar(511),
    firefighter_start_date text,
    firefighter_trainer boolean,
    firefighter_officer boolean,
    firefighter_deactive_date text
);

DROP TABLE IF EXISTS cfddb.attendance_TEST;
CREATE TABLE cfddb.attendance_TEST (
	attendance_id int PRIMARY KEY AUTO_INCREMENT,
    firefighter_id int NOT NULL,
    training_id int NOT NULL,
    check_in text,
    check_out text,
    auto_checkout boolean,
    credit boolean
);

DROP TABLE IF EXISTS cfddb.incident_TEST;
CREATE TABLE cfddb.incident_TEST (
	incident_id varchar(255) PRIMARY KEY,
    incident_dispatch_time varchar(255),
    incident_end_time varchar(255),
    incident_address varchar(255),
    incident_dispatch_reason varchar(255),
    incident_ems_units bool,
    incident_fire_units bool,
    incident_wildland_units bool,
    incident_area varchar(255),
    incident_notes varchar(1000)
);

DROP TABLE IF EXISTS cfddb.apparatus_TEST;
CREATE TABLE cfddb.apparatus_TEST (
	apparatus_id INT PRIMARY KEY AUTO_INCREMENT,
    apparatus_name varchar(255)
);

DROP TABLE IF EXISTS cfddb.apparatus_firefighter_incident_TEST;
CREATE TABLE cfddb.apparatus_firefighter_incident_TEST (
	id INT PRIMARY KEY AUTO_INCREMENT,
	incident_id INT,
    apparatus_id INT,
    firefighter_id INT,
    FOREIGN KEY (incident_id) REFERENCES cfddb.incident(incident_id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES cfddb.apparatus(apparatus_id),
    foreign key (firefighter_id) REFERENCES cfddb.firefighter
);