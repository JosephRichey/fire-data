# Main tables

select * from test.test_table;

select * from information_schema.columns
where table_name = 'test_table';

DROP TABLE IF EXISTS test.firefighter;
CREATE TABLE test.firefighter (
	id int PRIMARY KEY AUTO_INCREMENT,
    first_name varchar(255),
    last_name varchar(255),
    full_name varchar(511),
    start_date text,
    trainer boolean,
    officer boolean,
    active_status boolean,
    company_id 
);

DROP TABLE IF EXISTS test.apparatus;
CREATE TABLE test.apparatus (
	apparatus_id INT PRIMARY KEY AUTO_INCREMENT,
    apparatus_name varchar(255)
);

# Training Tables
DROP TABLE IF EXISTS test.training;
CREATE TABLE test.training (
	training_id int PRIMARY KEY AUTO_INCREMENT,
    training_type varchar(255),
    training_topic varchar(255),
    training_description varchar(1000),
    training_start_time text,
    training_end_time text,
    training_trainer int,
    training_delete text
);

DROP TABLE IF EXISTS test.attendance;
CREATE TABLE test.attendance (
	attendance_id int PRIMARY KEY AUTO_INCREMENT,
    firefighter_id int NOT NULL,
    training_id int NOT NULL,
    check_in text,
    check_out text,
    auto_checkout boolean,
    credit boolean
);

# Incident tables
DROP TABLE IF EXISTS test.incident;
CREATE TABLE test.incident (
	incident_id varchar(255) PRIMARY KEY,
    incident_dispatch_time varchar(255),
    incident_end_time varchar(255),
    incident_address varchar(255),
    incident_dispatch_reason varchar(255),
    incident_ems_units bool,
    incident_fire_units bool,
    incident_wildland_units bool,
    incident_area varchar(255),
    incident_canceled bool,
    incident_dropped bool,
    incident_notes varchar(1000),
    incident_finalized bool
);

DROP TABLE IF EXISTS test.firefighter_incident;
CREATE TABLE test.firefighter_incident (
	id INT PRIMARY KEY AUTO_INCREMENT,
	incident_id VARCHAR(255),
    firefighter_id INT,
    time_adjustment float,
    FOREIGN KEY (incident_id) REFERENCES test.incident(incident_id) ON UPDATE CASCADE,
    foreign key (firefighter_id) REFERENCES test.firefighter(firefighter_id)
);

DROP TABLE IF EXISTS test.apparatus_incident;
CREATE TABLE test.apparatus_incident (
	id INT PRIMARY KEY AUTO_INCREMENT,
    incident_id VARCHAR(255),
    apparatus_id INT,
    time_adjustment float,
    foreign key (incident_id) REFERENCES test.incident(incident_id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES test.apparatus(apparatus_id)
);

DROP TABLE IF EXISTS test.firefighter_apparatus;
CREATE TABLE test.firefighter_apparatus (
	id INT PRIMARY KEY AUTO_INCREMENT,
    incident_id VARCHAR(255),
    firefighter_id INT,
    apparatus_id INT,
    foreign key (incident_id) REFERENCES test.incident(incident_id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES test.apparatus(apparatus_id),
    foreign key (firefighter_id) references test.firefighter(firefighter_id)
);


# Inventory tables
DROP TABLE IF EXISTS test.inventory_item;
CREATE TABLE test.inventory_item (
	item_id int PRIMARY KEY,
    item_type varchar(255),
    item_sub_type varchar(255),
    item_description varchar(1000)
);

DROP TABLE IF EXISTS test.inventory_level;
CREATE TABLE test.inventory_level (
	item_id int PRIMARY KEY,
    item_count int
);


