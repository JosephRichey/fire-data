# Main tables
DROP TABLE IF EXISTS test.firefighter_contact;
DROP TABLE IF EXISTS test.firefighter_status_history;
DROP TABLE IF EXISTS test.certification;
DROP TABLE IF EXISTS test.certification_type;
DROP TABLE IF EXISTS test.setting;
DROP TABLE IF EXISTS test.user_log;
DROP TABLE IF EXISTS test.equipment_log;
DROP TABLE IF EXISTS test.equipment;
DROP TABLE IF EXISTS test.equipment_type;
DROP TABLE IF EXISTS test.attendance;
DROP TABLE IF EXISTS test.training;
DROP TABLE IF EXISTS test.firefighter_incident;
DROP TABLE IF EXISTS test.apparatus_incident;
DROP TABLE IF EXISTS test.firefighter_apparatus;
DROP TABLE IF EXISTS test.chain_of_command;
DROP TABLE IF EXISTS test.firefighter;
DROP TABLE IF EXISTS test.apparatus;
DROP TABLE IF EXISTS test.incident_xref;
DROP TABLE IF EXISTS test.incident;
DROP TABLE IF EXISTS test.company;

CREATE TABLE test.company (
	company_id int PRIMARY KEY AUTO_INCREMENT,
    company_name varchar(255)
);

CREATE TABLE test.firefighter (
	firefighter_id int PRIMARY KEY AUTO_INCREMENT,
    full_name varchar(511),
    start_date text,
    trainer boolean,
    officer boolean,
    active_status boolean,
    company_id int,
    firefighter_role varchar(255),
    foreign key (company_id) references test.company(company_id) ON DELETE SET NULL
);

CREATE TABLE test.chain_of_command (
	chain_of_command_id INT PRIMARY KEY auto_increment,
    firefighter_id INT,
    supervisor_id INT,
    foreign key (firefighter_id) references test.firefighter(firefighter_id) ON DELETE SET NULL,
    foreign key (supervisor_id) references test.firefighter(firefighter_id) ON DELETE SET NULL
);

CREATE TABLE test.firefighter_contact (
	contact_id int PRIMARY KEY auto_increment,
    firefighter_id int,
	street_address_1 varchar(255),
    street_address_2 varchar(255),
    city varchar(255),
    state char(2),
    zip_code varchar(10),
    phone_number varchar(15),
    email_address varchar(255),
    foreign key (firefighter_id) references test.firefighter(firefighter_id)
);

CREATE TABLE test.firefighter_status_history (
	status_history_id INT PRIMARY KEY auto_increment,
	firefighter_id INT,
    inactive_start_date varchar(15),
    inactive_end_date varchar(15),
    foreign key (firefighter_id) references test.firefighter(firefighter_id)
);

CREATE TABLE test.certification_type (
	certification_type_id INT PRIMARY KEY AUTO_INCREMENT,
    certification_name VARCHAR(255),
    lead_time INT,
    lead_time_unit VARCHAR(255),
    renew_time INT,
    renew_time_unit VARCHAR(255),
    certification_expire VARCHAR(15)  #expire the type, not the certification
);

CREATE TABLE test.certification (
	certification_id INT PRIMARY KEY AUTO_INCREMENT,
    type_id INT,
    firefighter_id INT,
    expiration_date VARCHAR(15),
    snooze_expires VARCHAR(15),
    foreign key (type_id) references test.certification_type(certification_type_id),
    foreign key (firefighter_id) references test.firefighter(firefighter_id)
);

CREATE TABLE test.setting (
	setting_id INT PRIMARY KEY AUTO_INCREMENT,
    major_setting_key VARCHAR(255),
    minor_setting_key VARCHAR(255),
    setting_value VARCHAR(255)
);

CREATE TABLE test.user_log (
	user_log_id INT PRIMARY KEY AUTO_INCREMENT,
    log_in VARCHAR(255),
    log_out VARCHAR(255) # Will need to test if the app crashes if I can still get a reliable log out
);

CREATE TABLE test.apparatus (
	apparatus_id INT PRIMARY KEY AUTO_INCREMENT,
    apparatus_name varchar(255)
);

CREATE TABLE test.equipment_type (
	equipment_type_id INT PRIMARY KEY AUTO_INCREMENT,
    equipment_type VARCHAR(255),
    check_lead_time INT,
    check_lead_time_unit VARCHAR(255),
    check_time INT,
    check_time_unit VARCHAR(255),
    expire_lead_time INT,
    expire_lead_time_unit VARCHAR(255),
    expire_time INT,
    expire_time_unit VARCHAR(255),
    equipment_type_expire VARCHAR(15)  #expire the type, not the equipment
    
);

CREATE TABLE test.equipment (
	equipment_id INT PRIMARY KEY AUTO_INCREMENT,
    equipment_name VARCHAR(255),
    type_id INT,
    firefighter_id INT,
    next_check_date VARCHAR(15),
    expiration_date VARCHAR(15),
    snooze_expires VARCHAR(15),
    expire_equipment VARCHAR(15),
    foreign key (type_id) references test.equipment_type(equipment_type_id),
    foreign key (firefighter_id) references test.firefighter(firefighter_id)
);

CREATE TABLE test.equipment_log (
	equipment_log_id INT PRIMARY KEY AUTO_INCREMENT,
    checked_by_id INT,
    equipment_id INT,
    check_date VARCHAR(15),
    foreign key (checked_by_id) references test.firefighter(firefighter_id),
    foreign key (equipment_id) references test.equipment(equipment_id)
);

# Training Tables
CREATE TABLE test.training (
	training_id int PRIMARY KEY AUTO_INCREMENT,
    training_type varchar(255),
    topic varchar(255),
    training_description varchar(1000),
    start_time text,
    end_time text,
    trainer int,
    training_delete text,
    foreign key (trainer) references test.firefighter(firefighter_id)
);

CREATE TABLE test.attendance (
	attendance_id int PRIMARY KEY AUTO_INCREMENT,
    firefighter_id int NOT NULL,
    training_id int NOT NULL,
    check_in text,
    check_out text,
    auto_checkout boolean,
    credit boolean,
    excused boolean,
    foreign key (firefighter_id) references test.firefighter(firefighter_id),
    foreign key (training_id) references test.training(training_id)
    
);

# Incident tables
CREATE TABLE test.incident (
	id INT PRIMARY KEY AUTO_INCREMENT,
	incident_id varchar(255),
    dispatch_time varchar(255),
    end_time varchar(255),
    address varchar(255),
    dispatch_reason varchar(255),
    ems_units bool,
    fire_units bool,
    wildland_units bool,
    area varchar(255),
    canceled bool,
    dropped bool,
    notes text,
    finalized bool
);

ALTER TABLE test.incident ADD INDEX (incident_id);


CREATE TABLE test.incident_xref (
	id INT PRIMARY KEY AUTO_INCREMENT,
    primary_incident varchar(255),
    additional_incident varchar(255),
    FOREIGN KEY (primary_incident) references test.incident(incident_id) ON UPDATE CASCADE,
    #FIXME Check that this logic works and it actually needs to cascade
    FOREIGN KEY (additional_incident) references test.incident(incident_id) ON UPDATE CASCADE
);

CREATE TABLE test.firefighter_incident (
	firefighter_incident_id INT PRIMARY KEY AUTO_INCREMENT,
	incident_id VARCHAR(255),
    firefighter_id INT,
    time_adjustment float,
    FOREIGN KEY (incident_id) REFERENCES test.incident(incident_id) ON UPDATE CASCADE,
    foreign key (firefighter_id) REFERENCES test.firefighter(firefighter_id)
);

CREATE TABLE test.apparatus_incident (
	apparatus_incident_id INT PRIMARY KEY AUTO_INCREMENT,
    incident_id VARCHAR(255),
    apparatus_id INT,
    time_adjustment float,
    foreign key (incident_id) REFERENCES test.incident(incident_id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES test.apparatus(apparatus_id)
);

CREATE TABLE test.firefighter_apparatus (
	firefighter_apparatus_id INT PRIMARY KEY AUTO_INCREMENT,
    incident_id VARCHAR(255),
    firefighter_id INT,
    apparatus_id INT,
    foreign key (incident_id) REFERENCES test.incident(incident_id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES test.apparatus(apparatus_id),
    foreign key (firefighter_id) references test.firefighter(firefighter_id)
);



