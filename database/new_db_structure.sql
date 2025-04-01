# Main tables

# Create and set database for specific department.
#CREATE DATABASE crabapple;
USE crabapple;

DROP TABLE IF EXISTS firefighter_contact;
DROP TABLE IF EXISTS firefighter_status_history;
DROP TABLE IF EXISTS certification;
DROP TABLE IF EXISTS certification_type;
DROP TABLE IF EXISTS setting;
DROP TABLE IF EXISTS user_log;
DROP TABLE IF EXISTS equipment_log;
DROP TABLE IF EXISTS equipment;
DROP TABLE IF EXISTS equipment_type;
DROP TABLE IF EXISTS equipment_check_compliance;
DROP TABLE IF EXISTS attendance;
DROP TABLE IF EXISTS training;
DROP TABLE IF EXISTS firefighter_incident;
DROP TABLE IF EXISTS apparatus_incident;
DROP TABLE IF EXISTS firefighter_apparatus;
DROP TABLE IF EXISTS chain_of_command;
DROP TABLE IF EXISTS firefighter;
DROP TABLE IF EXISTS apparatus;
DROP TABLE IF EXISTS incident_xref;
DROP TABLE IF EXISTS incident;
DROP TABLE IF EXISTS company;

CREATE TABLE company (
	company_id int PRIMARY KEY AUTO_INCREMENT,
    company_name varchar(255)
);

CREATE TABLE firefighter (
	firefighter_id int PRIMARY KEY AUTO_INCREMENT,
    full_name varchar(511),
    start_date date,
    trainer boolean,
    officer boolean,
    active_status boolean,
    company_id int,
    firefighter_role varchar(255),
    foreign key (company_id) references company(company_id) ON DELETE SET NULL
);

CREATE TABLE chain_of_command (
	chain_of_command_id INT PRIMARY KEY auto_increment,
    firefighter_id INT,
    supervisor_id INT,
    foreign key (firefighter_id) references firefighter(firefighter_id) ON DELETE SET NULL,
    foreign key (supervisor_id) references firefighter(firefighter_id) ON DELETE SET NULL
);

CREATE TABLE firefighter_contact (
	contact_id int PRIMARY KEY auto_increment,
    firefighter_id int,
	street_address_1 varchar(255),
    street_address_2 varchar(255),
    city varchar(255),
    state char(2),
    zip_code varchar(10),
    phone_number varchar(15),
    email_address varchar(255),
    foreign key (firefighter_id) references firefighter(firefighter_id)
);

CREATE TABLE firefighter_status_history (
	status_history_id INT PRIMARY KEY auto_increment,
	firefighter_id INT,
    inactive_start_date date,
    inactive_end_date date,
    foreign key (firefighter_id) references firefighter(firefighter_id)
);

CREATE TABLE certification_type (
	certification_type_id INT PRIMARY KEY AUTO_INCREMENT,
    certification_name VARCHAR(255),
    lead_time INT,
    lead_time_unit VARCHAR(255),
    renew_time INT,
    renew_time_unit VARCHAR(255),
    certification_expire date  #expire the type, not the certification
);

CREATE TABLE certification (
	certification_id INT PRIMARY KEY AUTO_INCREMENT,
    type_id INT,
    firefighter_id INT,
    expiration_date date,
    foreign key (type_id) references certification_type(certification_type_id),
    foreign key (firefighter_id) references firefighter(firefighter_id)
);

CREATE TABLE setting (
	setting_id INT PRIMARY KEY AUTO_INCREMENT,
    major_setting_key VARCHAR(255),
    minor_setting_key VARCHAR(255),
    setting_value VARCHAR(255)
);

CREATE TABLE user_log (
	user_log_id INT PRIMARY KEY AUTO_INCREMENT,
    log_in datetime,
    log_out datetime # Will need to test if the app crashes if I can still get a reliable log out
);

CREATE TABLE apparatus (
	apparatus_id INT PRIMARY KEY AUTO_INCREMENT,
    apparatus_name varchar(255)
);

CREATE TABLE equipment_type (
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
    equipment_type_expire date  #expire the type, not the equipment
);

CREATE TABLE equipment (
	equipment_id INT PRIMARY KEY AUTO_INCREMENT,
    equipment_name VARCHAR(255),
    equipment_type_id INT,
    firefighter_id INT,
    next_check_date date,
    expiration_date date,
    snooze_expires date,
    expire_equipment date,
    foreign key (equipment_type_id) references equipment_type(equipment_type_id),
    foreign key (firefighter_id) references firefighter(firefighter_id)
);

CREATE TABLE equipment_log (
	equipment_log_id INT PRIMARY KEY AUTO_INCREMENT,
    checked_by_id INT,
    equipment_id INT,
    check_date_time datetime,
    foreign key (checked_by_id) references firefighter(firefighter_id),
    foreign key (equipment_id) references equipment(equipment_id)
);

CREATE TABLE equipment_check_compliance (
	equipment_check_compliance_id INT PRIMARY KEY AUTO_INCREMENT,
    compliance_date_time datetime,
    compliance float
);

# Training Tables
CREATE TABLE training (
	training_id int PRIMARY KEY AUTO_INCREMENT,
    training_type varchar(255),
    topic varchar(255),
    training_description varchar(1000),
    start_time datetime,
    end_time datetime,
    credit_hours float,
    trainer int,
    training_expire date,
    foreign key (trainer) references firefighter(firefighter_id)
);


CREATE TABLE attendance (
	attendance_id int PRIMARY KEY AUTO_INCREMENT,
    firefighter_id int NOT NULL,
    training_id int NOT NULL,
    check_in datetime,
    check_out datetime,
    auto_checkout boolean,
    credit boolean,
    excused boolean,
    foreign key (firefighter_id) references firefighter(firefighter_id),
    foreign key (training_id) references training(training_id)
    
);

# Incident tables
CREATE TABLE incident (
	id INT PRIMARY KEY AUTO_INCREMENT,
	incident_id varchar(255),
    dispatch_time datetime,
    end_time datetime,
    address varchar(255),
    dispatch_code varchar(255), 
    ems_units bool,
    fire_units bool,
    wildland_units bool,
    area varchar(255),
    canceled bool,
    dropped bool,
    notes text,
    finalized bool,
    incident_expire date
);

CREATE TABLE response (
	id INT PRIMARY KEY AUTO_INCREMENT,
    dispatch_time datetime,
    end_time datetime,
    address varchar(255),
    dispatch_code varchar(255), 
    ems_units bool,
    fire_units bool,
    wildland_units bool,
    area varchar(255),
    canceled bool,
    dropped bool,
    notes text,
    finalized bool,
    incident_expire date
);

ALTER TABLE incident ADD INDEX (incident_id);

CREATE TABLE incident_xref (
	id INT PRIMARY KEY AUTO_INCREMENT,
    primary_incident varchar(255),
    additional_incident varchar(255),
    FOREIGN KEY (primary_incident) references incident(incident_id) ON UPDATE CASCADE,
    #FIXME Check that this logic works and it actually needs to cascade
    FOREIGN KEY (additional_incident) references incident(incident_id) ON UPDATE CASCADE
);

CREATE TABLE firefighter_incident (
	firefighter_incident_id INT PRIMARY KEY AUTO_INCREMENT,
	incident_id VARCHAR(255),
    firefighter_id INT,
    time_adjustment float,
    FOREIGN KEY (incident_id) REFERENCES incident(incident_id) ON UPDATE CASCADE,
    foreign key (firefighter_id) REFERENCES firefighter(firefighter_id)
);

CREATE TABLE apparatus_incident (
	apparatus_incident_id INT PRIMARY KEY AUTO_INCREMENT,
    incident_id VARCHAR(255),
    apparatus_id INT,
    foreign key (incident_id) REFERENCES incident(incident_id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES apparatus(apparatus_id)
);

CREATE TABLE firefighter_apparatus (
	firefighter_apparatus_id INT PRIMARY KEY AUTO_INCREMENT,
    incident_id VARCHAR(255),
    firefighter_id INT,
    apparatus_id INT,
    foreign key (incident_id) REFERENCES incident(incident_id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES apparatus(apparatus_id),
    foreign key (firefighter_id) references test.firefighter(firefighter_id)
);

select * from incident;
select * from incident_xref;

select * from incident inc1
left join incident_xref xref
	on inc1.incident_id = xref.primary_incident
left join incident inc2
	on xref.additional_incident = inc2.incident_id
;

select * from incident
where incident_id = 12345 
	or incident_id in (select additional_incident from incident_xref where primary_incident = 12345);

