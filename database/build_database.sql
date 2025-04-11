# Create and set database for specific department.
#CREATE DATABASE crabapple;
USE crabapple;

DROP TABLE IF EXISTS firefighter_contact;
DROP TABLE IF EXISTS firefighter_status_history;
DROP TABLE IF EXISTS certification;
DROP TABLE IF EXISTS certification_type;
DROP TABLE IF EXISTS setting;
#DROP TABLE IF EXISTS user_log;
DROP TABLE IF EXISTS equipment_log;
DROP TABLE IF EXISTS equipment;
DROP TABLE IF EXISTS equipment_type;
DROP TABLE IF EXISTS equipment_check_compliance;
DROP TABLE IF EXISTS attendance;
DROP TABLE IF EXISTS training;
DROP TABLE IF EXISTS firefighter_response;
DROP TABLE IF EXISTS apparatus_response;
DROP TABLE IF EXISTS firefighter_apparatus;
DROP TABLE IF EXISTS chain_of_command;
DROP TABLE IF EXISTS firefighter;
DROP TABLE IF EXISTS apparatus;
DROP TABLE IF EXISTS incident_unit;
DROP TABLE IF EXISTS unit;
DROP TABLE IF EXISTS response;
DROP TABLE IF EXISTS incident;
DROP TABLE IF EXISTS company;

# The company table stores the different crews or companies a firefighter can be assigned.
CREATE TABLE company (
	id int PRIMARY KEY AUTO_INCREMENT,
    company_name varchar(255)
    # No options to expire company. No history is retained.
);

# The firefighter is one of the most basic tables, storing details about each firefighter.
# The active status provides a way to easily filter if they can be used to create new records.
CREATE TABLE firefighter (
	id int PRIMARY KEY AUTO_INCREMENT,
    full_name varchar(511),
    start_date date,
    trainer boolean,
    officer boolean,
    active_status boolean,
    # No deletion of firefighters, only deactivation.
    company_id int,
    firefighter_role varchar(255),
    foreign key (company_id) references company(id) ON DELETE SET NULL
);

# This table helps structure the org chart.
CREATE TABLE chain_of_command (
	id INT PRIMARY KEY auto_increment,
    firefighter_id INT,
    supervisor_id INT,
    foreign key (firefighter_id) references firefighter(id) ON DELETE SET NULL,
    foreign key (supervisor_id) references firefighter(id) ON DELETE SET NULL
);

# This table stores contact information for each firefighter.
CREATE TABLE firefighter_contact (
	id int PRIMARY KEY auto_increment,
    firefighter_id int,
	street_address_1 varchar(255),
    street_address_2 varchar(255),
    city varchar(255),
    state char(2),
    zip_code varchar(10),
    phone_number varchar(15),
    email_address varchar(255),
    foreign key (firefighter_id) references firefighter(id)
);

# This table provides a history of all firefighters. They can be activated and 
# deactivated over time, and then calculate total tenure.
CREATE TABLE firefighter_status_history (
	id INT PRIMARY KEY auto_increment,
	firefighter_id INT,
    inactive_start_date date,
    inactive_end_date date,
    foreign key (firefighter_id) references firefighter(id)
);

# This holds the different certifications a firefighter can hold.
CREATE TABLE certification_type (
	id INT PRIMARY KEY AUTO_INCREMENT,
    certification_name VARCHAR(255),
    lead_time INT,
    lead_time_unit VARCHAR(255),
    renew_time INT,
    renew_time_unit VARCHAR(255),
    certification_expire date  
    # expire the type, not the certification
    # This allows existing certs to still be reported on, but prevents new ones from being added.
);

# This is the individiaul instance of a certification type, tied to a firefighter.
CREATE TABLE certification (
	id INT PRIMARY KEY AUTO_INCREMENT,
    type_id INT,
    firefighter_id INT,
    expiration_date date,
    foreign key (type_id) references certification_type(id),
    foreign key (firefighter_id) references firefighter(id)
);

# This table stores all the data that can be set to customize different aspects of the platform.
CREATE TABLE setting (
	id INT PRIMARY KEY AUTO_INCREMENT,
    major_setting_key VARCHAR(255),
    minor_setting_key VARCHAR(255),
    setting_value VARCHAR(255)
);

#CREATE TABLE user_log (#
#	user_log_id INT PRIMARY KEY AUTO_INCREMENT,
#    log_in datetime,
#    log_out datetime # Will need to test if the app crashes if I can still get a reliable log out
#);

# This shows all the apparatus that are active and can be used.
CREATE TABLE apparatus (
	id INT PRIMARY KEY AUTO_INCREMENT,
    apparatus_name varchar(255),
    apparatus_expire date
    # This allows existing apparatus to still be reported on, but prevents new records from being added.
);

# This table is used to track the type of response (Fire, EMS, etc.) Customizable to each department. 
CREATE TABLE unit (
	id INT PRIMARY KEY AUTO_INCREMENT,
    unit_type VARCHAR(255),
    unit_expire date
    # This allows existing to still be reported on, but prevents new records from being added.
);

# This stores "types" of equipment.
CREATE TABLE equipment_type (
	id INT PRIMARY KEY AUTO_INCREMENT,
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
    # This allows existing to still be reported on, but prevents new records from being added.
);

# These are instances of different equipment types.
CREATE TABLE equipment (
	id INT PRIMARY KEY AUTO_INCREMENT,
    equipment_name VARCHAR(255),
    equipment_type_id INT,
    firefighter_id INT,
    apparatus_id INT,
    next_check_date date,
    expiration_date date,
    snooze_expires date,
    expire_equipment date,
    foreign key (equipment_type_id) references equipment_type(id),
    foreign key (firefighter_id) references firefighter(id),
    foreign key (apparatus_id) references apparatus(id)
);

# This provides a record of who checked what piece of equipment and when it occured.
CREATE TABLE equipment_log (
	id INT PRIMARY KEY AUTO_INCREMENT,
    checked_by_id INT,
    equipment_id INT,
    check_date_time datetime,
    foreign key (checked_by_id) references firefighter(id),
    foreign key (equipment_id) references equipment(id)
);

# This provides a snapshot of the compliance percentage over time.
CREATE TABLE equipment_check_compliance (
	id INT PRIMARY KEY AUTO_INCREMENT,
    compliance_date date,
    check_compliance float,
    expiration_compliance float,
    overall_compliance float
);

# Training Table - stores all the trainings.
CREATE TABLE training (
	id int PRIMARY KEY AUTO_INCREMENT,
    training_type varchar(255),
    topic varchar(255),
    training_description varchar(1000),
    start_time datetime,
    end_time datetime,
    credit_hours float,
    trainer int,
    training_expire date,
    foreign key (trainer) references firefighter(id)
);

# Stores attendance of the trainings.
CREATE TABLE attendance (
	id int PRIMARY KEY AUTO_INCREMENT,
    firefighter_id int NOT NULL,
    training_id int NOT NULL,
    check_in datetime,
    check_out datetime,
    auto_checkout boolean,
    credit boolean,
    excused boolean,
    foreign key (firefighter_id) references firefighter(id),
    foreign key (training_id) references training(id)
);

# Incident tables
# The top level is an incident. 0 - many responses can be added to an incident.
CREATE TABLE incident (
	id INT PRIMARY KEY AUTO_INCREMENT,
	cad_identifier varchar(255),
    incident_start datetime,
    incident_end datetime,
    address varchar(255),
    dispatch_code varchar(255), 
    area varchar(255),
    canceled bool,
    dropped bool,
    finalized bool,
    incident_expire date
    # Retain for audit
);

# Many responses can be tied to an incident.
CREATE TABLE response (
	id INT PRIMARY KEY AUTO_INCREMENT,
    incident_id INT,
    response_start datetime,
    response_end datetime,
    notes text,
    response_expire date,
    # Retain for audit
    foreign key (incident_id) REFERENCES incident(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

# This tracks what type of units respond to an incident.
# (Sometimes, the type of apparatus doesn't matter because it's just used to transport individuals)
CREATE TABLE incident_unit (
	id INT PRIMARY KEY AUTO_INCREMENT,
    incident_id INT NOT NULL,
    unit_type_id INT NOT NULL,
    FOREIGN KEY (incident_id) REFERENCES incident(id) ON DELETE RESTRICT ON UPDATE CASCADE,
    FOREIGN KEY (unit_type_id) REFERENCES unit(id) ON DELETE RESTRICT ON UPDATE CASCADE
    
);

# This ties the firefighter to a response.
CREATE TABLE firefighter_response (
	id INT PRIMARY KEY AUTO_INCREMENT,
	response_id INT,
    firefighter_id INT,
    time_adjustment float,
    FOREIGN KEY (response_id) REFERENCES response(id) ON UPDATE CASCADE,
    foreign key (firefighter_id) REFERENCES firefighter(id)
);

# This ties an apparatus to a response.
CREATE TABLE apparatus_response (
	id INT PRIMARY KEY AUTO_INCREMENT,
    response_id INT,
    apparatus_id INT,
    foreign key (response_id) REFERENCES response(id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES apparatus(id)
);

# This ties a firefighter to an apparatus.
CREATE TABLE firefighter_apparatus (
	id INT PRIMARY KEY AUTO_INCREMENT,
    response_id INT,
    firefighter_id INT,
    apparatus_id INT,
    foreign key (response_id) REFERENCES response(id) ON UPDATE CASCADE,
    FOREIGN KEY (apparatus_id) REFERENCES apparatus(id),
    foreign key (firefighter_id) references firefighter(id)
);


    

