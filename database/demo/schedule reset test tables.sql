use cfddb;

DROP EVENT IF EXISTS reset_test_tables

DELIMITER //

CREATE EVENT reset_test_tables
ON SCHEDULE
    EVERY 60 MINUTE
    STARTS '2024-07-07 07:00:00'
    COMMENT 'Reset test tables.'
DO
    CALL sp_reset_test_tables();

//

DELIMITER ;