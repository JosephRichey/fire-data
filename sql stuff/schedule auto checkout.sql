use cfddb;

DROP EVENT IF EXISTS auto_checkout

DELIMITER //

CREATE EVENT auto_checkout
ON SCHEDULE
    EVERY 1 DAY
    STARTS '2024-02-07 07:00:00'
    COMMENT 'Run auto checkout sp.'
DO
    CALL sp_auto_checkout();

//

DELIMITER ;