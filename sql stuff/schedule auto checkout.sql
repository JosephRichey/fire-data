use cfddb;

DELIMITER //

ALTER EVENT weekly_auto_checkout
ON SCHEDULE
    EVERY 1 DAY
    STARTS '2024-02-07 03:05:00'
    COMMENT 'Weekly auto checkout event on Tuesday at 20:05:00'
DO
    CALL auto_checkout();

//

DELIMITER ;