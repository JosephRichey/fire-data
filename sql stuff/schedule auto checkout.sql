use cfddb;

DELIMITER //

CREATE EVENT weekly_auto_checkout
ON SCHEDULE
    EVERY 1 DAY
    STARTS '2024-02-03 20:06:00'
    COMMENT 'Weekly auto checkout event on Tuesday at 20:06:00'
DO
    CALL auto_checkout();

//

DELIMITER ;
