USE cfddb;
DROP PROCEDURE IF EXISTS sp_auto_checkout;

DELIMITER //
CREATE PROCEDURE sp_auto_checkout ()
BEGIN

-- Once a day, checkout everyone who is still checked in. Set end time as schedule end time of training.
UPDATE cfddb.attendance atten
LEFT JOIN cfddb.training train
	ON atten.training_id = train.training_id
SET check_out = training_end_time, 
auto_checkout = TRUE
WHERE CHECK_OUT IS NULL;


END//
DELIMITER ;