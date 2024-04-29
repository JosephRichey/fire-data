USE cfddb;
DROP PROCEDURE IF EXISTS sp_auto_checkout;

DELIMITER //
CREATE PROCEDURE sp_auto_checkout ()
BEGIN

-- Find all records that require auto checkouts.

-- This currently assumes there will only ever be one training per day, and this sp is ran every day.
SELECT 
    CONCAT(training_date, ' ', training_end_time) AS check_out
INTO @check_out FROM
    cfddb.training-- _TEST
WHERE
    training_id = (
		SELECT distinct training_id
        FROM
            cfddb.attendance-- _TEST
        WHERE
            check_out IS NULL);

-- Update all null checkouts to checkout time.
-- For CFD, this will run at 8:05 when training is always over.
-- For dynamic use, create a python script to run this when a training finishes.
UPDATE cfddb.attendance-- _TEST 
SET 
    check_out = @check_out,
    auto_checkout = TRUE
WHERE
    check_out IS NULL;


END//
DELIMITER ;