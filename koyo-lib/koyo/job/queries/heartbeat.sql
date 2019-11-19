UPDATE koyo_workers
SET heartbeat = CURRENT_TIMESTAMP
WHERE id = $1;
