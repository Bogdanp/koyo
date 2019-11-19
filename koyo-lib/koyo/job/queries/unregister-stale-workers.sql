WITH
  stale_workers AS (
    DELETE FROM
      koyo_workers
    WHERE
      heartbeat < CURRENT_TIMESTAMP - '1 hour'::INTERVAL
    RETURNING
      id
  )
UPDATE
  koyo_jobs
SET
  status = 'ready'
FROM stale_workers sw
WHERE sw.id = worker_id;
