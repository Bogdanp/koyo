WITH
  workers AS (
    DELETE FROM
      koyo_workers
    WHERE
      id = $1
    RETURNING
      id
  )
UPDATE
  koyo_jobs
SET
  status = 'ready'
FROM workers w
WHERE
  status = 'running' AND
  w.id = worker_id;
