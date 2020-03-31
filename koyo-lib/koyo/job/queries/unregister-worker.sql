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
  koyo_jobs j
SET
  status = 'ready'
FROM
  workers w
WHERE
      j.status = 'running'
  AND j.worker_id = w.id;
