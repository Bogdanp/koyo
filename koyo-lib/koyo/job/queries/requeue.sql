WITH
  jobs AS (
    UPDATE
      koyo_jobs
    SET
      status = 'ready',
      attempts = attempts - 1,
      started_at = NULL,
      worker_id = NULL
    WHERE
      id = ANY($1)
    RETURNING
      id
  )
SELECT
  id, PG_NOTIFY('koyo_jobs', j.id::TEXT)
FROM
  jobs j;
