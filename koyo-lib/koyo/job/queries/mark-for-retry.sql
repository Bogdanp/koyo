WITH
  job AS (
    UPDATE
      koyo_jobs
    SET
      status = 'ready',
      scheduled_at = $2,
      attempts = attempts + 1
    WHERE
      id = $1
    RETURNING
      id
  )
SELECT
  PG_NOTIFY('koyo_jobs', j.id::TEXT)
FROM
  job j;
