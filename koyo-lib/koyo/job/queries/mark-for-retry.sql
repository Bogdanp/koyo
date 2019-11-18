WITH
  job AS (
    UPDATE jobs
    SET
      status = 'ready',
      scheduled_at = $2
    WHERE id = $1
    RETURNING
      id
  )
SELECT
  PG_NOTIFY('koyo_jobs', j.id::TEXT)
FROM
  job j;
