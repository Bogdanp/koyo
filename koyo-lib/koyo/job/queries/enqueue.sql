WITH
  job AS (
    INSERT INTO
      koyo_jobs(queue, job, arguments, priority, scheduled_at)
    VALUES
      ($1, $2, $3, $4, DEFAULT)
    RETURNING
      id
  )
SELECT
  id, PG_NOTIFY('koyo_jobs', j.id::TEXT)
FROM
  job j;
