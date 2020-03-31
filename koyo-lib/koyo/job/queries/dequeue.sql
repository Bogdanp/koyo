UPDATE
  koyo_jobs
SET
  status = 'running',
  attempts = job.attempts + 1,
  started_at = CURRENT_TIMESTAMP,
  worker_id = $1
FROM (
  SELECT
    *
  FROM
    koyo_jobs
  WHERE
    queue = $2 AND
    status = 'ready' AND
    scheduled_at <= CURRENT_TIMESTAMP
  ORDER BY
    priority ASC
  LIMIT $3
  FOR UPDATE SKIP LOCKED
) job
WHERE
  koyo_jobs.id = job.id
RETURNING
  koyo_jobs.id,
  koyo_jobs.queue,
  koyo_jobs.job,
  koyo_jobs.arguments,
  koyo_jobs.attempts
