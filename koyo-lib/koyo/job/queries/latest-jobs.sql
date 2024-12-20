SELECT
  id, queue, job, arguments, status, priority, attempts, created_at, scheduled_at, started_at, worker_id
FROM
  koyo_jobs
WHERE
  ($1::TEXT IS NULL OR queue = $1)
  AND ($2 = -1 OR id < $2)
  AND ($3::TEXT[] IS NULL OR status = ANY($3))
ORDER BY id DESC
LIMIT 100
