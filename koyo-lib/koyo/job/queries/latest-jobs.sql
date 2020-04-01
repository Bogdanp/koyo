SELECT
  id, queue, job, arguments, status
FROM
  koyo_jobs
WHERE
  $1 = -1 OR id < $1
ORDER BY id DESC
LIMIT 100
