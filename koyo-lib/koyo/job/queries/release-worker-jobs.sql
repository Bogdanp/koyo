UPDATE koyo_jobs
SET
  status = 'ready'
WHERE
  status = 'running' AND
  worker_id = $1;
