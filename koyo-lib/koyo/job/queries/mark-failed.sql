UPDATE koyo_jobs
SET
  status = 'failed'
WHERE
  id = $1;
