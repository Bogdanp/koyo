UPDATE
  koyo_jobs
SET
  status = 'done'
WHERE
  id = $1;
