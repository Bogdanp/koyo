CREATE INDEX
  koyo_jobs_dequeue_idx
ON
  koyo_jobs(queue, status, priority ASC, scheduled_at ASC)
WHERE
  status = 'ready';
