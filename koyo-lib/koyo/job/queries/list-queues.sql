SELECT
  queue,
  COALESCE(SUM(1) FILTER (WHERE status = 'ready'), 0) AS total_ready,
  COALESCE(SUM(1) FILTER (WHERE status = 'running'), 0) AS total_running,
  COALESCE(SUM(1) FILTER (WHERE status = 'done'), 0) AS total_done,
  COALESCE(SUM(1) FILTER (WHERE status = 'failed'), 0) AS total_failed
FROM
  koyo_jobs
GROUP BY 1;
