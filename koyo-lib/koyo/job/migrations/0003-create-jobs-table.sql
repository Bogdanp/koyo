CREATE TABLE koyo_jobs(
  id BIGSERIAL PRIMARY KEY,
  queue TEXT NOT NULL,
  job TEXT NOT NULL,
  arguments BYTEA NOT NULL,
  status koyo_job_status NOT NULL DEFAULT 'ready',
  priority INTEGER NOT NULL DEFAULT 0,
  attempts INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  scheduled_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  started_at TIMESTAMPTZ,
  worker_id BIGINT
) WITH (fillfactor=70);