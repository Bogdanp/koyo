CREATE DOMAIN koyo_job_status AS TEXT CHECK(VALUE IN ('ready', 'running', 'done', 'failed'));
