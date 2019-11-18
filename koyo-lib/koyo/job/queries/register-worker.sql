INSERT INTO koyo_workers(pid, hostname) VALUES($1, $2) RETURNING id;
