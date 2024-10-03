INSERT INTO koyo_sessions(
  id, deadline
) VALUES (
  $1, CURRENT_TIMESTAMP + $2::INTERVAL
) ON CONFLICT (
  id
) DO UPDATE SET
  deadline = CURRENT_TIMESTAMP + $2::INTERVAL;
