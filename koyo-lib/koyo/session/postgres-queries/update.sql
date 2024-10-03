INSERT INTO koyo_session_data(
  id, key, data
) VALUES (
  $1, $2, $3
) ON CONFLICT (
  id, key
) DO UPDATE SET
  data = $3;
