CREATE UNLOGGED TABLE IF NOT EXISTS koyo_sessions(
  id UUID PRIMARY KEY,
  deadline TIMESTAMPTZ NOT NULL
);