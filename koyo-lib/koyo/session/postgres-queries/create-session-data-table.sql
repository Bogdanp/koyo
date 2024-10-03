CREATE UNLOGGED TABLE IF NOT EXISTS koyo_session_data(
  id UUID REFERENCES koyo_sessions(id) ON DELETE CASCADE,
  key TEXT NOT NULL,
  data BYTEA,

  CONSTRAINT koyo_session_data_pk PRIMARY KEY (id, key)
);
