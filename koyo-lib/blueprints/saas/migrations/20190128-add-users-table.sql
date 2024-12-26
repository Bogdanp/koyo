#lang north

-- @revision: 2b39dc065de3cd5ebb612e2d80f00d8c
-- @description: Creates the "users" table.
-- @up {
CREATE TABLE users(
  id SERIAL PRIMARY KEY,
  roles TEXT[] NOT NULL DEFAULT '{user}',
  username TEXT NOT NULL UNIQUE,
  password_hash TEXT NOT NULL,
  is_verified BOOLEAN NOT NULL DEFAULT FALSE,
  verification_code TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT users_username_is_lowercase CHECK(username = LOWER(username))
);
-- }

-- @down {
DROP TABLE users;
-- }
