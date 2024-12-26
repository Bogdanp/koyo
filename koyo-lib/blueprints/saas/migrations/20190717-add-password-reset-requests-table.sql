#lang north

-- @revision: efed79200bf19e497ce82c46ae7c7999
-- @parent: 2b39dc065de3cd5ebb612e2d80f00d8c
-- @description: Adds the password_reset_requests table.
-- @up {
CREATE TABLE password_reset_requests(
  user_id INTEGER NOT NULL REFERENCES users(id) UNIQUE,
  ip_address TEXT NOT NULL,
  user_agent TEXT NOT NULL,
  token TEXT NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }

-- @down {
DROP TABLE password_reset_requests;
-- }
