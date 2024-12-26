#lang north

-- @revision: e55f1f4883753d2465b7f3e4c7c5715e
-- @parent: efed79200bf19e497ce82c46ae7c7999
-- @description: Adds the "workspaces", "workspace_members" and "workspace_invites" tables.
-- @up {
CREATE TABLE workspaces(
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  slug TEXT NOT NULL UNIQUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
-- }
-- @up {
CREATE TABLE workspace_members(
  workspace_id INTEGER NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  role TEXT NOT NULL,

  CONSTRAINT workspace_members_pk PRIMARY KEY (workspace_id, user_id)
);
-- }
-- @up {
CREATE TABLE workspace_invites(
  id SERIAL PRIMARY KEY,
  workspace_id INTEGER NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  email TEXT NOT NULL,
  token TEXT NOT NULL,
  role TEXT NOT NULL
);
-- }

-- @down {
DROP TABLE workspace_invites;
-- }
-- @down {
DROP TABLE workspace_members;
-- }
-- @down {
DROP TABLE workspaces;
-- }
