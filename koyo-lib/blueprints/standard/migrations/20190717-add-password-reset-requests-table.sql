#lang north

-- @revision: efed79200bf19e497ce82c46ae7c7999
-- @parent: 2b39dc065de3cd5ebb612e2d80f00d8c
-- @description: Adds the password_reset_requests table.
-- @up {
create table password_reset_requests(
  user_id integer not null references users(id) unique,
  ip_address text not null,
  user_agent text not null,
  token text not null,
  expires_at timestamptz not null default current_timestamp
);
-- }

-- @down {
drop table password_reset_requests;
-- }
