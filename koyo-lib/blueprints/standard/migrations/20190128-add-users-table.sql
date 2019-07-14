#lang north

-- @revision: 2b39dc065de3cd5ebb612e2d80f00d8c
-- @description: Creates the "users" table.
-- @up {
create table users(
  id serial primary key,
  username text not null unique,
  password_hash text not null,
  is_verified boolean not null default false,
  verification_code text not null,
  created_at timestamptz not null default current_timestamp,
  updated_at timestamptz not null default current_timestamp,

  constraint users_username_is_lowercase check(username = lower(username))
);
-- }

-- @down {
drop table users;
-- }
