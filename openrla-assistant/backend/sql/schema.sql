create table if not exists election (
  id     integer  primary key,
  title  text     not null,
  date   datetime not null,
  active boolean  unique
                  check (active = 1)
);

create table if not exists contest (
  id          integer primary key,
  external_id text    not null,
  description text    not null,
  num_ranks   integer not null,
  vote_for    integer not null
);

create table if not exists election_contests (
  election_id integer not null
                      references election (id),
  contest_id  integer not null
                      references contest (id)
);

create table if not exists candidate (
  id          integer primary key,
  external_id text    not null,
  contest_id  text    not null
                      references contest (id),
  description text    not null,
  type        text    not null
);
