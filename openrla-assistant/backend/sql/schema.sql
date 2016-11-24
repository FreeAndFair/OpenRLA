create table if not exists election (
  id      integer  primary key,
  created datetime not null
                   default current_timestamp,
  title   text     not null,
  date    datetime not null,
  active  boolean  unique
                   check (active = 1)
);

create table if not exists contest (
  id          integer  primary key,
  created     datetime not null
                       default current_timestamp,
  external_id text     not null,
  description text     not null,
  num_ranks   integer  not null,
  vote_for    integer  not null
);

create table if not exists election_contests (
  created     datetime not null
                       default current_timestamp,
  election_id integer  not null
                       references election (id),
  contest_id  integer  not null
                       references contest (id)
);

create table if not exists candidate (
  id          integer  primary key,
  created     datetime not null
                       default current_timestamp,
  external_id text     not null,
  contest_id  text     not null
                       references contest (id),
  description text     not null,
  type        text     not null
);

create table if not exists manifest (
  id        integer  primary key,
  created   datetime not null
                     default current_timestamp,
  vendor    text     not null
                     check (
                          vendor = "dominion"
                       or vendor = "freeandfair"
                     ),
  type      text     not null,
  src_path  text     not null,
  file_path text  -- Allow null for two-step insert
);

create table if not exists ballot_image (
  id        integer  primary key,
  created   datetime not null
                     default current_timestamp,
  src_path  text     not null,
  file_path text  -- Allow null for two-step insert
);
