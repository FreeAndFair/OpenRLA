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
  contest_id  integer  not null
                       references contest (id),
  description text     not null,
  type        text     not null
);

create table if not exists election_contest_outcome (
  created      datetime not null default current_timestamp,
  election_id  integer  not null references election (id),
  contest_id   integer  not null references contest (id),
  candidate_id integer  not null references candidate (id),
  share        real     not null
                        check (
                              0.0 <= share
                          and share <= 1.0
                        ),
  primary key (election_id, contest_id, candidate_id)
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

create table if not exists ballot (
  id        integer  primary key,
  created   datetime not null
                     default current_timestamp,
  src_path  text     not null,
  file_path text  -- Allow null for two-step insert
);

create table if not exists election_ballot (
  election_id  integer not null references election (id),
  ballot       integer not null references ballot (id)

);

create table if not exists audit (
  id          integer  primary key,
  created     datetime not null
                       default current_timestamp,
  election_id integer  not null
                       references election (id),
  date        datetime not null,
  risk_limit  real     not null
                       check (
                             0.0 < risk_limit
                         and risk_limit < 1.0
                       ),
  active      boolean  unique
                       check (active = 1)
);

create table if not exists audit_mark (
  audit_sample_id integer not null references audit_sample (id),
  contest_id      integer not null references contest (id),
  candidate_id    integer not null references candidate (id)
);

create table if not exists audit_contest (
  audit_id   integer not null references audit (id),
  contest_id integer not null references contest (id)
);

create table if not exists audit_current_sample (
  audit_id  integer primary key
                    references audit (id),
  sample_id integer not null
                    references audit_sample (id)
);

create table if not exists audit_sample (
  id        integer not null primary key,
  audit_id  integer not null references audit (id),
  ballot_id integer not null references ballot (id)
);

PRAGMA foreign_keys = ON;
