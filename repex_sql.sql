with c as (
  select m.id, tstamp, value from data d
  join metadata m on m.id=d.meta_id
  join terms t on t.id=m.term_id
  where t.id = 15
),
mean as (
select avg(value) as idx from c

),
idx as (
  select m.id, mean
  from metadata m
  natural join mean
)
select distinct * from idx order by mean
