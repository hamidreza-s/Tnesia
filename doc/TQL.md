TQL
======

Tnesia Query Language is a query language for manipulating time-series data which is stored in Tnesia.

Proposal
-----

Syntax
---

**Select**

```sql
SELECT all | record_keys()
FROM timeline()
[ WHERE
   [ [ AND ] conditions() ]
   [ [ AND ] SINCE datetime() TILL datetime() ]
   [ [ AND ] ORDER order() ]
   [ [ AND ] LIMIT limit() ] ]
```

**Insert**

```sql
INSERT INTO timeline() record_keys()
RECORDS record_values()
```

**Delete**

```sql
DELETE FROM timeline()
WHEN record_time()
```

Types
---

```sql
timeline() :: 'string()'
record_keys() :: {'string()', ...}
record_values() :: {'string()', ...}
record_time() :: 'string()'
datetime() :: 'string(YYYY-MM-DD HH:MI:SS)'
order() :: 'asc' | 'des'
limit() :: 'integer()'
conditions() :: condition() AND condition()
condition() :: 'string()' comparator() 'string()'
comparator() :: == | != | > | >= | < | <=
```