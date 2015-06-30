TQL
======

Tnesia Query Language is a query language for manipulating time-series data which is stored in Tnesia.

Proposal
-----

Syntax
---

**Select**

```sql
SELECT all | { field_name() }
FROM timeline()
[ SINCE datetime() ]
[ TILL datetime() ]
[ ORDER order() ]
[ LIMIT integer() ]
[ WHERE conditions() ]
```

**Insert**

```sql
INSERT INTO timeline()
RECORDS { record() }
```

**Delete**

```sql
DELETE FROM timeline()
WHEN record_time()
```

Types
---

```sql
{ any() } :: list()

timeline() :: 'string()'

record() :: field_name_value() [ , field_name_value() , ... ]
record_time() :: 'string()'

field_name_value() :: field_name() : field_value()
field_name() :: 'string()'
field_value() :: 'string()' | 'integer()'

datetime() :: YYYgY-MM-DD HH:MI:SS

conditions() :: condition() [ , conjunctive() condition() , ... ]
conjunctive() :: AND | OR
condition :: field_name() comparator() field_value()
comparator() :: == | != | > | >= | < | <=
```