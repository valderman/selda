<div class="pane" id="left">

## The Tutorial of Selda

As Selda uses a lot of fancy type magic to achieve a safe yet flexible
programming model, figuring out how to use it from type signatures alone
can be hard. While the types keep queries nice and safe, the type errors
when something goes wrong can be a bit daunting.

This tutorial aims to give the reader a thorough intuition of how Selda works
and why, to make those type errors more manageable and to help the reader avoid
making them in the first place.

This tutorial assumes that the reader is already familiar with
relational databases and can write and understand simple SQL queries.
It is structured around a series of progressively more advanced Selda examples,
which are dissected and discussed in depth, concept by concept.

<span class="biglink">
<a title="Squadala, we're off!" href="tutorial/ch1-example-explained">Get started!</a>
</span>

</div>

<div class="pane" id="right">

### Chapters

1. [An Example, Explained](tutorial/ch1-example-explained).
   Covers the definition and creation of tables, inserting values,
   and performing simple queries.
2. [Destructive Operations](tutorial/ch2-destructive-operations).
   Covers deletes, updates, and more advanced flavours of insert.
3. [Advanced Queries](tutorial/ch3-advanced-queries).
   Covers ad hoc creation of rows from within queries, and other misc.
   neat query tricks.
4. **Joins and Aggregates** (*TBD*).
   Covers the various types of joins and aggregate queries supported by Selda.
5. **Constraints and Indexes** (*TBD*).
   Covers more advanced schema definitions, including foreign keys, indexes,
   and other constraints.
6. **Migrations and Prepared Statements** (*TBD*).
   Covers how to migrate database tables from one schema to another, and how to improve
   performance through prepared statements.

</div>
