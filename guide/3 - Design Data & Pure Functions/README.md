# 3 - Design Data & Pure Functions

PureScript has a best-in-class type system with powerful features like algebraic data types, generics, row types, type classes. Let's put this type system to work to help us represent the the data our application will use to support our use cases.

We'll sometimes use primitive data like strings, booleans, and numbers to represent values in our application. But most data we'll work with have meaning in our application beyond being a boolean or a string. Types allow us to imbue values with compiler-enforced meaning and perform double-duty as a form of documentation.

I like to break data down into three distinct kinds:

- **Entities** are data with a persistent identity, like a *User* or an *Article*. In other words, two users are the same user only if they have the same identity, and a user can change value over time and be considered the same user so long as it has preserved its identity. They'll often be aggregates of other values.
- **Values** are data without an identity, like an *Email*, *Username*, or *List*. Two emails are the same if their value is the same. If an email is changed, it should be considered a new value.
- **Lifecycles** are data representing the various states that a value or entity can be in at different points in time. Lifecycles will usually be data types that contain values or entities  and might represent transitions among several types.

If you're familiar with type-driven design and best practices for ensuring types capture valid states in your domain while disallowing invalid states, then feel free to skip ahead to see how I've designed data specific to the Conduit application. If you're new to this process, however, start with *Principles For Designing Data*.

# Table of Contents

[1 - Principles For Designing Data](./1-Principles-For-Designing-Data-efcdf83a-d352-409e-b730-2f26e5662a15.md)

[2 - Entities & Values](./2-Entities-Values-3c7969c9-be1c-44a2-b5d4-bfee2af6ea0e.md)

[3 - Processes & Lifecycles](./3-Processes-Lifecycles-0448fcb5-84d2-4b74-a385-a5ae4cd92a93.md)