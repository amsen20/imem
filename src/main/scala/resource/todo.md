# Overall TODO List

- Make most fields and methods private, and classes final
- Prevent users from:
  - Creating `UnsafeRef`/`InternalRef`/`ImmutRef`/`MutRef` instances directly
  - Accessing the `value` field except through `read`/`write` methods
  - Leaking the `value` field
  - Modifying the `value` using `read` method (consider exclusive capabilities)
  - Copying Box, ImmutRef, or MutRef freely (consider dependent types)
