# ChangeLog

## lkn_core 0.2.0

* **[Breaking Change]** Puppeteers are now notified when a puppet enters
  or leaves the instance they are registered to. As a consequence,
  Puppeteers implementations have to implement new callbacks.
* **[Breaking Change]** Entities can now be stopped in a clean way. As a
  consequence, Entities implementations have to implement new callbacks.
