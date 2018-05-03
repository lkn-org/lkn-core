# ChangeLog

## lkn_core 0.4.3

* The Instance now proxifies all requests targeting Systems, to reduce the risk
  of race conditions while registering new Puppets.
* Bump several development dependencies

## lkn_core 0.4.2

* Rely on the Instance to notify the registered Puppeteers. We were using a
  Registry before, but the latter are not very tolerant with process crash.

## lkn_core 0.4.1

* Fix a minor error in the System module interface

## lkn_core 0.4.0

* **[Breaking Change]** It is no possible to send per-system options when
  registering a puppet
* **[Breaking Change]** Rename the `key` argument of the cast/call of a
  Puppeteer into the more straightforward `puppeteer_key`

## lkn_core 0.3.0

* When an instance was killed (once empty), its pool was crashing.
* **[Breaking Change]** Puppeteers can now be stopped in a clean way. As a
  consequence, Puppeteers implementation have to implement a new callback.
* **[Breaking Change]** Puppeteers now get a digest of the instances it
  registers to. As a consequence, Puppeteers implementation have to implement a
  new callback.

## lkn_core 0.2.0

* **[Breaking Change]** Puppeteers are now notified when a puppet enters
  or leaves the instance they are registered to. As a consequence,
  Puppeteers implementations have to implement new callbacks.
* **[Breaking Change]** Entities can now be stopped in a clean way. As a
  consequence, Entities implementations have to implement new callbacks.
