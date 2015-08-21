time
====
Time card system written in Haskell with MongoDB as backend, with a shell-like
interface

Background
----------
This project was written with the sole purpose of learning Haskell and MongoDB,
but the program is working fine and I'm using it as my time card system (at the
time of writing). But the polish is not, well, polished.

Noteworthy features 
-------------------
Meaning, parts that were particularly fun to write :)

* The [Maybeify](https://github.com/rootmos/time/blob/master/Maybeify.hs)
  module, which uses [Template Haskell](https://wiki.haskell.org/Template_Haskell)
  to wrap all types in a data constructor with Maybe:s, making all fields
  optional! This was used to read the configuration data, which may not contain
  all necessary information.
* Using [Optparse-Applicative](https://hackage.haskell.org/package/optparse-applicative)
  was a blast! I used it to parse the command line options and implement the
  shell with its subcommands-feature.

Installation
------------
The dependencies are listed in the file `dependencies` and can be installed
with [cabal](https://www.haskell.org/cabal/). A wrapper is provided in the make
target: `make deps`.

Compilation is done by `make`, after that a nice `time` binary should be
present in your repository!

Usage
-----
After compilation you can run the `time` program like this:
```
time --host=localhost --port=27017 --name=foobar
```
This will try to connect to a MondoDB server running on the `localhost`, as a
user foobar. If the user don't exist then the program asks if you want to
create it.

After getting a nice `>` prompt, you can `add` time, `show` status for periods,
or use `week` to get a summary of the week! Happy time-reporting!
