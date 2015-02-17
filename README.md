Plywood
=======

Hierarchical data storage and retrieval server

WARNING
=======

This is early alpha software at this point, the
instructions below are bound to change drastically,
and until we remove this warning please assume that
this software is not production ready.

Overview
========

The Plywood database server provides a simple web 
interface to store JSON objects that are tree
structured and then search on and retrieve information
from that structure with simple HTTP Restful semantics.

Requirements for end-users
==========================

Erlang Version 17+ is required.
Some of the scripts require a modern Perl, but are
not required for the function of the server.

Installing Erlang 17
====================

See your operating system's documentation on installing
Erlang version 17. If that is not available, you can try
Erlang solutions for a prebuilt package for your OS:

  https://www.erlang-solutions.com/downloads

If this doesn't end up working out for you, you may try
a source install from the Erlang github repo:

  https://github.com/erlang/otp

Building and Running Plywood
============================

Building on Linux
-----------------

```
git clone https://github.com/ricecake/plywood.git
cd plywood
./rebar get-deps co
make package
```

This will make a package for your system's package manager,
see your package manager's documentation (man rpm|man dpkg)
to determine how to install the package.

Once you have the package installed, the software can be
started and stopped:

```
plywood start
plywood stop
```

Basic Usage
===========

From here you can run one of the test scripts, for
instance the Perl5 script with a directory to
index as the argument:

```
perl test-data.pl /home/shane/Downloads/
```

this will produce the key that is used for storage:

  home-USERNAME-Downloads

Now travel to the server with a web browser and it
will return the JSON structure of the tree:

  http://localhost:8080/tree/home-USERNAME-Downloads/

And if you want the JSON structure for a sub-dir:

  http://localhost:8080/tree/home-USERNAME-Downloads/isos/

Search semantics (much like GNU find) is on its way.

Output
======

Once you get this far you'll be able to start pulling
data out of plywood. The structure is basically as such:

```
{"name":"asdf","id":"/asdf","children":
  [{"name":"test1","id":"/asdf/test1","data":
    [{
      "time":1418833894,
      "size":0,
      "mtime":1418833883,
      "mode":"100644",
      "ctime":1418833883,
      "atime":1418833883
    }]
  }]
}
```

Directories will have names and ids, and an array of children
which can be directories OR files if you're indexing a filesystem
with this software.

Files will have stat data in an array hashed. And this is of course
potentially recursive with a bigger filesystem than what is shown above,
and so can get very large.
