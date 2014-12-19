Windex
======

Hierarchical data storage and retrieval

WARNING
=======

This is early alpha software at this point, the
instructions below are bound to change drastically

Overview
========

Windex provides a simple web interface to store JSON
objects that are in a tree format and then search on
and retrieve information from that structure with
simple HTTP Restful semantics.

Requirements for end-users
==========================

Erlang Version 17+ is required.
For some of the scripts you will need some other languages such as a modern Perl5,
but they are not necessary for the function of the server.

Setting up and building
=======================

  git clone https://github.com/ricecake/plywood.git
  cd plywood
  ./rebar get-deps co
  erl -pa ebin/ -pa deps/*/ebin
    #you will now be in the erlang shell
  application:ensure_all_started(plywood).

Basic Usage
===========

From here you can run one of the test scripts, for
instance the Perl5 script with a directory to
index as the argument:

  perl test-data.pl /home/shane/Downloads/

this will produce the key that is used for storage:

  home-shane-Downloads

Now travel to the server with a web browser and it
will return the JSON structure of the tree:

  http://localhost:8080/tree/home-shane-Downloads/

And if you want the JSON structure for a sub-dir:

  http://localhost:8080/tree/home-shane-Downloads/isos/

Search semantics (much like GNU find) is on its way.

Output
======

Once you get this far you'll be able to start pulling
data out of plywood. The structure is basically as such:

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

Directories will have names and ids, and an array of children
which can be directories OR files if you're indexing a filesystem
with this software.
Files will have stat data in an array hashed. And this is of course
potentially recursive with a bigger filesystem than what is shown above,
and so can get very large.
