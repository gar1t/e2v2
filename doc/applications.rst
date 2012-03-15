=================
 e2 Applications
=================

e2 applications are logical units of code that can be started and stopped
within an Erlang VM.

For conceptual background on how Erlang manages applications, see
:doc:`erlang-as-os`.

An e2 application is implemented as a single `e2_application` behavior module
that defines the application's top-level processes. These processes are started
and supervised when the application is started.

Here's an `e2_application` behavior module that defined a single `hello`
service:

.. code-block:: erlang
   :linenos:

   -module(hello_app).

   -behavior(e2_application).

   -export([init/0]).

   init() ->
       {ok, [hello]}.

Line 1 is common to all Erlang modules -- it defined the name of the module.

Line 3 declares that the module is an `e2 application` behavior. This
declaration does nothing more than tell the Erlang compiler to confirm that the
expected behavior callback functions are exported.

The rest of the module defines the list of exported functions and the required
`init/0` callback function.

`init/0` returns a list of top-level processes. I
