===============
Getting Started
===============

The Source
==========

Get the latest source code from the `e2 repository on github`__.

__ https://github.com/gar1t/e2

.. code-block:: bash

   git clone git@github.com:gar1t/e2.git

Note the directory that e2 was cloned into and create a variable for it:

.. code-block:: bash

   export E2_HOME=<top-level e2 directory>

Building e2
===========

Run `make` within the e2 directory.

.. code-block:: bash

   cd $E2_HOME
   make

Creating a Project
==================

Rebar is used to create project skeletons in e2.

Check if you have rebar installed on your system:

.. code-block:: bash

   which rebar

If you get a message that rebar is not in your path, create an alias to rebar
in the e2 root directory:

.. code-block:: bash

   alias rebar=$E2_HOME/rebar

where `$E2_HOME` is the directory your cloned e2 into (see above).

.. todo::

   - How can we extend the rebar templates without copying into ~/.rebar?

   - Show basic project

   - Maybe a more advanced project (e.g. socket server)

   - A web app?

Next Steps
==========

:doc:`overview`
