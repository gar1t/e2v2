==============
 Concise Code
==============

Writing concise means writing less code. Less code is generally easier to read
and understand than verbose code.

Here's the Fibonacci series algorithm in Java [#f1]_:

.. code-block:: java

   public class FibonacciIterative {
       public static int fib(int n) {
           int a=0, b=1;
           for (int i=0; i < n; i++) {
               int aSave = a;
               a = b;
               b = aSave + b;
           }
           return a;
        }
   }

Here's the functional equivalent in Erlang:

.. code-block:: erlang

   fibo(0) -> 0;
   fibo(1) -> 1;
   fibo(N) when N > 0 -> fibo(N-1) + fibo(N-2).

The Erlang code not only has fewer lines of code, it resembles the `definition
of the Fibonacci series`__.

While functional languages are not necessarily or automatically more concise
than their imperative counterparts, functional programming conventions
encourage short, single-purpose functions that can used in a more declarative
style.

The following example in Python illustrates a common pattern in imperative
languages: declare a local list variable, loop over some structure and add
values to that list, return the list:

.. code-block:: python

   def download_files(file_keys):
       files = []
       for key in file_keys:
           files.append(get_file(key, temp_file(key)))
       return downloaded

Here's the equivalent code in Erlang:

.. code-block:: erlang

   download_files(FileKeys) ->
       [get_file(Key, temp_file(Key)) || Key <- FileKeys].

While it happens that Python supports list comprehension and can be used to
write essentially the same code [#f2]_, Erlang enforces the practice.

__ http://en.wikipedia.org/wiki/Fibonacci_number

.. rubric:: Footnotes

.. [#f1] This example uses a loop rather than recursion to illustrate the
         alogithm rather than the logical definition of the Fibonacci
         series. It's possible to implement this function in fewer lines in
         Java.

.. [#f2] Here's the same function implemented using Python list
         comprehension. This is using Python in a "functional style".

.. code-block:: python

   def download_files(file_keys):
       [get_file(key, temp_file(key)) for key in file_keys]
