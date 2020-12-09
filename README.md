playaxdata
==========

Access Playax data from R

Goals
=====

The main goal of this package is to encapsulate the complexity of the internal
Playax databases under a clear, semantically well-defined interface which 
requires users to remember as little as possible about how the actual database
is organized. 

Query efficiency is a somewhat secondary goal as this package is meant for 
analysis and as the lower layer of relatively static dashboards. The main technical 
goals are _i)_ correct queries, and; _ii)_ ease of update as the underlying 
database and system evolves.
