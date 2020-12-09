playaxdata
==========

Access Playax data from R

Goals
=====

The main goal of this package is to encapsulate the complexity of the internal
Playax databases under a set of clear, semantically well-defined interfaces which 
require users to remember as little as possible about how the actual database
is organized. 

Query efficiency is a somewhat secondary goal as this package is meant for use in
analysis environments and as the lower layer of relatively static dashboards. 
The main technical goals are: _i)_ correct queries, and; _ii)_ ease of update 
as the underlying database and system evolves.
