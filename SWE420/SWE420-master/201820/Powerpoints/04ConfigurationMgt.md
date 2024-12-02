Configuration Management - Keeping track of it all
====
What is Configuration Management?

“Systems engineering process for establishing and maintaining consistency of a product’s performance, functional and physical attributes with its requirements, design, and operational information throughout its life.”[^1]
[^1]: Wikipedia
<br><br>
For us it is keeping track of all of the versions of our software
 * Being able to tell exactly what has changed between any two versions of our system
  * Ensuring that we can undo any individual change

# Configuration Management Strategies
 * Reserve - Modify - Replace
 * Patching
 * Distributed Patching

## Reserve - Modify - Replace
* When you want to modify a file, you have to notify the system which reserves the file for you.
* While you are working on it, no one else can change it
* When you are finished, your new file replaces the old one
	
Downsides?

## Patching
* Instead of storing the entire new file, patching systems just record the changes you made
* less storage
* multiple people can be working on a file at a time
* can calculate what any individual version was
* can remove a change in the middle of the stream

conflict: when two people change the same lines.  

### Terminology
* repository - the files holding the history of changes
* checkout - get a version of the system to start editing
* commit - put a change into the repository

Suppose there is one repository

* Then we only put stuff in it when it is ready to go to production
  * very few commits that are very large
    * conflict rate?
    * ability to back something out?

## Distributed Configuration Control
Suppose everyone has a copy of the repository

* Remote Tracking - the latest version you have fetched
* Local - your copy with your commits

Distributed operations

* fetch
* merge
* pull (fetch and merge)
* commit 
* push
* Branching