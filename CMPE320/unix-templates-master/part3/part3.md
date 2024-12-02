# UNIX Developer Commands
This markdown file is the template for you to use.  It will be processed by the
autograder to extract your answers.  The autograder will look for markdown code segments.
It will extract the text in the code segment, and then it will execute it.

There are two types of code blocks that my utility will recognize:
1. using the \`text\` for a single line
2. using the \`\`\` multi-lines of text \`\`\`` for multiple lines


For example, you can write a solution like:

0. an example question 

`example answer`

or 

0. a longer example question
```
{
    test, test test
}
```

## Questions and Answers

# Editing Files
There are many different text editors in the UNIX world, but the _de facto_ standard today
is Vim, or _vi improved_.  Even though integrated development environments (IDEs) are 
also really common and offer a wide variety of tools, knowing the basics of the 
`vim` is still an important skill, especially for quick editing tasks.


1.	Write the command line to use vi or vim to edit a file

`vi foo.c`

2.	From within vim, how would you save a file

`:w`

3.	From within vim, how would force save a file

`:w!`

4. Write the current file to a new file name 'moe.txt':

`:w moe.txt`

5.	From within vim, how you insert one file into another file

`:r larry.txt`

6.	Write a sed script to replace all commas with colons in a file named "commas.txt"

`sed 's/,/:/g' commas.txt`

7.	Write the vim command to replace all commas with colons in the current line in the editor:

`:s/,/:/g`

8.	Write the vim command to replace all commas with colons in the whole file:

`:%s/,/:/g`

9.	Yank 8 lines from the current editor text into the copy buffer (hint: this does not get the colon):

`8y`

10. Paste the contents of the current copy buffer at the location of the cursor (hint: this doesn't get the colon either):

`p`

11. Use the "bang" operator to run "gcc test.c":

`:!gcc test.c`

12. Use vim's internal make command to build a project?

`:make`

13. After using vim's internal make command, how you go to the next compliation error? (see :h quickfix.txt for direction)

`:cnext`

14. After using vim's internal make command, how you go to the previous compliation error? (see :h quickfix.txt for direction)

`:cprevious`

# Using GIT
This section explores using `git` from the command line.  Git was designed for use at the command line, and while there 
is a GUI front-end for git available in just about every IDE, there are still times when its either easier to drop 
into the command line to do some arcane task or just to show off to your friends, or to automate tasks in shell scripts. 
For example, setting up an automated testing environment that will pull a commit and automatically run tests on the 
code base.

15. Use `git` to clone a repository: `https://github.com/githubtraining/hellogitworld.git` to the current directory.

`git clone https://github.com/githubtraining/hellogitworld.git`

16. Use `git` to initialize create a new repository from the current directory

`git init`

17. Use `git` to add all of the '*.java' files the _staged changes_ to be committed

`git add \./\*.java`

18. Use `git` to commit the current _staged changes_ to the current (local) repository with a message "fixed bug #334"

`git commit -m "fixed bug #334"`

19. Use `git` to add a remote repository named 'origin' using the URL: `https://gitlab.engr.ship.edu/aa1234/bestproject.git` 

`git remote add origin https://gitlab.engr.ship.edu/aa1234/bestproject.git`

20. Use `git` to push changes to a remote repository named `origin` (the default):

`git push origin`

21. Use `git` to add an additional remote repository named 'testing' using the URL: `https://gitlab.engr.ship.edu/bb5678/testing.git`

`git remote add testing http://gitlab.engr.ship.edu/bb5678/testing.git`

22. Use `git` to push changes to the new testing repository only:

`git push testing`

23. Use `git` to merge changes from the `remote` repository to your local repository:

`git merge remote`

24. Use `git` to fetch any changes from the `remote` repository into your local repository:

`git fetch remote`

25. Use `git` to compare the branch `version1` with the current state of the repository:

`git diff version1`

26. Use `git` to revert any changes to the file named 'main.c':

`git checkout HEAD -- main.c`

27. Use `git` to revert all changes to all files:

`git reset --hard`

28. The git log contains a history of all commits made to a project.  It can be really useful to assess how much someone is contributing to a project.  Using what you've learned so far, use the git log command to retrieve all the commits, find just the author entries, remove the "Author:" tag, sort them, count the unique entires, and the sort the list in descending order.

`git log | grep "Author" | cut -c 9- | uniq -c | sort -nr`

# Using GDB
The GNU debugger is one of the most important tools for a developer.  This set of exercises will introduce you to some basic GDB commands.  One of the interesting aspects of GDB is that it runs completely in a terminal, reading from `stdin`, and writing to `stdout`.  In the sample files for this part there is a pre-compiled executable, `debug.bin`.  This is the executable that will be used in the tests for each of the commands.

29. Use GDB to open an executable (just give the command line)

`gdb debug.bin`

30. GDB can run the program under test, and can even give command line arguments to it.  Give the GDB commands to set three command line arguments to the program: "one two three", and then run the program.  Do not show the command to run GDB, the test do that for you (i.e. it will use the command from question #28).
```
set args one two three
run
```

31. When a program crashes, UNIX systems can be configured to _dump core_, a historical term from the days of the old core memory systems.  The core file is a complete description of your process when the OS killed it.  This is incredibly useful for _post mortem_ debugging, especially for complex programs.  However, the size of these files can be quite large, so the feature is turned off by default.  Show the `ulimit` command to enable the generation of a core file when a program crashes.

`ulimit -c unlimited`

32. Use the GDB to open a core file and print the location where it aborted.  Give just the input to GDB, the test will run GDB for you.

`gdb --core core`

33. GDB allows you to set break points before running a program.  Give the command to set a break point on a function called "copy", and then run 
the program.

```
break copy
run
```

34. GDB tracks the connection between source code and executable code.  Show the GDB commands to list only the source for the copy function, and then print a break point on the line for the memcpy.  As before, GDB will already be running.

```
list copy
break 9
run
```

35. GDB allows a special type of break point called a watch point (aka data breakpoint).  The watch point will detect whenever a variable is changed, any time its changed.  Watching local variables can only happen after the function has been called.  So, show the GDB commands to break when the `main` function is called, run the program, and when it breaks, set a watch point on the local variable `i`, and then continue running the program.  This is only four commands.

```
break main
run
watch i
continue
```

36. GDB watch points can actually use complex expressions, like `a*a + b *b > 40`.  Like the previous question, set a watch point on i, but this time, when its value is > 2.  Note: the variable i is used to loop over the command line arguments, so you'll need to set at least three command line arguments, `one two three`.

```
set args one two three
break main
run
watch i > 2
c
```

37. GDB can show you the call stack of the program when it encounters a break point.  As in a previous question, show the GDB commands to put a break point in the `copy` function, run the command, and then show the call stack.

```
break copy
run
bt
```

38. GDB can also show you local variables whenever it encounters a break point.  Look up the `info` command to show the local variables.  Show the GDB commands to put a break point in at `copy`, and then run the program, and then display the local variables.  

```
break copy
run
info locals
```

39. GDB even lets you change (some) variables.  Show GDB commands to set three command line arguments (`one, two, three`), put a break point in on line 20, run the program, and then make change the value of `argc` to 0, and then continue running the program.  This should make the for loop skip printing command line arguments!

```
set args one two three
break 20
run
set args
c
```

# Valgrind and Friends

Perhaps one of the most important developer's tools for C programmers is `valgrind`, a tool that will analyze the execution of your code and analyze it for memory issues, performance issues, and other problems.

40. The `debug.bin` program in that we've been using clearly has a segfault.  Show the command to run `valgrind` on your this program.

`valgrind ./debug.bin`

41. Valgrind can also track full details about leaked memory.  Show the valgrind command line to perform a full leak check on the `./debug.bin`:

`valgrind --leak-check=full ./debug.bin`

42. Valgrind includes several different tools.  One interesting one is the `cachegrind` tool, which can show instruction hits/misses, as well as branch prediction accuracy.  Show the command to run the `cachegrind` tool on `/bin/ls`:

`valgrind /bin/ls --tool=cachegrind`

43. Valgrind also includes a tool to capture information about the function calls your program makes.  Show how to run valgrind to collect this information (you may need top use some google foo here):

`valgrind /bin/ls --tool=callgrind`

44. The output of call grind is less than impressive, but there another program that can be used to interpet the results.  Use some Google foo and show the command to show the annotated call history after running valgrind.  Just show the new program.

`valgrind /bin/ls --tool=callgrind_annotate`

45. Valgrind also has a tool for analyzing threads which will be useful later in the semester.  Just enter the name of the tool below.

`valgrind --tool=helgrind`

[//]: <> (Completed by Andrew Januszko and Andrew Wertz)