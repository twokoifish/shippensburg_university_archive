# UNIX Basic Commands - Part 1
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

1.	list contents of current directory
`ls`
2.	list contents in “/etc” directory
`ls /etc`
3.	list contents of current directory using long listing format
`ls -l`
4.	list contents of current directory using long listing format and with sizes printed as “human readable” out-put
`ls -lh`
5.	list contents of current directory – including hidden files 
`ls -a`
6.	create a new directory named “foo”
`mkdir foo`
7.	change your current directory into “foo”
`cd foo`
8.	print your current working directory
`pwd`
9.	create a new, 0 byte size file called ‘bob’
`touch bob`
10.	move / rename a file named ‘bob’ to ‘chip’
`mv bob chip`
11.	copy  ‘chip’ to ‘dave’
`cp chip dave`
12.	remove the file ‘chip’
`rm chip`
13.	cd “up” a directory
`cd ..`
14.	remove the ‘foo’ whole directory, even if it has files in it
`rm -r foo`
15.	concatenate two files, “/etc/issue” and “/etc/issue.net” to the screen
`cat /etc/issue /etc/issue.net`
16.	list only your running processes on the system
`ps`
17.	list all running processes on the system, including command line arguments.
`ps -eaf`
18.	kill process 3345
`kill 3345`
19.	list who is logged on to the system
`w`
20.	read the manual page about command 'command'
`man command`
21.	search for manual pages that contain the keyword ‘create’
`apropos create`
22.	create a tape archive (tar) file named "backup.tar" from the contents of the directory "/etc/network"
`tar -cvf backup.tar /etc/network`
23.	create a gzip compressed tape archive (tar) file named "backup.tgz" from the contents of the directory "/etc/network"
`tar -cvzf backup.tgz /etc/network`
24.	extract a tape archive (tar) "simple.tar" file to the current directory, using verbose output
`tar -xvf simple.tar`
25.	create a “.zip” file "backup.zip" of all the files in the /etc/directory, including subdirectories
`zip -r backup.zip /etc/network`
26.	extract all of the files from the files.zip file
`unzip files.zip`
27.	Show the amount of disk free space available on the system, in human readable format
`df -h`
28.	Show the amount of space used by the `/usr` directory (including its subdirs) in human readable format
`du -sh /usr`
29.	Display the text “hello world” on your terminal
`echo "hello world"`
30.	Use the “nohup” command to run a long-running program, in this case "sleep 15"
`nohup sleep 15`
31.	Find all of the lines of all .java files that match the string “public static void”
`grep 'public static void' *.java`
32.	Find all of the lines of a file that do NOT match the string “public static void”
`grep -v 'public static void' *.java`
33.	The file 'addrbook.txt' is separated by tabs.  Find all of the lines that start with either Martin or martin.  Look at regular expressions.
`grep '^[Mm]artin' addrbook.txt`
34.	Use the cut command to select only columns 10-20 of the addrbook.txt
`cut -c 10-20 addrbook.txt`
35.	Use the cut command to select fields 1,3,5 from a comma delimited file (CSV) addrbook.csv
`cut -d ',' -f 1,3,5 addrbook.csv`
36.	Use the head command to select the first 10 lines of the addrbook.txt file
`head addrbook.txt`
37.	Use the tail command to select the last 10 lines of a file
`tail addrbook.txt`
38.	Use the tail command to show the last lines of a file and then any new lines as they arrive
`tail -f addrbook.txt`
39.	Sort a text file addrbook.txt in alphanumeric, ascending order
`sort addrbook.txt`
40.	Sort a text file in alphanumeric, descending order
`sort -r addrbook.txt`
41.	Sort a text file nums.txt in numeric order
`sort -n nums.txt`

[//]: <> (Completed by Andrew Januszko and Andrew Wertz.)