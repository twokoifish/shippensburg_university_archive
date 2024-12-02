# UNIX Basic Commands - Part 2
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

# UNIX Plumbing
The plumbing operators allow you to redirect output to/from a file or to another program:
*	`command > file`  - run command, send its standard output to file
*	`command < file` – run command, read file as its standard input
*	`command 2> file` – run command, send its standard error to file
*	`command1 | command2` – run command 1 and 2, send command 1’s  standard output to command 2’s standard input 
*	`command 2>&1` – run command, merge its standard error to its standard output

UNIX includes hundreds of small utilities each of which that can transform their behavior based on command line arguments.
This has given UNIX an edge for power-users who can string together command lines to perform a variety of tasks.  

1. Use the echo command and redirection to create a file called "output", and echo the string "hello world".

`echo hello world | cat > output`

2. Use the echo command and redirection to append to the file called "output", and add the phrase "from the shell".

`echo "from the shell" >> output`

3. Use the "ls" command to list the files in /etc and pipe the output to the grep utility to look for file names contain at least one upper case letter (use regular expressions).

`ls /etc | grep "[A-Z]"`

4. Run the "ls" command to list files in "/usr" and pipe its output to the "tee" command.  Use the tee command to create a file called "output".  You should see the contents on the terminal and in the output file.  Tee creates a "tee" in the pipeline.

`ls /usr | tee output`

5. Use the "grep" command to look, recursively, in the "/etc" directory for files containing the word "password" and send it through the "tee" command.  But, since you won't be able to read all files, you'll need to send standard error to standard out so that "tee" will see both output and error messages.  Hint: you'll need to put the redirection of stderr to stdout before the pipe operator.

`grep -R "password" /etc 2>&1 | tee`

6. Use the "cut" command to extract just the email address from the file "addrbook.csv" (its in field 4), but redirect the output to a file called "email.txt"

`cut -d, -f 4 addrbook.csv > email.txt`

7. The "fmt" command can reformat text.  Use it to reformat the file "words.txt" into a column of words (paragraph width of 1).  Pipe the output into the "sort" command, and then pipe the output to "uniq", with the option to count the occurences.  Then pipe the output to sort the result numerically in descending order, and then finally display the counts of the top 10 words (first 10 lines).

`fmt -w 1 words.txt | sort | uniq -c | sort -dr | head`

8. For each of the top 10 words in the "words.txt" file (see previous question), this time, "cut" off the count and display the result.

`fmt -w 1 words.txt | sort | uniq -c | sort -dr | head | awk '{print $2}'`

9. For each file in the /etc current directory, create a list of the file's group, and then pipe that through sort, unique, and finally, sort into which group owns the most files; show the group and number of files.

`ls -l /etc  |  awk '{print $4}' | sort -rn | uniq -c | sort -rn`

10. Use the "fmt" command to reformat the words.txt into a "balanced" paragraph and then pipe the output to the "mail" command, given the subject "Shell Redirection" and email it noreply@example.com

`fmt words.txt | mail -s "Shell Redirection" noreply@example.com`

11.	Use the find to list all files in the "/etc" directory, but pipe the output to one of the pagination programs (more or less) to view the output one page at a time:

`find /etc | less`

12.	Use the ps command to list all process, then pipe into the grep command to filter it to lines that contain the word "bash"

`ps | grep bash`

13.	Use the git command line utility to list all of the available branches that include the string “release-to-test”

`git grep release-to-test $(git rev-list --all)`

14. Use the "paste" command to "serialize" the "nums.txt" file into a single line, using a delimiter of "+", which creates an arithmetic expression.  Pipe this into the "bc" program to compute the sum of the numbers.

`paste -s -d "+" nums.txt | bc`

15. Use the "ifconfig" command find only lines containing "inet" addresses.

```
ifconfig -a | grep "inet"
```
# Programming with sed
Find one of the millions of references on the sed tool then answer these questions (some of these are basically Hacker Rank questions, so theres some easy Hackos here).  
Remember, sed expects its input to come from standard input, and it writes its output to standard output – so you’ll want to use the pipe operations:

16.	Echo the text "Trump wins election" and pipe through sed to replace the text "Trump" with "Biden"

`echo "Trump wins election" | sed 's/Trump/Biden/'`

17.	Echo the phrase "this is a test,,this is only a test" through sed, and use character types to delete *all* punctation marks.

`echo "this is only a test,,this is only a test" | sed s/[[:punct:]]/' '/g`

18.	Use sed on the "addrbook.txt" to replace every occurrence of an email address at "teleworm.us" to "telework.us" - but in place (actually edit the file).
`sed -i 's/teleworm.us/telework.us/g' addrbook.txt`


# Programming with awk
Find one of the millions of references on the awk tool then answer these questions (some of these are basically Hacker Rank questions, so theres some easy Hackos here).  
Remember, just like sed, awk expects its input to come from standard input, and it writes its output to standard output – so you’ll want to use the pipe operations.
Hacker Rank also has a section awk, which may be useful and vaguely similar.  One thing that may be helpful is that Awk uses space as the default delimiter.  To change
that character you can use the "FS" variable.  You can change it to something else, for example, tabs:  `awk -vFS="\t" '{ print $3 }' < addrbook.txt` will print all of the phone
numbers in the address book.

19. Write an awk script that print the email address for everyone in Connecticut (state = CT).

`awk -vFS="\t" '{if($7=="CT") print $4}' < addrbook.txt`

20.	Write an awk script that will compute the average of the grades in "grades.txt" of three numbers (fields 2, 3, 4) and print the first field, a colon, and the average on a line (see HR: Awk #2)

`awk '{avg=$2+$3+$4; print $1,":",avg/3}' < grades.txt`

21.	Write an awk script that can detect any line in addrbook.txt that does contain exactly 8 fields, separated by spaces (see HR: Awk #1)

`awk -vFS="\t" '{if(NF==8) print "8"}' < addrbook.txt`

22. Use AWK to transform the nums.txt into a C language Array initialization file, e.g. `int A[] = { 1,3,5,3,2 ...};`, its OK to put everything on one long line.  Hint: check out awk BEGIN, END, and variables.

`awk '{ awkArray[counter++] = $1; } END { printf "int A[] = {"; for (n=0; n<counter;n++){ if(counter-1==n) printf "%d", awkArray[n]; else printf("%d,",awkArray[n]);} printf "};\n";}' < nums.txt`

# UNIX Enviornment Variables

23.	Write the command to set the TERM environment variable to "vt100"

`export TERM=vt100`

24.	Write the command show the value of the HOME environment variable 

`echo $HOME`

25.	Write the command to show all environment variables and pipe it through less

`set | less`

26.	Write the command to delete an environment variable PS4 (not just make its value a null string)

`unset PS4`

27. Write the command to add the "/foo" directory to end of the environment variable where UNIX searches for executables.

`export PATH=$PATH:/foo`

28. Show all environment variables and grep for any that contain the word "bash"

`set | grep bash`

29. Show the command that would change the prompt (where you type commands) to "enter command> "

`export PS1="enter command>"`

# UNIX Enviornment Variables
Use “bash” to write the following as shell scripts (there are thousands of shell script references on the internet).  Normally, shell scripts would be saved to a file.  In this case, use the "multi-line code" block and put the contents here.

30.	Print all the odd numbers from 1 to 99 on the terminal

```
INDEX="0"
while [ $INDEX -lt 100 ]
do
  ISEVEN=$(( $INDEX % 2 ))
  if [ $ISEVEN -ne 0 ]
  then
    echo $INDEX
  fi
  INDEX=$(($INDEX + 1))
done
```

31.	Write a bash script that will loop over all of the file names in the current directory (look up bash for loops) and use an if statement and test to determine if the file is a zero length file.  If it is, print its name. Hint: check out the bash man page to look for the buillt in comparisons.  You should find a way to test if a file is not zero, but that shouldn't be a complication!
```
for file in *; do 
    if [ ! -s "$file" ]; then 
        echo "$file"
    fi
done
```
32. Print a times table from 1 to 10 on each dimension  For example, your output should look like:

><pre>1 2 3 4 5 6 7 8 9 10 
>2 4 6 8 10 12 14 16 18 20 
>3 6 9 12 15 18 21 24 27 30 
>4 8 12 16 20 24 28 32 36 40 
>5 10 15 20 25 30 35 40 45 50 
>6 12 18 24 30 36 42 48 54 60 
>7 14 21 28 35 42 49 56 63 70 
>8 16 24 32 40 48 56 64 72 80 
>9 18 27 36 45 54 63 72 81 90 
>10 20 30 40 50 60 70 80 90 100</pre>

```
for i in {1..10}
do
	for j in {1..10}
	do
		printf "$(($i * $j)) "
	done
printf "\n"
done
```

33. The "wget" program can retrieve a web page.  Write a shell script that will retrieve a URL and save it as a file named "current.html".  Then, use the "diff" command, and using an "if" statement, test to see if the current.html is different than the "previous.html" and if so print the word "different" to stdout.  If they are not different produce no output.  Either way, copy the "current.html" to "previous.html".  In this manner, if you were to run this periodically you could be notified whenever a URL has changed.  You may need to use options to make wget become more quiet.

```
wget --quiet -O current.html http://www.ship.edu 2>&1 > /dev/null

DIFF=$(diff current.html previous.html)
if [ "$DIFF" != "" ]; then
  echo different
fi

mv current.html previous.html
```

[//]: <> (Completed by Andrew Januszko and Andrew Wertz. Question #2 Completed by Ryan)