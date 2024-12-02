CHK = checker
SUM = summary
ALL = q9write q9read ip.bin q9read.output

.DEFAULT_GOAL = all

.PHONY: all clean

all: q9read.output
	@echo "Comparing output" | tee -a $(SUM)
	@echo "Diff output (empty line means correct):\n" | tee -a $(SUM)
	@diff --strip-trailing-cr q9read.output q9read.ans | tee -a $(SUM)

q9write: q9write.c
	@echo "\n======---"$@"---=======" | tee -a $(SUM)
	@echo `date` | tee -a $(SUM)
	@echo "Checking format violation by a script..." | tee -a $(SUM)
	@./checker $@.c | tee -a $(SUM)
	@echo "Compiling "$<" to "$@ | tee -a $(SUM)
	@gcc -o $@ $^ -lm >> $(SUM) 2>&1

ip.bin: q9write q9write.input
	@./q9write < q9write.input

q9read: q9read.c ip.bin
	@echo "\n======---"$@"---=======" | tee -a $(SUM)
	@echo `date` | tee -a $(SUM)
	@echo "Checking format violation by a script..." | tee -a $(SUM)
	@./checker $@.c | tee -a $(SUM)
	@echo "Compiling "$<" to "$@ | tee -a $(SUM)
	@gcc -o $@ q9read.c -lm >> $(SUM) 2>&1

q9read.output: q9read ip.bin
	@./q9read > q9read.output

clean:
	-@rm -rf $(ALL) summary *.output
