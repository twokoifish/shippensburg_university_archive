CHK = checker
SUM = summary
ALL = q1 q4 q5 q6 q9 q11 q12

.DEFAULT_GOAL = all

.PHONY: all clean

all: $(ALL)

%: %.c
	@echo "======---"$@"---=======" | tee -a $(SUM)
	@echo `date` | tee -a $(SUM)
	@echo "Checking format violation by a script..." | tee -a $(SUM)
	@./checker $@.c | tee -a $(SUM)
	@echo "Compiling "$<" to "$@ | tee -a $(SUM)
	@gcc -o $@ $<
	@for input in $(wildcard $@.input*) ; do \
		echo "Comparing using input "$$input" and answer "`echo $$input | sed -e "s/input/ans/g"` | tee -a $(SUM) ; \
		echo "Diff output (empty line means correct):\n" | tee -a $(SUM) ; \
		./$@ < $$input > $@.output | tee -a $(SUM) ; \
		diff --strip-trailing-cr $@.output `echo $$input | sed -e "s/input/ans/g"` | tee -a $(SUM) ; \
	done

clean:
	-@rm -rf $(ALL) summary *.output
