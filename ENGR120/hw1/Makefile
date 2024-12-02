CHK = checker
SUM = summary
ALL = q1 q2 q3 q4 q5 q8 q11 q12

word-dot = $(word $2,$(subst ., ,$1))

.DEFAULT_GOAL = all

.PHONY: all clean

all: $(ALL)

%: %.c
	@echo "======---"$@"---=======" | tee -a $(SUM)
	@echo `date` | tee -a $(SUM)
	@echo "Checking format violation by a script..." | tee -a $(SUM)
	@./checker.py hello.c | tee -a $(SUM)
	@echo "Compiling "$<" to "$@ | tee -a $(SUM)
	@gcc -o $@ $<
	@for input in $(wildcard $@.input.*) ; do \
		echo "Comparing using input "$$input" and answer "$${input//"input"/"ans"} | tee -a $(SUM) ; \
		echo "Diff output (empty line means correct):\n" | tee -a $(SUM) ; \
		./$@ < $$input > $@.output | tee -a $(SUM) ; \
		diff --strip-trailing-cr $@.output $${input//"input"/"ans"} | tee -a $(SUM) ; \
	done

clean:
	-@rm -rf $(ALL) summary
