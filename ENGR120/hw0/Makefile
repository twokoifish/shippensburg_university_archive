CHK = checker
ANS = hello.ans.txt
IN =
OUT = hello.txt
SUM = summary

.DEFAULT_GOAL = diff

.PHONY: check diff clean

diff: $(ANS) $(OUT) hello
	@echo "*===========*" | tee -a $(SUM)
	@echo `date` | tee -a $(SUM)
	@echo "Checking program output ..." | tee -a $(SUM)
	@diff -B $(ANS) $(OUT) | tee -a $(SUM)
	@exit $(.SHELLSTATUS)

hello.txt: hello $(IN)
	@echo "*===========*" | tee -a $(SUM)
	@echo `date` | tee -a $(SUM)
	@echo "Running program ..." | tee -a $(SUM)
	@./hello | tee -a $(OUT)

hello: hello.o
	gcc hello.o -o hello

hello.o: hello.c
	gcc -c hello.c

check: hello.c
	@echo "*===========*" | tee -a $(SUM)
	@echo `date` | tee -a $(SUM)
	@echo "Checking code style violation ..." | tee -a $(SUM)
	@./checker.py hello.c | tee -a $(SUM)

clean:
	-@rm -rf hello.o hello hello.txt summary
