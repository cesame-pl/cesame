# Define directories
SRC_DIR = src
TEST_DIR = test
LIB_DIR = $(SRC_DIR)/lib
OUT_DIR = $(TEST_DIR)/output

# Define flags
FLAGS = -a -s -l

# Define command
LLI := lli

# Build targets
.PHONY : all
all : clean cesame test run_lli summary

.PHONY : cesame
cesame : 
	make -C $(LIB_DIR) printbig.o
	make -C $(SRC_DIR) cesame
	mv $(SRC_DIR)/cesame cesame

# Test different flags and files
.PHONY: test
test: cesame
	@echo "Testing cesame with different flags and files..."
	@$(foreach flag,$(FLAGS), \
		mkdir -p $(OUT_DIR)$(flag); \
		$(foreach file,$(wildcard $(TEST_DIR)/*.csm), \
			./cesame $(flag) $(file) > $(OUT_DIR)$(flag)/$(notdir $(basename $(file))).out 2> $(OUT_DIR)$(flag)/$(notdir $(basename $(file))).err || true; \
		) \
	) > /dev/null

	@$(foreach flag,$(FLAGS), \
		find $(OUT_DIR)$(flag) -type f -empty -delete; \
	) > /dev/null

# Executes programs in LLVM bitcode format
.PHONY: run_lli
run_lli: test
	@echo "Running lli on files in $(OUT_DIR)-l"
	@for file in $(wildcard $(OUT_DIR)-l/*.out) $(wildcard $(OUT_DIR)-l/*.err); do \
		if [ $${file: -4} = ".out" ]; then \
			$(LLI) $$file > $(TEST_DIR)/$$(basename $$file .out).out; \
		elif [ $${file: -4} = ".err" ]; then \
			cp $$file $(TEST_DIR); \
		fi \
	done

# Summary target
.PHONY: summary
summary: run_lli
	@EXPECTED_PASS=$$(ls $(TEST_DIR)/test*.csm 2>/dev/null | wc -l); \
	EXPECTED_FAIL=$$(ls $(TEST_DIR)/fail*.csm 2>/dev/null | wc -l); \
	ACTUAL_PASS=$$(ls $(TEST_DIR)/*.out 2>/dev/null | wc -l); \
	ACTUAL_FAIL=$$(ls $(TEST_DIR)/*.err 2>/dev/null | wc -l); \
	echo "\nSummary:"; \
	echo "Failed Cases - Actual: $$ACTUAL_FAIL; Expected: $$EXPECTED_FAIL"; \
	echo "Passed Cases - Actual: $$ACTUAL_PASS; Expected: $$EXPECTED_PASS"

# Clean output files on all dirs
.PHONY : clean
clean :
	make -C $(LIB_DIR) clean
	make -C $(SRC_DIR) clean
	rm -f cesame
	rm -rf $(OUT_DIR)*
	rm -f $(TEST_DIR)/*.out $(TEST_DIR)/*.err
