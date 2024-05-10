# Define directories
SRC_DIR = src
LIB_DIR = src/lib
TEST_DIR = test
OUT_DIR = $(TEST_DIR)/output

# Define flags
FLAGS = -a -s -l

# Build targets
.PHONY : all
all : clean cesame test

.PHONY : cesame
cesame : 
	make -C $(LIB_DIR) printbig.o
	make -C $(SRC_DIR) cesame
	mv $(SRC_DIR)/cesame cesame

.PHONY : clean
clean :
	make -C $(LIB_DIR) clean
	make -C $(SRC_DIR) clean
	rm -f cesame
	rm -rf $(OUT_DIR)*  # Remove output directories and files

# Test different flags and files
.PHONY: test
test:
	@$(foreach flag,$(FLAGS), \
		mkdir -p $(OUT_DIR)$(flag); \
		$(foreach file,$(wildcard $(TEST_DIR)/*.csm), \
			./cesame $(flag) $(file) > $(OUT_DIR)$(flag)/$(notdir $(basename $(file))).out 2> $(OUT_DIR)$(flag)/$(notdir $(basename $(file))).err || true; \
		) \
	) > /dev/null

	@$(foreach flag,$(FLAGS), \
		find $(OUT_DIR)$(flag) -type f -empty -delete; \
	) > /dev/null
