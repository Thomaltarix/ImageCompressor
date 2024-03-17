##
## EPITECH PROJECT, 2024
## MiniLibC
## File description:
## Makefile
##

## Names
NAME = imageCompressor

BINARY_NAME = $(NAME)-exe

COVERAGE_NAME = $(NAME)-test

BONUS_NAME = bonus_$(NAME)


## Paths
BINARY_PATH = $(shell stack path --local-install-root)

COVERAGE_PATH = \
	$(BINARY_PATH)/hpc/imageCompressor/$(COVERAGE_NAME)/$(COVERAGE_NAME).tix


## Rules
.PHONY:	all clean fclean re tests_run clean_tests bonus

all:	$(NAME)

$(NAME):
		stack build
		@cp $(BINARY_PATH)/bin/$(BINARY_NAME) ./$(NAME)

clean:
		@stack clean
		@make clean -sC bonus/

fclean: clean clean_tests clean_bonus
		@make fclean -sC bonus/
		@rm -f $(NAME)
		@rm -rf test/coverage
		@rm -f app/Main

re:	fclean all

tests_run:
		stack test --coverage
		@mkdir -p test/coverage
		@cp $(COVERAGE_PATH) test/coverage/

clean_tests:
		rm -rf test/coverage
		rm -f $(COVERAGE_PATH)
		rm -f app/Main

bonus:
		@make -sC bonus/
		@cp bonus/$(BONUS_NAME) ./

clean_bonus:
		@make fclean -sC bonus/
		@rm -f $(BONUS_NAME)
