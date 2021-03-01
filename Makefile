NAME = wolfram

all: $(NAME)

$(NAME):
	stack build
	mv `stack path --local-install-root`/bin/wolfram-exe wolfram

clean:
	stack clean

fclean:
	stack clean
	$(RM) $(NAME)

re: fclean all

.PHONY = all clean fclean re