preprocessing:
	@R CMD BATCH script/preprocessing.R
	@make clean

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  clean             to remove build files"
	@echo "  server-up         to run the HTTP Server"
	@echo "  preprocessing     to preprocess the dataset"
	@echo "  all               to preprocess and run HTTP Server"

clean:
	@find . -name '*~' -exec rm --force  {} +
	@find . -name '*.pyc' -exec rm --force {} +
	@find . -name '*.pyo' -exec rm --force {} +
	@find . -name '*.Rout' -exec rm --force {} +
	@find . -name '*.RData' -exec rm --force {} +
	@echo "<< Clean ... >>"

all: preprocessing server-up
