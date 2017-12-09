preprocessing:
	@R --no-save CMD BATCH preprocessing.R
	@make clean

ml:
	@R --no-save CMD BATCH ml.R
	@make clean

pca:
	@R --no-save CMD BATCH dr.R
	@make clean


help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  clean             to remove build files"
	@echo "  preprocessing     to preprocess the dataset"
	@echo "  ml               to preprocess and run HTTP Server"
	@echo "  all               to preprocess and run HTTP Server"

clean:
	@find . -name '*~' -exec rm --force  {} +
	@find . -name '*.pyc' -exec rm --force {} +
	@find . -name '*.pyo' -exec rm --force {} +
	@find . -name '*.Rout' -exec rm --force {} +
	@find . -name '*.RData' -exec rm --force {} +
	@echo "<< Clean ... >>"

all: preprocessing ml
