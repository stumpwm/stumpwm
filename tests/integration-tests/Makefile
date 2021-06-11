run-tests: $(shell ls -1 testcases/*.sh|sed -r 's/\.sh/.results\/success/')

generate-tests: test-runner.iid
	./script internal-generate-tests "${COUNT}"

testcases/%.results/success: testcases/%.sh test-runner.iid
	./script internal-run-one-test "$*"

test-runner.iid: container-scripts/* Dockerfile Makefile ../../*.ac ../../*.asd ../../*.in ../../*.lisp ../../*.sh ../../.*.el ../../.*.yml ../../[A-Z]*
	docker build --iidfile test-runner.iid --build-arg userUID="$(shell id -u)" -f Dockerfile ../..

.PHONY: run-tests generate-tests
