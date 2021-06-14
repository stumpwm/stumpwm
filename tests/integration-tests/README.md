# Generative, end-to-end test suite for stumpwm

## Why

A window manager's primary purpose is to interact with the windowing system and the user.
Since (end-to-end) integration tests are the only kind that actually test this primary functionality, they are arguably the most important kind of tests for any window manager.

Property based testing, or generative testing, is an approach to partly automate bug finding.
Rather than manually writing each test case, they are generated based on certain rules.
When an automatically generated test case fails, it is investigated manually to confirm that the test is correct, and then committed to the repository.
The repository therefore contains both
1. individual test cases that have previously failed, to prevent regression
2. rules to generate new test cases.

## Generating and running tests

For usage help, run:
```
./script
```

When you run tests, the test results will be in `testcases/*.results/`.
Successful tests will have a `success` file while failed tests will have a `fail` file containing a message.
Usually, the `stdout.txt` and `screenshot-*.png` files will show the necessary details.

If you generate tests, they will be named `testcases/generated-*.sh`.
If you want to commit a generated test to the repository, rename it first.
It can also be a good idea to minimize the test, i.e. make it shorter while retaining the failure.

## Development

The file `container-scripts/generate-test-code` contains the code and rules to generate a random test case.
