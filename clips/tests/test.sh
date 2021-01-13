#!/bin/bash

index=1
testCaseIn="test_cases/$index.in"

# execute all test cases
while [ -f "$testCaseIn" ]
do
	input=$(cat load.in $testCaseIn)

	echo "Running test case $index ..."
	output=$(printf "$input \n(exit)\n" | clips)

	# line from which the output is specific for each test case
	fromLine=$(echo "$output" | grep -n 'TRUE' | head -n 1 | tr ':' "\n" | head -n 1)
	fromLine=$((fromLine+1))

	# remove common output
	output=$(printf "$output" | tail -n "+$fromLine" | head -n -1)
	referenceOutput=$(cat "test_cases/$index.out")

	# compare output with reference output
	if diff -q <(echo "$referenceOutput") <(echo "$output") &>/dev/null;
	then printf "PASSED!\n"
	else printf "FAILED\ndiff below:\n"; diff <(echo "$referenceOutput") <(echo "$output"); echo "";
	fi

	index=$((index+1))
	testCaseIn=test_cases/$index.in
done
