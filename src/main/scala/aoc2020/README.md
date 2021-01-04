# Advent of Code 2020

## Day 1

### Part 1:
Find two numbers in the input that sum to 2020.

### Approaches

#### Sort and Search
Sort the list then, using two pointers, search from each end of the list until
we find a pair that sums to our target.

#### Set and check
Create a set containing all the numbers, then for each number, check if the set
contains (2020 - x).

### Part 2:
Find three numbers in the input that sum to 2020.
