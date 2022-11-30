# Timing Results

## Part 1

Enter the timing data for each of the following program. Place the
"real time" measurement in the specified place at the end of the line.

(It the program takes longer than 5 minutes you can terminate its
execution and just indicate "more than 5 minutes" as the time.)

- `time_HW_Lists_Sorted_Words.ml` real time is:         10m46.167s

- `time_HW_Lists_Unsorted_Words.ml` real time is:       9m26.402s

- `time_HW_BSTree_Sorted_Words.ml` real time is:        9m20.425s

- `time_HW_BSTree_Unsorted_Words.ml` real time is:      1m3.115s

- `time_HW_RBTree_Sorted_Words.ml` real time is:        0m53.145s

- `time_HW_RBTree_Unsorted_Words.ml` real time is:      1m0.610s

- `time_HW_TR_Lists_Sorted.ml` real time is:            9m19.785s

- `time_HW_TR_Lists_Unsorted.ml` real time is:          10m54.048s

- `time_HW_TR_BSTree_Sorted.ml` real time is:           12m20.840s

- `time_HW_TR_BSTree_Unsorted.ml` real time is:         1m8.618s

- `time_HW_TR_RBTree_Sorted.ml` real time is:           0m50.317s

- `time_HW_TR_RBTree_Unsorted.ml` real time is:         1m1.265s

## Part 2

Write your answer to the following questions just below the question.

### Question 1

Explain why is the time for `time_HW_Lists_Sorted_Words.ml` so similar to 
the time for `time_HW_Lists_Unsorted_Words.ml`. 

it does not matter if the words are sorted or unsorted since a list based set
inserts each word the into a list the same way.

### Question 2

Explain why is the time for `time_HW_BSTree_Sorted_Words.ml` different from
`time_HW_BSTree_Unsorted_Words.ml`. 

the size is equal but the height would be different.
BStrees do not keep balance with every insertion making it 
unbalanced.

### Question 3

Explain why is the time for `time_HW_RBTree_Sorted_Words.ml` so similar to 
the time for `time_HW_RBTree_Unsorted_Words.ml`. 

the order does not matter since the RBTree always keeps balance, 
therefor it will take the same amount of time to reach the end of both 
child trees.

### Question 4

How different are the times for `time_HW_RBTree_Sorted_Words.ml` and
`time_HW_TR_RBTree_Sorted_Words.ml`?  Explain why these times are
related in this way.

`time_HW_TR_RBTree_Sorted_Words.ml` is faster by 3 seconds. It is faster because
tail recursion uses less memorry in the stack which makes it faster than regular recursion.

### Quetions 5

Given the effort to write tail recursive functions, does this
optimization seem worth the effort?

in general the optimization would be worth it but in the case of my impementation of 
tail recursive functions, it is not. I may have implemented the tail recursion wrong.
I do not know why the tests are really similar