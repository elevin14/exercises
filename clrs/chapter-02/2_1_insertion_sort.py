import math

# CLRS exercises from chapter 2.1


# Insertion sort translated from psuedo code on page 18. Credit to CLRS.
def insertion_sort(arr):
    for j in range(1,len(arr)):
        key = arr[j]
        i = j -1
        while i >= 0 and arr[i] > key:
            arr[i + 1] = arr[i]
            i -= 1
        arr[i + 1] = key
    return arr


print(insertion_sort([6,3,6,7,1,6]))
print(insertion_sort([31,41,59,26,41,58]))


# 2.1-2, nonincreasing insertion sort
def nonincreasing_insertion_sort(arr):
    for j in range(1,len(arr)):
        key = arr[j]
        i = j -1
        while i >= 0 and arr[i] < key:
            arr[i + 1] = arr[i]
            i -= 1
        arr[i + 1] = key
    return arr


print(nonincreasing_insertion_sort([31,41,59,26,41,58]))


# 2.1-3
# Implement a linear search
# Initilization: output is nil because no matches before index 0, which is an empty set
# Maintanance: If no values have been found, output is still nil. If value has been found, out is index.
#              If value is found in loop, then out is updated to that index.
# Termination: Return value is given. Output is either nil or value of index
def linear_search(arr, v):
    for i in range(0,len(arr)):
        if arr[i] == v:
            return i
    return None

print(linear_search([1,4,6,3,2],3))
print(linear_search([1,4,6,3,2],8))

# 2.1-4
# Implement binary addition
# Initilization: Output is empty because the array after index length of array is empty, so there are no numbers
#   to add.
# Maintanance: The last i values in C contain the last i-1 values of A and B added together. In the loop, we save a
#   potential carry digit. Then the ith+1 value of C is updated by adding A and B[i]. Finally, the ith value of C is
#   updated with the carried amount. After this update, the last i+1 values of C represent the last ith values of A
#   and B added together.
# Termination: The loop terminates when all values in A and B have been iterated through. The i+1 values in C are the
#   full addtions of A and B
def add_binary_integers(A,B):
    C = [0]*(len(A)+1)
    for i in range(len(A)-1,-1,-1):
        carry = (C[i+1] + A[i] + B[i]) // 2
        C[i+1] = (C[i+1] + A[i] + B[i]) % 2
        C[i] = carry
    return C


print(add_binary_integers([1,1,1,1],[1,1,1,1]))