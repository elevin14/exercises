import math

# CLRS exercises from chapter 2.2

# 2.2-1
# n^3... is equivalent to theta(n^3)

# 2.2-2 selection sort.
# The loop invariant is that the array before the current index holds the x lowest items.
# It only needs to run for n-1 elements because the last element will be the largest by definition.
# Best case theta(n^2). Worst case theta(n^2)
def selection_sort(arr):
    for j in range(0,len(arr)-1):
        min_val = arr[j]
        min_index = j
        for i in range(j+1,len(arr)):
            if arr[i] < min_val:
                min_val = arr[i]
                min_index = i
        arr[min_index], arr[j] = arr[j], arr[min_index]
    return arr

print(selection_sort([6,3,6,7,1,6]))
print(selection_sort([31,41,59,26,41,58]))

# 2.2-3
# For linear search, on average, it will take n/2 time to find the element. Worst case would be n (the entire array
# needs to be checked before the element is found, or the element is not in the array). Average and worst-case in theta
# notation are theta(n).

# 2.2-4
# Almost any algorithm can be modified to give a constant time return for a specific input. Thereby, that would be the
# best case run-time.