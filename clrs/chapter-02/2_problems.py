import math

# CLRS exercises from chapter 2 problems


# 2-1
# A) Insertion sort has a runtime of theta(n^2). To sort one list of length k would take theta(k^2). If there are n/k
# lists, theta(n/k*k^2)=theta(nk).
# B) Logically, the merge sort tree would only go n/k deep now instead of n deep. So, the merge part would take
# theta(n lg(n/k))
# C) Merge sort runs in theta(n lgn). For theta(nk+n lg(n/k)) to be larger than this, k > lg n.
# D) In practice, you should experiment with different sizes of k. The constants may have an undue effect on runtime
# due to the small size of k.

# An implementation of this sort:
def merge_insertion_sort(arr, k):

    def merge_sort_helper(p, r):
        if p < r and p-r//2 <= k:
            q = (p+r)//2
            insertion_sort(p,q)
            insertion_sort(q+1,r)
            merge(p,q,r)
        elif p < r:
            q = (p+r)//2
            merge_sort_helper(p,q)
            merge_sort_helper(q+1,r)
            merge(p,q,r)

    def merge(p,q,r):
        n1 = q - p + 1
        n2 = r - q
        l_arr = [0] * (n1+1)
        r_arr = [0] * (n2+1)
        for i in range(n1):
            l_arr[i] = arr[p+i]
        for j in range(n2):
            r_arr[j] = arr[q + j+1]
        l_arr[n1] = float('inf')
        r_arr[n2] = float('inf')
        i = 0
        j = 0
        for k in range(p, r+1):
            if l_arr[i] <= r_arr[j]:
                arr[k] = l_arr[i]
                i += 1
            else:
                arr[k] = r_arr[j]
                j += 1

    def insertion_sort(p, r):
        for j in range(p, r+1):
            key = arr[j]
            i = j - 1
            while i >= 0 and arr[i] > key:
                arr[i + 1] = arr[i]
                i -= 1
            arr[i + 1] = key

    merge_sort_helper(0,len(arr)-1)
    return arr


print(merge_insertion_sort([31,41,59,26,41,58], 3))

# 2-2
# A) To show bubblesort actually sorts, we'd need to show the list invariant.
# B/C) In unstructured terms, the loop invariant is that the lowest element from 1+1 to j is in j after the loop.
# Therefore the list 1 to i is sorted.
# D) Worst case runtime of bubblesort is O(n^2), for when list is in reverse order. This is the same as insertion sort.

# An implementation of bubble sort:
def bubble_sort(arr):
    for i in range(len(arr)-1):
        for j in range(len(arr)-1,i,-1):
            if arr[j] < arr[j-1]:
                arr[j], arr[j-1] = arr[j-1], arr[j]
    return arr


print(bubble_sort([31,41,59,26,41,58]))

# 2-3
# A) Assuming addition and multiplication are constant time operations, theta(n)
# B) A polynomial evaluator, input is array of coefficients. Runtime is theta(n^2) assuming power is runtime theta(n)
def poly_eval(arr, x):
    y = 0
    for i in range(len(arr)):
        y += arr[i] * x ** i
    return y
# C/D) y = a_i+1 * 1 + a_i+2 * x^1...a_k+n * x^(n-(i+1))

# An implementation of Horner's rule:
def horner(arr,x):
    y = 0
    for i  in range(len(arr)-1,-1,-1):
        y = arr[i] + x*y
    return y

print(poly_eval([5,6,4,3], 3))
print(horner([5,6,4,3], 3))


# 2-4
# A) Inverstions of (2,3,8,6,1) are (8,6), (8,1), (2,1), (3,1), (6,1)
# B) Most inverstions would be a reverse ordered list. It would have n(n+1)/2=theta(n^2) inversions.
# C) Insertion sort is directly propertional to inversions present. Each inversion is one operation of the sort.
# D) Algorithm to find inversions in theta(n lgn)
def inversion_finder(arr):

    inversions = 0

    def merge_sort_helper(p,r):
        if p < r:
            q = (p+r)//2
            merge_sort_helper(p,q)
            merge_sort_helper(q+1,r)
            merge(p,q,r)

    def merge(p,q,r):
        n1 = q - p + 1
        n2 = r - q
        l_arr = [0] * (n1+1)
        r_arr = [0] * (n2+1)
        for i in range(n1):
            l_arr[i] = arr[p+i]
        for j in range(n2):
            r_arr[j] = arr[q + j+1]
        l_arr[n1] = float('inf')
        r_arr[n2] = float('inf')
        i = 0
        j = 0
        for k in range(p, r+1):
            if l_arr[i] <= r_arr[j]:
                arr[k] = l_arr[i]
                i += 1
            else:
                arr[k] = r_arr[j]
                j += 1
                nonlocal inversions
                inversions+= 1
        return arr

    merge_sort_helper(0,len(arr)-1)
    return inversions

print(inversion_finder([2,3,8,6,1]))

