import math

# CLRS exercises from chapter 2.3


# Merge sort translated from psuedo code, credit to CLRS.
# Modifications made to adopt to python.
def merge_sort(arr):

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
        return arr

    merge_sort_helper(0,len(arr)-1)
    return arr


print(merge_sort([6,3,6,7,1,6]))
print(merge_sort([31,41,59,26,41,58]))


# 2.3-2 Merge sort without sentinels:
def merge_sort2(arr):

    def merge_sort_helper(p,r):
        if p < r:
            q = (p+r)//2
            merge_sort_helper(p,q)
            merge_sort_helper(q+1,r)
            merge(p,q,r)

    def merge(p,q,r):
        n1 = q - p + 1
        n2 = r - q
        l_arr = [0] * n1
        r_arr = [0] * n2
        for i in range(n1):
            l_arr[i] = arr[p+i]
        for j in range(n2):
            r_arr[j] = arr[q + j+1]
        i = 0
        j = 0
        for k in range(p, r+1):
            if i >= n1:
                arr[k:r+1] = r_arr[j:]
                break
            elif j >= n2:
                arr[k:r+1] = l_arr[i:]
                break
            elif l_arr[i] <= r_arr[j]:
                arr[k] = l_arr[i]
                i += 1
            else:
                arr[k] = r_arr[j]
                j += 1
        return arr

    merge_sort_helper(0,len(arr)-1)
    return arr


print(merge_sort2([6,3,6,7,1,6]))
print(merge_sort2([31,41,59,26,41,58]))
print(merge_sort2([1,2,3,4,5,4]))


# 2.3-4 Insertion sort as recursive procedure.
# T(n) = theta(1) if n = 0
# T(n) = T(n-1)+n-1 if n > 0
def insertion_sort(arr):

    def insertion_sort_helper(n):
        if n > 0:
            insertion_sort_helper(n-1)
            for i in range(n-1):
                if arr[n-1] < arr[i]:
                    arr.insert(i,arr[n-1])
                    arr.pop()

    insertion_sort_helper(len(arr))
    return arr


print(insertion_sort([6,3,6,7,1,6]))
print(insertion_sort([31,41,59,26,41,58]))
print(insertion_sort([1,2,3,4,5,4]))


# 2.3-5 Binary search
# A worst case is if the value is not in the array. In that case, the procedure continually halves the range until one
# value is left. It takes lg(n) iterations to do this.
def binary_search(arr, v):

    def binary_search_helper(a, b):
        if a != b:
            midpoint = (b+a)//2
            if arr[midpoint] == v:
                return midpoint
            elif arr[midpoint] > v:
                binary_search_helper(a, midpoint-1)
            else:
                binary_search_helper(midpoint+1, b)

    return binary_search_helper(0, len(arr)-1)


print(binary_search([1,2,3,4,6],3))
print(binary_search([1,2,3,4,6],8))


# 2.3-6 Assuming we can use methods to bulk reassign array values, using a binary search approach could improve
# insertion sort. However, if we must move elements one at a time, there will be no improvement in insertion sort.


# 2.3-7 First use merge sort to sort S theta(n lgn). Then iterate through the array. For each element, use binary search
# to see if there exists an element x - A[i]. This is n applications of the binary search, so theta(n lgn). Adding those
# together, for a total of theta(n lgn).

# Easier to understand merge sort, not as space efficient.
def merge_sort3(arr):

    def merge(arr1,arr2):
        total_len = len(arr1) + len(arr2)
        arr1.append(float('inf'))
        arr2.append(float('inf'))
        i, j = 0, 0
        arr_out = [0] * total_len
        for k in range(total_len):
            if arr1[i] < arr2[j]:
                arr_out[k] = arr1[i]
                i += 1
            else:
                arr_out[k] = arr2[j]
                j += 1
        return arr_out


    if len(arr) <= 1:
        return arr
    else:
        p = len(arr) // 2
        return merge(merge_sort(arr[:p]), merge_sort(arr[p:]))


print(merge_sort3([6,3,6,7,1,6]))
print(merge_sort3([31,41,59,26,41,58]))