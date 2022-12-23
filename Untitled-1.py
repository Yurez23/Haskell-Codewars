def find_unique(arr):
    # Initialize a variable to store the XOR of all elements
    xor = 0
    
    # XOR all elements in the array
    for element in arr:
        xor ^= element
    
    # Find the rightmost set bit in the XOR result
    rightmost_set_bit = xor & -xor
    
    # Initialize variables to store the two unique elements
    unique1 = 0
    unique2 = 0
    
    # Divide the array into two groups based on the rightmost set bit
    for element in arr:
        if element & rightmost_set_bit:
            unique1 ^= element
        else:
            unique2 ^= element
    
    return [unique1, unique2]

print(find_unique([1,2,3,5,2,1]))