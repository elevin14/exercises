def problem_1 n
    sum_of_mults = 0
    i = 1
    while i < n
        if i%3 == 0 or i%5 == 0
            sum_of_mults += i
        end
        i += 1
    end
    sum_of_mults
end

def problem_2 n
    sum_of_evens = 2
    fib1 = 1
    fib2 = 2
    while fib2 < n
        temp = fib2
        fib2 = fib2 + fib1
        fib1 = temp
        if fib2 % 2 == 0
            sum_of_evens += fib2
        end
    end
    sum_of_evens
end

def problem_3 n
    current_factor = 2.0
    largest_factor = 0
    while n % current_factor == 0
        largest_factor = 2
        n /= current_factor
    end
    current_factor = 3.0
    while n > 1
        while n % current_factor == 0
            largest_factor = current_factor
            n /= current_factor
        end
        current_factor += 2
    end
    largest_factor
end

puts problem_1 1000
puts problem_2 4000000
puts problem_3 600851475143