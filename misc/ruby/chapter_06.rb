beer_bottles = 99
while beer_bottles > 0
    puts beer_bottles.to_s + ' bottles of beer on the wall'
    beer_bottles -= 1
end

puts "What is the starting year?"
year = gets.chomp.to_i
puts "What is the ending year?"
end_year = gets.chomp.to_i
while year <= end_year
    if year % 4 == 0 and (year % 100 != 0 or year%400 == 0)
        puts year
    end
    year += 1
end


byes_in_a_row = 0
while byes_in_a_row < 3
    grandson = gets.chomp
    if grandson == grandson.upcase
        year = rand(21) + 1930
        puts 'NO, NOT SINCE ' + year.to_s + '!'
        if grandson == 'BYE'
            byes_in_a_row += 1
        else
            byes_in_a_row = 0
        end
    else
        puts 'HUH?! SPEAK UP, SONNY!'
        byes_in_a_row = 0
    end
end

