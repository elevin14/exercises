puts "Hi, what is your first name?"
first = gets.chomp
puts "And your middle name?"
middle = gets.chomp
puts "Finally, what is your last name?"
last = gets.chomp
puts "Pleased to meet you #{first} #{middle} #{last}."

puts "What is your favorite number?"
favorite = gets.chomp
puts "Isn't #{favorite.to_i+1} a better number?"