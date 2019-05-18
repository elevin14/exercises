puts "What do you want?"
request = gets.chomp
puts "WHADDAYA MEAN \"" + request.upcase + "\"?!? YOU'RE FIRED!!"

lineWidth = 50
puts ''
puts 'Table of Contents'.center lineWidth
puts ''
puts 'Chapter 1:  Numbers '.ljust(lineWidth/2) + 'page 1'.rjust(lineWidth/2)
puts 'Chapter 2:  Letters '.ljust(lineWidth/2) + 'page 72'.rjust(lineWidth/2)
puts 'Chapter 3:  Variables  '.ljust(lineWidth/2) + 'page 118'.rjust(lineWidth/2)

