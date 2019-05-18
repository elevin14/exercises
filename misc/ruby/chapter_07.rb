word = gets.chomp
words = []
while word != ''
    words.push word
    word = gets.chomp
end
sorted = []
while words != []
    min = words[0]
    index = 0
    words.each_with_index do |word2,i|
        if min > word2
            index = i
            min = word2
        end
    end
    sorted.push min
    words.delete_at(index)
end    
puts sorted