birth = Time.mktime(1990,3,14,2,0)
puts birth
puts (birth + 1000000000).to_s

class OrangeTree
    
    def initialize
        @height = 0
        @age = 0
        @oranges = 0
    end

    def height
        @height
    end

    def countTheOranges
        puts @oranges        
    end

    def pickAnOrange
        if @oranges > 0
            @oranges -= 1  
            puts "Mmmmm, good orange"
        else
            puts "There are no more oranges this year"
        end
      
    end

    def oneYearPasses
        @age += 1
        @height += 1
        if @age > 5
            @oranges = 4 + @age * 2
        end
        if @age > 100
            exit
        end
    end

end

tree = OrangeTree.new
6.times do
    tree.oneYearPasses
end
20.times do
    tree.pickAnOrange
end
tree.oneYearPasses
tree.countTheOranges