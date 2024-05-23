=begin
Kacper Jodlowski
Lista 10
Zadanie 1
=end

class Collection 
    def initialize (array)
        @arr = array
    end
    def add (elem)
        @arr << elem
    end
    def length ()
        return @arr.length()
    end
    def get (id)
        return @arr[id]
    end
    def swap(i,j)
        temp = @arr[i]
        @arr[i] = @arr[j]
        @arr[j] = temp
    end
    def print()
        @arr.each { |x| puts x}
    end
end

class Sorter 
    def sort1(kolekcja)
        did = true
        while did 
            did = false
            (0...(kolekcja.length()-1)).each do |id|
                if kolekcja.get(id)>kolekcja.get(id+1)
                then 
                    kolekcja.swap(id,id+1)
                    did = true
                end
            end
                
        end
    end
    
    def sort2(kolekcja)
        (0...kolekcja.length()).each do |i|
            min = 999999
            min_id = i
            (i...kolekcja.length()).each do |j|
                if kolekcja.get(j)<min then
                    min = kolekcja.get(j)
                    min_id = j
                end
            end 
            kolekcja.swap(i,min_id)
        end
    end
end


coll = Collection.new([3,1,4,1,5,9,2,6,5,3,5])
coll2 = Collection.new([3,1,4,1,5,9,2,6,5,3,5])
sorter = Sorter.new
puts "Kolekcja przed sort:"
coll.print()

sorter.sort1(coll)
puts "Kolekcja po sort1:"
coll.print()

sorter.sort2(coll2)
puts "Kolekcja po sort2:"
coll2.print()