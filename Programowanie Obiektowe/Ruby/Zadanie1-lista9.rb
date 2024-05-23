=begin
Kacper Jodlowski
Lista 9
Zadanie 1
=end

class Function 
    def initialize (&block)
        @func = block
        
    end
    def value (x)
        return @func.call(x)
    end
    def zero (a,b,e)
        l = a
        p = b

        if (value(a) * value(b) > 0)
            return nil
        end

        mid = (l+p)/2.0
        midV = value(mid)

        while (l-p).abs>e
            if midV*value(b)>0 then p = mid else l = mid end
            mid = (l+p)/2.0
            midV = value(mid)
            
        end
        return mid
    end
    def field(a,b)
        suma = 0
        dokl = 1000000
        delta = (b-a)/dokl.to_f
        for i in 1..dokl
            suma += delta*value(a+i*delta)
        end
        return suma
    end
    def deriv(x)
        delta = 0.00001
        return (value(x+delta) - value(x)) / delta
    end

end


f = Function.new{ | x | x*x*Math.sin(x) }

puts "Wartość funkcji dla x = 2: #{f.value(2)}"
puts "Miejsce zerowe funkcji w przedziale [1, 5]: #{f.zero(1, 5, 0.0001)}"
puts "Pole powierzchni między wykresem a osią OX w przedziale [0, 3]: #{f.field(0, 3)}"
puts "Pochodna w punkcie x = 1: #{f.deriv(1)}"
