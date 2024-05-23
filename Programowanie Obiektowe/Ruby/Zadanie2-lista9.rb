=begin
Kacper Jodlowski
Lista 9
Zadanie 2
=end

class Function2D
    def initialize (&block)
        @func = block
        
    end
    def value (x,y)
        return @func.call(x,y)
    end
    
    def volume(a,b,c,d)
        suma = 0
        dokl = 1000
        deltaA = (b-a)/dokl.to_f
        deltaC = (d-a)/dokl.to_f
        for i in 1..dokl
            for j in 1..dokl
                suma += (deltaA*deltaC*value(a+i*deltaA,c+j*deltaC)).abs
            end
        end
        return suma
    end
    def contour_line(a, b, c, d, height, step = 0.1)
        result = []
    
        (a..b).step(step) do |x|
          (c..d).step(step) do |y|
            if (value(x, y) - height).abs < step
              result << [x, y]
            end
          end
        end
    
        result
    end
  

end


f = Function2D.new { |x, y| x * y * Math.sin(x + y) }
  
puts "Wartość funkcji dla punktu (2, 3): #{f.value(2, 3)}"
puts "Objętość między wykresem funkcji a prostokątem [0, 2] x [0, 3]: #{f.volume(0, 2, 0, 3)}"
puts "Linia konturowa dla funkcji w przedziale [0, 2] x [0, 3] dla wysokości 1: #{f.contour_line(0, 2, 0, 1, 1)}"

