=begin
Kacper Jodlowski
Lista 10
Zadanie 2
=end

class Node 
    def initialize(input)
        @data = input
        @next_node = nil
        @prev_node = nil
    end
    def set_next_node(node)
        @next_node = node
    end
    def set_prev_node(node)
        @prev_node = node
    end
    def get_value()
        return @data
    end
    def clear_connections()
        @next_node = nil
        @prev_node = nil
    end
    def next()
        return @next_node
    end
    def prev()
        return @prev_node
    end 
end

class Kolekcja 
    def initialize()
        @head = nil
        @tail = nil
    end
    def is_empty()
        if @head == nil then return true else return false end
    end
    def add_empty(node)
        if self.is_empty() then 
            @head = node
            @tail = node
        end
    end
    def connect_nodes_Left_Right(node1,node2)
        node1.set_next_node(node2)
        node2.set_prev_node(node1)
    end
    def add_end_node(node)
        node.clear_connections()
        if self.is_empty() then
            self.add_empty()
            return
        end
        self.connect_nodes_Left_Right(@tail,node)
        @tail = node
    end
    def add_start_node(node)
        node.clear_connections()
        if self.is_empty() then
            self.add_empty(node)
            return
        end
        self.connect_nodes_Left_Right(node,@head)
        @head = node
    end
    def add_end_value(value)
        node = Node.new(value)
        self.add_end_node(node)
    end
    def add_start_value(value)
        node = Node.new(value)
        self.add_start_node(node)
    end
    def add_sort_node(node)
        node.clear_connections()
        if self.is_empty() then
            self.add_empty(node)
            return
        end
        if node.get_value() > @tail.get_value() then
            self.add_end_node(node)
            return
        end
        if node.get_value() < @head.get_value() then
            self.add_start_node(node) 
            return
        end
        tempnode = @head
        while tempnode.get_value() < node.get_value()
            tempnode = tempnode.next() 
        end
        self.connect_nodes_Left_Right(tempnode.prev,node)
        self.connect_nodes_Left_Right(node,tempnode)
    end
    def add_sort_value(value)
        node = Node.new(value)
        self.add_sort_node(node)
    end
    def print()
        tempnode = @head
        while tempnode!=nil 
            puts(tempnode.get_value())
            tempnode=tempnode.next()
        end
    end
end


kol = Kolekcja.new()
kol.add_sort_value(5)
kol.add_sort_value(3)
kol.add_sort_value(8)
kol.add_sort_value(1)
kol.add_sort_value(7)
kol.add_sort_value(4)
kol.add_sort_value(9)
kol.add_sort_value(2)
kol.add_sort_value(6)
kol.print()