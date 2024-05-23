
/*
Kacper Jodlowski
Lista 5
Zadanie 2
*/

abstract class Expression{
    public abstract String toString();
    public abstract int evaluate();
}


class Variable extends Expression {
    private int value;
    private char znak;
    public Variable(char c,int x){
        this.value = x;
        this.znak = c;
    }
    public void setValue(int x){
        this.value=x;
    }
    public String toString(){
        return Character.toString(this.znak);
    }
    public int evaluate(){
        return value;
    }
}
class Constant extends Expression{
    private int value;
    public Constant(int x){
        this.value=x;
    }
    public String toString(){
        return Integer.toString(this.value);
    }
    public int evaluate(){
        return value;
    }
}



class Node extends Expression{

    abstract class Functor
    {
        public char znak;

        public abstract int execute(int a,int b);
        public void setZnak(char c){
            this.znak=c;
        }
    }

    class Multiply extends Functor{
        public int execute(int a,int b)
        {
            return a*b;
        }
    }
    class Add extends Functor{
        public int execute(int a,int b)
        {
            return a+b;
        }
    }
    class Substract extends Functor{
        public int execute(int a,int b)
        {
            return a-b;
        }
    }
    class Divide extends Functor{
        public int execute(int a,int b) //dont care about divide over 0 for now
        {
            return a/b;
        }
    }

    
    private Functor function;

    Expression left;
    Expression right;

    public Node(char operacja, Expression left, Expression right){
        switch (operacja) {
            case '+':
                this.function = new Add();
                break;
            case '-':
                this.function = new Substract();
                break;
            case '*':
                this.function = new Multiply();
                break;
            case '/':
                this.function = new Divide();
                break;
            default:
                break;
        }
        this.function.setZnak(operacja);
        this.left = left;
        this.right = right;
    }

    public String toString(){
        return "("+this.left.toString()+" "+this.function.znak+" "+this.right.toString()+")";
    }
    public int evaluate(){
        return this.function.execute(this.left.evaluate(), this.right.evaluate());
    }
   
}

public class Zad2{
    public static void main(String [] args){
        Expression T = new Node('+',new Constant(5),new Variable('x',4));
        System.out.println(T.evaluate());
        System.out.println(T.toString());

        Variable x = new Variable('x',0);
        Expression expr1 = new Node('+',new Constant(3), x);
        System.out.println("Expression 1: " + expr1.toString());

        System.out.println("Variable x: " + x.toString()); 
        System.out.println("Variable x: " + x.evaluate());

        x.setValue(5); 

        System.out.println("Variable x: " + x.evaluate());

        int result1 = expr1.evaluate();
        System.out.println("Result 1 (x=5): " + result1);

        Variable y = new Variable('y',0); 
        Expression expr2 = new Node('*',new Constant(3), y);
        System.out.println("Expression 2: " + expr2.toString());

        y.setValue(11);  

        int result2 = expr2.evaluate();
        System.out.println("Result 2 (y=11): " + result2);



        Variable w = new Variable('w',0);
        Variable z = new Variable('z',0);
        Expression expr = new Node('+',
                            new Node('*',new Node('+',new Constant(4), w), new Node('*',new Constant(3), z)),
                            new Node('+',new Node('*',new Constant(2), w), new Constant(1))
                        );

        System.out.println("Original expression: " + expr.toString());

        w.setValue(4);
        z.setValue(7);

        int result = expr.evaluate();
        System.out.println("Result (w=4, z=7): " + result);

        expr = new Node('+',
                            new Node('*',new Node('/',new Constant(4), w), new Node('*',new Constant(3), z)),
                            new Node('-',new Node('*',new Constant(2), w), new Constant(1))
                        );

        System.out.println("Original expression: " + expr.toString());

        w.setValue(6);
        z.setValue(10);

        result = expr.evaluate();
        System.out.println("Result (w=6, z=10): " + result);

    }
}