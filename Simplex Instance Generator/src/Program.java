public class Program {
    
    public static void main(String[] args){
        InstanceGenerator instanceGenerator = new InstanceGenerator();
        Instance instance = instanceGenerator.generate(2, 2);

        System.out.println(instance);
    }
}
