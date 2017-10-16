public class Program {
    
    public static void main(String[] args){
        if(args.length<2)
        {
            System.err.println("Provide two arguments, number of variables and number of constraints");
        }
        try{
            int variableNumber = Integer.parseInt(args[0]);
            int constraintNumber = Integer.parseInt(args[1]);

            InstanceGenerator instanceGenerator = new InstanceGenerator();
            SimplexInstance simplexInstance = instanceGenerator.generate(variableNumber, constraintNumber);

            System.out.println(simplexInstance);
        }
        catch (NumberFormatException e) {
            System.err.println("Arguments were not integers");
        }
    }
}
