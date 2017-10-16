public class Program {

    /***
     * Expects two integers in args as input.
     * Prints coefficients, constants, constraints, initial objective value, expected objective value.
     * @param args
     */
    public static void main(String[] args){
        if(args.length<2)
        {
            System.err.println("Provide two arguments, number of variables and number of constraints");
            return;
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
