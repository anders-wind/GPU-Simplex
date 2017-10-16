import java.util.ArrayList;
import java.util.List;

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
            int numberOfInstances = 1;
            if(args.length>2)
                numberOfInstances = Integer.parseInt(args[2]);

            InstanceGenerator instanceGenerator = new InstanceGenerator();
            List<SimplexInstance> instances = new ArrayList<>();
            for (int i = 0; i < numberOfInstances; i++) {
                instances.add(instanceGenerator.generate(variableNumber, constraintNumber));
            }

            for (SimplexInstance instance : instances) {
                System.out.println(instance);
            }
        }
        catch (NumberFormatException e) {
            System.err.println("Arguments were not integers");
        }
    }
}
