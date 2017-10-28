import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;

public class Program {

    /***
     * Expects two integers in args as input.
     * Prints coefficients, constants, constraints, initial objective value, expected objective value.
     * @param args
     */
    public static void main(String[] args){
        if(args.length<5)
        {
            System.err.println("Usage: SimplexInstanceGenerator [instances_num] " +
                    "[var_num lower] [var_num upper] [constr_num lower] [constr_num upper]");
            return;
        }
        try{
            int numberOfInstances = Integer.parseInt(args[0]);
            int variableLowNumber = Integer.parseInt(args[1]);
            int variableHighNumber = Integer.parseInt(args[2]);
            int constraintLowNumber = Integer.parseInt(args[3]);
            int constraintHighNumber = Integer.parseInt(args[4]);

            InstanceGenerator instanceGenerator = new InstanceGenerator();
            List<SimplexInstance> instances = new ArrayList<>();
            for (int i = 0; i < numberOfInstances; i++) {
                instances.add(instanceGenerator.generate(variableLowNumber,
                            variableHighNumber, constraintLowNumber, constraintHighNumber));
            }

            SimplexSolver simplexSolver = new SimplexSolver();
            BiConsumer<List<SimplexInstance>, Benchmark.Timer> obj = ((insts, timer) ->
                insts.forEach((inst) -> {
                    float res = simplexSolver.solveSimplex(inst, timer);
                    inst.setExpectedObjective(res);
                }));

            // benchmark avg time needed to execute cplex.solve() for n instances
            Benchmark.benchmark("Simplex solver", obj, instances);

            for (SimplexInstance instance : instances) {
                System.out.println(instance);
            }
        }
        catch (NumberFormatException e) {
            System.err.println("Arguments were not integers");
        }
    }
}
