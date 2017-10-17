import ilog.concert.IloException;
import ilog.concert.IloLinearNumExpr;
import ilog.concert.IloNumVar;
import ilog.cplex.IloCplex;

public class SimplexSolver {

    public float solveSimplex(SimplexInstance instance) {
        float expectedObjective = 0;
        int variableNumber = instance.getVariableNumber(), constraintNumber = instance.getConstrainNumber();
        try {
            IloCplex cplex = new IloCplex();
            cplex.setOut(null);
            IloNumVar[] x =  cplex.numVarArray(variableNumber,0,100);

            // Add the objective function
            IloLinearNumExpr obj = cplex.linearNumExpr();
            for (int i = 0; i < variableNumber; i++) {
                obj.addTerm(instance.coefficients[i], x[i]);
            }
            cplex.addMaximize(obj);

            // Add the constraints
            IloLinearNumExpr expr;
            for (int i = 0; i < constraintNumber; i++) {
                expr = cplex.linearNumExpr();
                for (int j = 0; j < variableNumber; j++) {
                    expr.addTerm(instance.constraints[i][j], x[j]);
                }
                cplex.addLe(expr, instance.constants[i]);
            }

            // Solve the formulation
            boolean solved = cplex.solve();
            if(solved){
                if(cplex.getCplexStatus().equals(IloCplex.CplexStatus.Infeasible))
                    System.err.println("Was infeasible");
                else if(cplex.getCplexStatus().equals(IloCplex.CplexStatus.Unbounded))
                    System.err.println("Was unbounded");
                expectedObjective += cplex.getObjValue();
            }

        } catch (IloException e) {
            e.printStackTrace();
        }
        return expectedObjective;
    }
}
