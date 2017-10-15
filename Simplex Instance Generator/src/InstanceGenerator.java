import ilog.concert.IloException;
import ilog.concert.IloLinearNumExpr;
import ilog.concert.IloNumVar;
import ilog.cplex.IloCplex;

import java.util.Random;

public class InstanceGenerator {
    private Random random = new Random();

    public Instance generate(int variableNumber, int constraintNumber){
        int[] nonBasic = new int[variableNumber];
        int[] basic = new int[constraintNumber];
        double[][] constraints = new double[constraintNumber][variableNumber];
        double[] initialCoefficients = new double[variableNumber];
        double[] initialConstants = new double[constraintNumber];


        double initialObjective = random.nextDouble()*100;
        initialize(nonBasic, basic, constraints, initialCoefficients, initialConstants);


        double expectedObjective = solveWithCplex(variableNumber, constraintNumber, constraints, initialCoefficients, initialConstants, initialObjective);

        return new Instance(nonBasic, basic, constraints, initialObjective, initialCoefficients, initialConstants, expectedObjective);
    }

    private void initialize(int[] nonBasic, int[] basic, double[][] constraints, double[] initialCoefficients, double[] initialConstants) {
        for (int i = 0; i < nonBasic.length; i++) {
            nonBasic[i] = i;
            initialCoefficients[i] = random.nextDouble() * 100;
        }
        for (int i = 0; i < basic.length; i++) {
            basic[i] = i;
            initialConstants[i] = random.nextDouble() * 100;
        }

        for (int i = 0; i < basic.length; i++) {
            for (int j = 0; j < nonBasic.length; j++) {
                constraints[i][j] = random.nextDouble() * 100;
            }
        }
    }

    private double solveWithCplex(int variableNumber, int constraintNumber, double[][] constraints, double[] initialCoefficients, double[] initialConstants, double initialObjective) {
        double expectedObjective = initialObjective;
        try {
            IloCplex cplex = new IloCplex();
            IloNumVar[] x =  cplex.boolVarArray(variableNumber);

            IloLinearNumExpr obj = cplex.linearNumExpr();
            for (int i = 0; i < variableNumber; i++) {
                obj.addTerm(initialCoefficients[i], x[i]);
            }
            cplex.addMaximize();

            IloLinearNumExpr expr;
            for (int i = 0; i < constraintNumber; i++) {
                expr = cplex.linearNumExpr();
                for (int j = 0; j < variableNumber; j++) {
                    expr.addTerm(constraints[i][j], x[j]);
                }
                cplex.le(expr, initialConstants[i]);
            }

            boolean solved = cplex.solve();
            if(solved){
                expectedObjective += cplex.getBestObjValue();
            }

        } catch (IloException e) {
            e.printStackTrace();
        }
        return expectedObjective;
    }
}
