import java.util.List;

import org.apache.log4j.Logger;
import org.chombo.util.DoubleTable;
import org.chombo.util.Utility;

/*
 * FP-HMM: Failure Prediction Analysis for Hadoop Cluster
 * Author: Bikash Agrawal
 * Organization: University of Stavanger
 * Description: EM algorithm to update likelihood and predict output sequences. 
 * 
 */
public class EM {
	private String[] states;
	private String[] observations;
	private DoubleTable stateTransitionProb;
	private DoubleTable stateObservationProb;
	private double[] intialStateProb;
	private int numStates;
	private int numObservations;
	private static final  String DELIM = ",";
	private static Logger LOG;
	/**
	 * @param states
	 * @param observations
	 */
	public EM(List<String> lines, Logger LOG) {
		EM.LOG = LOG;
		int count = 0;
		states = lines.get(count++).split(DELIM);
		observations = lines.get(count++).split(DELIM);
		numStates = states.length;
		numObservations = observations.length;
		LOG.debug("numStates:" + numStates + " numObservations:" + numObservations);
		
		
		//state transition probablity
		stateTransitionProb = new DoubleTable(numStates, numStates);
		for (int i = 0; i < numStates; ++i) {
			stateTransitionProb.deseralizeRow(lines.get(count++), i);
		}
		
		//state observation probability
		stateObservationProb = new DoubleTable(numStates, numObservations);
		for (int i = 0; i < numStates; ++i) {
			stateObservationProb.deseralizeRow(lines.get(count++), i);
		}
		
		//initial state probility
		intialStateProb =  Utility.doubleArrayFromString(lines.get(count++), DELIM);
	}
	
}
