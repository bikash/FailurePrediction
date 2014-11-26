/******************
 * Author: Bikash Agrawal
 * Date: 25th Nov 2014
 * Email: er.bikash21@gmail.com
 * Description: Hidden Markov Model
 */

import java.util.List;
import org.apache.log4j.Logger;
import org.chombo.util.DoubleTable;
import org.chombo.util.Utility;


public class HiddenMarkovModel {
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
	public HiddenMarkovModel(List<String> lines, Logger LOG) {
		HiddenMarkovModel.LOG = LOG;
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
	
	/**
	 * @param stateIndx
	 * @return
	 */
	public double getIntialStateProbability(int stateIndx) {
		return intialStateProb[stateIndx];
	}
	
	/**
	 * @param stateIndx
	 * @return
	 */
	public double[] getAllDestStateProbility(int stateIndx) {
		return stateTransitionProb.getRow(stateIndx);
	}

	/**
	 * @param srcStateIndx
	 * @param dstStateIndx
	 * @return
	 */
	public double getDestStateProbility(int srcStateIndx, int dstStateIndx) {
		return stateTransitionProb.get(srcStateIndx, dstStateIndx);
	}

	/**
	 * @param stateIndx
	 * @return
	 */
	public double[] getObservationProbility(int stateIndx) {
		return stateObservationProb.getRow(stateIndx);
	}

	/**
	 * @param state
	 * @param observation
	 * @return
	 */
	public double getObservationProbabiility(int stateIndx, int observationIndx) {
		return stateObservationProb.get(stateIndx,  observationIndx);
	}

	/**
	 * @param observation
	 * @return
	 */
	public int getObservationIndex(String observation) {
		int indx = 0;
		boolean found = false;
		for (String obs : observations) {
			if (obs.equals(observation)) {
				found = true;
				break;
			}
			++indx;
		}
		return found? indx : -1;
	}
	
	/**
	 * @param indx
	 * @return
	 */
	public String getState(int indx) {
		return states[indx];
	}

	public int getNumStates() {
		return numStates;
	}

	public int getNumObservations() {
		return numObservations;
	}
}