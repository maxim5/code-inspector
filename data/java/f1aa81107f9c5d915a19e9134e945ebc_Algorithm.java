package hu.belicza.andras.factorization.algorithm;

import java.math.BigInteger;

/**
 * Defines an abstract class for controllable, monitorable algorithms to get a factor of a number.
 * 
 * @author Andras Belicza
 */
public abstract class Algorithm {
	
	/** The overall execution time of the algorithm (handled respect to resume, suspention) in nanoseconds. */
	protected long executionTimeNanos;
	/** The system time nanos when the execution time was last updated. */
	protected long executionTimeLastUpdated;
	
	/** Initial content of the state builder (which doesn't change). */
	private String stateBuilderInitialContent;
	
	/** A request for the current status is signed by setting this variable to true. */
	protected volatile boolean        requestingStatus;
	/** This is where the algorithm state will be stored.                            */
	protected volatile AlgorithmState algorithmStateSnapshot;
	/** Flag to tell if the algorithm is completed.                                  */
	protected volatile boolean        completed;
	/** A request for suspension is signed by setting this variable to true.         */
	protected volatile boolean        requestingSuspension;
	/** A requesting to stop is signed by setting this variable to true.             */
	protected volatile boolean        requestingStop;
	/** A request for resume is signed by setting this variable to true.             */
	protected volatile boolean        requestingResume;
	
	/**
	 * Returns a factor of <code>n</code>.<br>
	 * This method handles the calculation initialization and deinitialization.
	 * @param n <code>n</code> to be analysed
	 * @return a factor of <code>n</code> or either <code>n</code> or 1 if <code>n</code> is a prime
	 */
	public final BigInteger getFactor( final BigInteger n ) {
		stateBuilderInitialContent = "algorithmClass=" + getClass().getName() + "\nn=" + n + "\n";
		executionTimeLastUpdated = System.nanoTime();
		
		final BigInteger factor = getFactorImpl( n );
		
		createStateSnapshot( 1.0f, createStateBuilder() );
		completed = true;
		
		return factor;
	}
	
	/**
	 * The implementation of the factorization of <code>n</code>.
	 * @param n <code>n</code> to be analysed
	 * @return a factor of <code>n</code> or either <code>n</code> or 1 if <code>n</code> is a prime
	 */
	protected abstract BigInteger getFactorImpl( final BigInteger n );
	
	/**
	 * Returns the current state of the algorithm.
	 * @return the current state of the algorithm
	 */
	public synchronized AlgorithmState getState() {
		if ( completed )
			return algorithmStateSnapshot;
		
		requestingStatus = true;
		try {
			while ( requestingStatus )
				wait();
			
			return algorithmStateSnapshot;
		} catch ( final InterruptedException ie ) {
			ie.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Creates and returns a state builder (which is {@link StringBuilder}.
	 * with basic properties loaded into it.
	 * @return a state builder
	 */
	protected StringBuilder createStateBuilder() {
		return new StringBuilder( stateBuilderInitialContent );
	}
	
	/**
	 * Creates a snapshot of the current state of the algorithm.
	 * @param completionState completion state of the algorithm
	 * @param stateBuilder builder to be used to build the string representation of the state
	 */
	protected void createStateSnapshot( final float completionState, final StringBuilder stateBuilder ) {
		recalculateExecutinTime();
		stateBuilder.append( "executionTimeNanos=" ).append( executionTimeNanos );
		
		algorithmStateSnapshot = new AlgorithmState( executionTimeNanos, completionState, stateBuilder.toString() );
		requestingStatus = false;
		synchronized ( this ) {
			notifyAll();
		}
	}
	
	/**
	 * Recalculates the current execution time.
	 */
	private void recalculateExecutinTime() {
		final long currentNanoTime = System.nanoTime();
		executionTimeNanos += currentNanoTime - executionTimeLastUpdated;
		executionTimeLastUpdated = currentNanoTime;
	}
	
	/**
	 * Suspends the execution of the algorithm.
	 */
	public synchronized void suspend() {
		requestingSuspension = true;
		try {
			while ( requestingSuspension )
				wait();
			
			// Execution time will not be measured during suspention.
			recalculateExecutinTime();
		} catch ( final InterruptedException ie ) {
			ie.printStackTrace();
		}
	}
	
	/**
	 * Acknowledges the suspention and the algorithm will stay here while suspended.
	 */
	protected synchronized void suspended() {
		requestingSuspension = false;
		notifyAll();
		try {
			while ( !requestingResume )
				wait();
			requestingResume = false;
			notifyAll();
		} catch ( final InterruptedException ie ) {
			ie.printStackTrace();
		}
	}
	
	/**
	 * Resumes the execution of the algorithm.
	 */
	public synchronized void resume() {
		requestingResume = true;
		notify();
		try {
			while ( requestingResume )
				wait();
			
			// Restart execution time measurement from here.
			executionTimeLastUpdated = System.nanoTime();
		} catch ( final InterruptedException ie ) {
			ie.printStackTrace();
		}
	}
	
	/**
	 * Stops the execution of the algorithm.
	 */
	public synchronized void stop() {
		requestingStop = true;
		try {
			while ( requestingStop )
				wait();
		} catch ( final InterruptedException ie ) {
			ie.printStackTrace();
		}
	}
	
	/**
	 * Acknowledges that stop has been executed.  
	 */
	protected synchronized void stopping() {
		requestingStop = false;
		notifyAll();
	}
	
}
