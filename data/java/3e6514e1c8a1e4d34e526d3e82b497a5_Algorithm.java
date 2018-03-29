package dataanalyzer.entity;

import dataanalyzer.gui.IAlgorithmPanel;
import java.io.Serializable;
// TODO: Create an algorithm to give extended information about data sequences (min max size time etc)
/**
 * This abstract class provides the base algorithm code. It must be exended to implement
 * a custom algorithm. This class does not do any of the processing for the algorithm,
 * that is the job of AlgorithmProcess.
 * @author David Neilsen
 */
public abstract class Algorithm implements Serializable {

    /**
     * Creates an instance of AlgorithmProcess specific to the algorithm.
     * @return the AlgorithmProcess
     */
    public abstract AlgorithmProcess createProcess();

    /**
     * Returns the name of the algorithm.
     * @return the name of the algorithm
     */
    public abstract String getName();

    /**
     * Signal to dispose of any related variables the algorithm holds referance to.
     */
    public void dispose() {
    }

    /**
     * Gets the GUI for the algorthim.
     * @return an instance of IAlgorithmPanel
     */
    public IAlgorithmPanel getGUI() {
        return null;
    }

    /**
     * Collects input from the algorithms GUI.
     * @param o the object to collect
     */
    public void collectInput(Object o) {
    }

    /**
     * Returns the name of the algorithm.
     * @return the name of the algorthim
     */
    @Override
    public String toString() {
        return getName();
    }
}
