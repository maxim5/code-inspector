package edu.usc.csci578.sp2011.algorithm;

import org.w3c.dom.Document;

import java.io.PrintStream;

public interface Algorithm
{
    /**
     * Returns the name of the algorithm
     *
     * @return a string to describe the algorithm
     */
    String getName();

    /**
     * Run the algorithm to detect architectural smell given the inputs (3 DOM's for XML files). The output
     * is written to the provided PrintStream
     *
     * @param archGraphDOM      DOM of the oodt-filemgr_smellArchGraph.xml file
     * @param specifiedGraphDOM DOM of the oodt-filemgr_smellArch_specified.xml file
     * @param methodInfoDOM     DOM of the oodt-filemgr_methodInfo.xml file
     * @param output            the PrintStream to print algorithm output to
     */
    void run(Document archGraphDOM, Document specifiedGraphDOM, Document methodInfoDOM, PrintStream output);

}
