package com.codetest;

import java.util.ArrayList;
import java.util.List;

import junit.framework.Assert;

import org.junit.Test;

public class TwoSigmaCodeTestJumbleSortTest {

	@Test
	public void testNull() {
		Assert.assertNull(TwoSigmaCodeTestJumbleSort.sortedOutPut(null));
	}

	public void testEmpty() {
		final List<String> input = new ArrayList<String>();

		final String inputStr = "";
		final String outputStr = "";

		input.add(inputStr);

		final List<String> output = TwoSigmaCodeTestJumbleSort.sortedOutPut(input);

		Assert.assertEquals(input.size(), output.size());
		Assert.assertEquals(outputStr, output.get(0));
	}

	@Test
	public void testStringSort() {
		final List<String> input = new ArrayList<String>();

		final String inputStr = "car truck 8 4 bus 6 1";
		final String outputStr = "bus car 1 4 truck 6 8";

		input.add(inputStr);

		final List<String> output = TwoSigmaCodeTestJumbleSort.sortedOutPut(input);

		Assert.assertEquals(input.size(), output.size());
		Assert.assertEquals(outputStr, output.get(0));
	}

	@Test
	public void testMultipleLines() {
		final List<String> input = new ArrayList<String>();

		final String inputStr1 = "car truck 8 4 bus 6 1";
		final String outputStr1 = "bus car 1 4 truck 6 8";

		final String inputStr2 = "i am 1268 going -836 to 275 kill you";
		final String outputStr2 = "am going -836 i 275 kill 1268 to you";
		final String inputStr3 = "hello i 18 -8 67 time sweet potato 256 rum 643";
		final String outputStr3 = "hello i -8 18 67 potato rum sweet 256 time 643";
		final String inputStr4 = "angel time -2175 try axis -235 tarjon people 098 bank";
		final String outputStr4 = "angel axis -2175 bank people -235 tarjon time 98 try";
		final String inputStr5 = "-21575 try sort jumble -283 amazing algo +876";
		final String outputStr5 = "-21575 +876 algo amazing -283 jumble sort try";
		final String inputStr6 = "";
		final String outputStr6 = "";

		input.add(inputStr1);
		input.add(inputStr2);
		input.add(inputStr3);
		input.add(inputStr4);
		input.add(inputStr5);
		input.add(inputStr6);

		final List<String> output = TwoSigmaCodeTestJumbleSort.sortedOutPut(input);

		Assert.assertEquals(input.size(), output.size());
		Assert.assertEquals(outputStr1, output.get(0));
		Assert.assertEquals(outputStr2, output.get(1));
		Assert.assertEquals(outputStr3, output.get(2));
		Assert.assertEquals(outputStr4, output.get(3));
		Assert.assertEquals(outputStr5, output.get(4));
		Assert.assertEquals(outputStr6, output.get(5));
	}
}
