/**
 * Manage unique numbers which will be added to if-else labels to differentiate each if-else statement
 */

package davs.mini.jvm.lang;

import java.util.Stack;

public class IfElse
	{
		
		private int magicNumber; //Numbers generated which will be associated with each if-else statement
		private int currentMagicNumber; //Specific number associated with specific if-else statement
		private Stack<Integer> ifsIds; //Holds all if-else identifiers
		private Stack<Boolean> elses; //Is else present for each if-else call
		
		public IfElse()
			{
				magicNumber = 0;
				currentMagicNumber = magicNumber;
				ifsIds = new Stack<Integer>();
				elses = new Stack<Boolean>();
			}
		
		public void pushIfElse(Boolean isElsePresent)
			{
				magicNumber++;
				ifsIds.push(magicNumber);
				currentMagicNumber = magicNumber;
				elses.push(isElsePresent);
			}
		
		public void popIfElse()
			{
				currentMagicNumber = ifsIds.pop();
				elses.pop();
			}
		
		public int getIfElseMagicNumber()
			{
				return currentMagicNumber;
			}
		
		public boolean isElsePresent()
			{
				return elses.peek();
			}

	}
