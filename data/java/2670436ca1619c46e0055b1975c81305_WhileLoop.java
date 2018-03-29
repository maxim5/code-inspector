/**
 * Manage unique numbers which will be added to while loop labels to differentiate each while loop
 */

package davs.mini.jvm.lang;

import java.util.Stack;

public class WhileLoop
	{
		
		private int magicNumber; //Numbers generated which will be associated with each while loop
		private int currentMagicNumber; //Specific number associated with specific while loop
		private Stack<Integer> whileLoopsIds; //Holds all while loop identifiers
		
		public WhileLoop()
			{
				magicNumber = 0;
				currentMagicNumber = magicNumber;
				whileLoopsIds = new Stack<Integer>();
			}
		
		public void enterWhile()
			{
				magicNumber++;
				whileLoopsIds.push(magicNumber);
				currentMagicNumber = magicNumber;
			}
		
		public void popWhile()
			{
				currentMagicNumber = whileLoopsIds.pop();
			}
		
		public int getWhileMagicNumber()
			{
				return currentMagicNumber;
			}

	}
