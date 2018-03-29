/**
 * data-server. 2012-6-25
 */
package com.anxlng.trial;

/**
 * 尝试直接进行byte[]数组比较速度快，还是转成 int速度快
 * @author tangjixing <anxlng@sina.com>
 */
public class ByteArrayEqualsOrPraseEquals {
	
	public static boolean arrayEquals(byte[] a1, int a1Offset, byte[] a2,
			int a2Offset, int length) {
		if(a1.length < a1Offset + length || a2.length < a2Offset + length) {
			return false;
		}
		while (length-- > 0) {
			if (a1[a1Offset++] != a2[a2Offset++]) {
				return false;
			}
		}
		return true;
	}
	
	public static boolean arrayEquals(byte[] a1, int a1Offset, byte[] a2) {
		return a1[a1Offset++] == a2[0] && a1[a1Offset] == a2[1];
	}
	
	public static boolean arrayEquals2(byte[] a1, int a1Offset, int a2) {
		if(a1.length < a1Offset + 2) {
			return false;
		}
		int ia1 = ((a1[a1Offset++] << 8) & 0xff00) | (a1[a1Offset] & 0xff);
		
		return a2 == ia1;
	}
	
	public static String toBinary(byte[] array, int offset, int len) {
		len = 2;
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < len; i++) {
			byte b = array[i + offset];

			for (int n = 7; n >= 0; n--) {
				sb.append(digits[(b >>> n) & 0x01]);
			}
		}
		return sb.toString();
	}
	
	final static char[] digits = { '0', '1', '2', '3', '4', '5', '6', '7', '8',
		'9', 'a', 'b', 'c', 'd', 'e', 'f' };

	public static void main(String[] args) {
		byte[] a1 = {(byte)0x12, (byte)0x21, (byte)0x40, (byte)0x01};
		byte[] a2 = {(byte)0x40, (byte)0x01};
		int ia2 = 0x4001;
		
		long s = System.nanoTime();
		for (int i=0; i<1000000; i++) {
			arrayEquals(a1, 2, a2);
//			boolean b1 = arrayEquals(a1, 2, a2, 0, 2);
		}
		
		long m = System.nanoTime();
		for (int i=0; i<1000000; i++) {
			arrayEquals2(a1, 2, ia2);
		}
		
		System.out.println(System.nanoTime() - m);
		System.out.println(m - s);
		boolean b1 = arrayEquals(a1, 2, a2);
		boolean b2 = arrayEquals2(a1, 2, ia2);
		System.out.println(b1);
		System.out.println(b2);
		
		byte[] bts = new byte[10];
		for (byte bt : bts) {
			System.out.print(bt);
		}
		System.out.println();
		int len = 3;
		System.out.println(toBinary(a1, 0, len));
		System.out.println(len);
	}
}
