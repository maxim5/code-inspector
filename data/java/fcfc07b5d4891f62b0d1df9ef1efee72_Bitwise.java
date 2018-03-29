/**
 * Manage bitwise operations in a byte or array of bytes.
 *
 * Unit tests are in {@see TestBitwise}. See TestBitwise.java.
 */
public class Bitwise {
    private static final int bitmasks[] = {128, 64, 32, 16, 8, 4, 2, 1};

    /**
     * Check to see if bit i is set in byte. Returns true if it is
     * set, false otherwise.
     */
    public static boolean isset(int i, byte b) {
        return (b & bitmasks[i]) != 0;
    }

    /**
     * Check to see if bit i is set in array of bytes. Returns true if
     * it is set, false otherwise.
     */
    public static boolean isset(int i, byte bytes[]) {
        return isset(i % 8, bytes[i / 8]);
    }

    /**
     * Set bit i in byte and return the new byte.
     */
    public static byte set(int i, byte b) {
    	return (byte) (b | bitmasks[i]); // FIXME
    }

    /**
     * Set bit i in array of bytes.
     */
    public static void set(int i, byte bytes[]) {
        int index = i / 8;
        int biti = i % 8;
        bytes[index] = set(biti, bytes[index]);
    }

    /**
     * Clear bit i in byte and return the new byte.
     */
    public static byte clear(int i, byte b) {
    	byte compare = (byte) ~bitmasks[i];
    	return (byte) (b & compare); // FIXME
    }

    /**
     * Clear bit i in array of bytes and return true if the bit was 1
     * before clearing, false otherwise.
     */
    public static boolean clear(int i, byte bytes[]) {
        int index = i / 8;
        int biti = i % 8;
        if(isset(i, bytes)) {
        	bytes[index] = clear(biti, bytes[index]);
        	return true;
        }
        else
        	return false; // FIXME
    }

    /**
     * Clear every bit in array of bytes.
     *
     * There is no clearAll for a single byte, you can just get a new
     * byte for that.
     */
    public static void clearAll(byte bytes[]) {
        for(int i = 0; i < bytes.length; ++i) {
            bytes[i] = 0;
        }
    }

    /**
     * Convert byte to a string of bits. Each bit is represented as
     * "0" if it is clear, "1" if it is set.
     */
    public static String toString(byte b) {
        String s = "";
        for(int i = 0; i < 8; ++i) {
            s += isset(i, b) ? "1" : "0";
        }
        return s;
    }

    /**
     * Convert array of bytes to string of bits (each byte converted
     * to a string by calling {@link #byteToString(byte b)}, every
     * byte separated by sep, every "every" bytes separated by lsep.
     */
    public static String toString(byte bytes[], String sep,
                                  String lsep, int every) {
        String s = "";
        for(int i = 0; i < bytes.length * 8; ++i) {
            if(i > 0)
                if(every > 0 && i % (8 * every) == 0)
                    s += lsep;
                else if(i % 8 == 0)
                    s += sep;
            s += isset(i, bytes) ? "1" : "0";
        }
        return s;
    }

    /**
     * Convert array of bytes to string of bits, each byte separated
     * by sep. See {@link #byteToString(byte bytes[], String sep)}.
     */
    public static String toString(byte bytes[], String sep) {
        return toString(bytes, sep, null, 0);
    }

    /**
     * Convert array of bytes to string of bits, each byte separated
     * by a comma, and every 8 bytes separated by a newline. See
     * {@link #byteToString(byte bytes[], String sep)}.
     */
    public static String toString(byte bytes[]) {
        return toString(bytes, ",", "\n", 8);
    }
}