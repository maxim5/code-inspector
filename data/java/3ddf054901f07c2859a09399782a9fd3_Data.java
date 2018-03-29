/*
 * Data.java
 * 
 * Copyright (c) 2010, Ralf Biedert All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 * 
 * Redistributions of source code must retain the above copyright notice, this list of
 * conditions and the following disclaimer. Redistributions in binary form must reproduce the
 * above copyright notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the author nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */
package junit.data;

import java.util.ArrayList;
import java.util.List;

/**
 * Data for the jUnit tests.
 * 
 * @author Ralf Biedert
 */
public class Data {
    /** Test data. */
    public final static String s0[] = new String[] {};

    /** Test data. */
    public final static String s1[] = new String[] { "*some.value$is\0älways<bigger);th°n\nanotérone;" };

    /** Test data. */
    public final static String s5[];
    
    /** Test data. */
    public final static String sn[];

    /** File for file tests */
    public final static String DATA_PATH = "core/tests/junit/data/";

    /** Initialize big values */
    static {
        s5 = strings(5);
        sn = strings(100000);
    }

    /**
     * Returns an array of strings
     * 
     * @param num
     * @return .
     */
    public static String[] strings(int num) {
        final List<String> strings = new ArrayList<String>();

        for (int i = 0; i < num; i++) {
            strings.add("" + i);
        }

        return strings.toArray(new String[0]);
    }
}
