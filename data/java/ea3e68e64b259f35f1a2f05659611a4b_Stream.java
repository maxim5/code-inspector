/*
 * Stream.java February 2001
 *
 * Copyright (C) 2001, Niall Gallagher <niallg@users.sf.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or 
 * implied. See the License for the specific language governing 
 * permissions and limitations under the License.
 */

package org.simpleframework.util.buffer;

import java.io.IOException;
import java.io.InputStream;

/**
 * The <code>Stream</code> interface is used to represent anything that
 * can be streamed. Typically this is used to represent a region of
 * memory that can be read through an <code>InputStream</code> object.
 * Representing an object as a stream ensures it can each time the 
 * input stream is acquired it reads from the start of the buffer.
 *
 * @author Niall Gallagher
 *
 * @see org.simpleframework.util.buffer.Buffer
 */ 
public interface Stream {

   /**
    * This method is used so that a buffer can be represented as a
    * stream of bytes. This provides a quick means to access the data
    * that has been written to the buffer. It wraps the buffer within
    * an input stream so that it can be read directly.
    *
    * @return a stream that can be used to read the buffered bytes
    */   
  public InputStream getInputStream() throws IOException;        
}
