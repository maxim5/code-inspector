/************************************************************************
 * Interface.java is part of Ti4j 3.1.0 Copyright 2013 Emitrom LLC
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 **************************************************************************/
package com.emitrom.ti4j.desktop.client.network;

import java.util.ArrayList;
import java.util.List;

import com.emitrom.ti4j.core.client.JsoHelper;
import com.emitrom.ti4j.core.client.ProxyObject;
import com.google.gwt.core.client.JavaScriptObject;

public class Interface extends ProxyObject {

    protected Interface(JavaScriptObject obj) {
        jsObj = obj;
    }

    public final native String getDisplayName()/*-{
		var obj = this.@com.emitrom.ti4j.core.client.ProxyObject::getJsObj()();
		return obj.getDisplayName();
    }-*/;

    public final native IPAddress getIPAddress()/*-{
		var obj = this.@com.emitrom.ti4j.core.client.ProxyObject::getJsObj()();
		return obj.getIPAddress();
    }-*/;

    public final native String getName()/*-{
		var obj = this.@com.emitrom.ti4j.core.client.ProxyObject::getJsObj()();
		return obj.getName();
    }-*/;

    public final native IPAddress getSubnetMask()/*-{
		var obj = this.@com.emitrom.ti4j.core.client.ProxyObject::getJsObj()();
		return obj.getSubnetMask();
    }-*/;

    public final native boolean supportsIPv4()/*-{
		var obj = this.@com.emitrom.ti4j.core.client.ProxyObject::getJsObj()();
		return obj.supportsIPv4();
    }-*/;

    public final native boolean supportsIPv6()/*-{
		var obj = this.@com.emitrom.ti4j.core.client.ProxyObject::getJsObj()();
		return obj.supportsIPv6();
    }-*/;

    static List<Interface> fromJsArray(JavaScriptObject peers) {
        List<Interface> files = new ArrayList<Interface>();
        int size = JsoHelper.arrayLength(peers);
        for (int i = 0; i < size; i++) {
            files.add(new Interface(JsoHelper.getValueFromJavaScriptObjectArray(peers, i)));
        }
        return files;
    }

}
