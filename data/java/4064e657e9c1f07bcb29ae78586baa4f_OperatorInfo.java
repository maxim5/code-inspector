/*
 *  stream.ai
 *
 *  Copyright (C) 2011-2012 by Christian Bockermann, Hendrik Blom
 * 
 *  stream.ai is a library, API and runtime environment for processing high
 *  volume data streams. It is composed of three submodules "stream-api",
 *  "stream-core" and "stream-runtime".
 *
 *  The stream.ai library (and its submodules) is free software: you can 
 *  redistribute it and/or modify it under the terms of the 
 *  GNU Affero General Public License as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any 
 *  later version.
 *
 *  The stream.ai library (and its submodules) is distributed in the hope
 *  that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program.  If not, see http://www.gnu.org/licenses/.
 */
package com.rapidminer.tools;

import org.w3c.dom.Element;

import stream.annotations.Description;

import com.rapidminer.operator.Operator;

/**
 * @author chris
 * 
 */
public class OperatorInfo implements Comparable<OperatorInfo> {

	String group;
	String key;
	String className;
	String icon = null;
	String docText = null;

	public OperatorInfo(Description desc, Class<? extends Operator> clazz) {
		className = clazz.getCanonicalName();
		init(desc);

		if (desc.name() == null)
			key = clazz.getSimpleName();

		if (desc.group() == null)
			group = clazz.getPackage().getName();
	}

	public OperatorInfo(com.rapidminer.annotations.OperatorInfo info,
			Class<? extends Operator> clazz) {
		className = clazz.getCanonicalName();
		init(info);
	}

	public OperatorInfo(Class<? extends Operator> clazz) {
		className = clazz.getCanonicalName();

		//
		// the com.rapidminer.annotations.OperatorInfo annotation has
		// precedence...
		//
		if (clazz
				.isAnnotationPresent(com.rapidminer.annotations.OperatorInfo.class))
			init(clazz
					.getAnnotation(com.rapidminer.annotations.OperatorInfo.class));
		else {
			if (clazz.isAnnotationPresent(Description.class))
				init(clazz.getAnnotation(Description.class));
		}
	}

	private void init(com.rapidminer.annotations.OperatorInfo info) {
		key = info.name();
		group = info.group();
		docText = info.text();
		icon = info.icon();
	}

	private void init(Description info) {
		key = info.name();
		group = info.group();
		docText = info.text();
		icon = info.icon();
	}

	public OperatorInfo(String grp, String key, String className) {
		this.group = grp;
		this.key = key;
		this.className = className;
	}

	/**
	 * @return the group
	 */
	public String getGroup() {
		return group;
	}

	/**
	 * @param group
	 *            the group to set
	 */
	public void setGroup(String group) {
		this.group = group;
	}

	/**
	 * @return the key
	 */
	public String getKey() {
		return key;
	}

	/**
	 * @param key
	 *            the key to set
	 */
	public void setKey(String key) {
		this.key = key;
	}

	/**
	 * @return the className
	 */
	public String getClassName() {
		return className;
	}

	/**
	 * @param className
	 *            the className to set
	 */
	public void setClassName(String className) {
		this.className = className;
	}

	/**
	 * @return the icon
	 */
	public String getIcon() {
		return icon;
	}

	/**
	 * @param icon
	 *            the icon to set
	 */
	public void setIcon(String icon) {
		if (icon == null || "".equals(icon.trim()))
			return;
		this.icon = icon;
	}

	/**
	 * @return the docText
	 */
	public String getDocText() {
		return docText;
	}

	/**
	 * @param docText
	 *            the docText to set
	 */
	public void setDocText(String docText) {
		this.docText = docText;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(OperatorInfo arg0) {

		if (!this.group.equals(arg0.group)) {
			return group.compareTo(arg0.group);
		}

		return getKey().compareTo(arg0.getKey());
	}

	public boolean equals(Object o) {
		if (o instanceof OperatorInfo) {
			return this.toString().equals(o.toString());
		}
		return false;
	}

	public String toString() {
		return "Group: " + group + ", Key: " + key + ", Class: " + className;
	}

	public String toXML() {
		StringBuffer s = new StringBuffer();
		s.append("\t<operator>\n");

		s.append("\t\t<key>" + getKey() + "</key>\n");
		s.append("\t\t<class>" + getClassName() + "</class>\n");

		s.append("\t</operator>\n");
		return s.toString();
	}

	public void addDomNode(Element node) {
	}
}