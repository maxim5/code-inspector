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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.rapidminer.beans.utils.OperatorHelpFinder;

/**
 * @author chris
 * 
 */
public class OperatorList {

	static Logger log = LoggerFactory.getLogger(OperatorList.class);
	TreeSet<OperatorInfo> operators = new TreeSet<OperatorInfo>();

	Document doc = null;
	String name = "DataStream";
	String version = "5.0";
	String docBundle = "stream/plugin/resources/i18n/OperatorsDoc";

	public OperatorList() throws Exception {
		DocumentBuilder b = DocumentBuilderFactory.newInstance()
				.newDocumentBuilder();
		doc = b.newDocument();

		Element operatorsNode = doc.createElement("operators");
		doc.appendChild(operatorsNode);
	}

	public OperatorList(File file) throws Exception {
		this(new FileInputStream(file));
	}

	public OperatorList(URL url) throws Exception {
		this(url.openStream());
	}

	public Collection<OperatorInfo> getOperators() {
		return operators;
	}

	public OperatorList(InputStream in) throws Exception {
		DocumentBuilder b = DocumentBuilderFactory.newInstance()
				.newDocumentBuilder();
		doc = b.parse(in);

		NodeList groupList = doc.getElementsByTagName("group");
		for (int i = 0; i < groupList.getLength(); i++) {

			Element node = (Element) groupList.item(i);
			String grp = node.getAttribute("key");
			NodeList ops = node.getElementsByTagName("operator");

			for (int j = 0; j < ops.getLength(); j++) {

				Element op = (Element) ops.item(j);

				String opKey = op.getElementsByTagName("key").item(0)
						.getTextContent();
				String className = op.getElementsByTagName("class").item(0)
						.getTextContent();

				OperatorInfo info = new OperatorInfo(grp, opKey, className);

				Class<?> clazz = Class.forName(className);
				String help = OperatorHelpFinder.findOperatorHelp(clazz,
						OperatorList.class.getClassLoader());
				if (help != null)
					info.setDocText(help);

				log.debug("Adding operator-info {}", info);
				operators.add(info);
			}
		}
		log.debug("{} operators read from input-stream", operators.size());
	}

	public void add(OperatorInfo info) {
		if (operators.contains(info)) {
			log.debug("Operator {} already exists!");
			return;
		}

		log.debug("Adding operator-info {}", info);
		operators.add(info);

		if (doc != null) {
			log.debug("  Adding description to DOM");
			Element group = findOrCreateGroupElement(info.getGroup(), doc);
			insertOperator(info, group, doc);
		}
	}

	public void add(Collection<OperatorInfo> infos) {
		for (OperatorInfo info : infos)
			add(info);
	}

	public void setDefaultGroup(String grp) {
		for (OperatorInfo op : operators) {
			if (op.getGroup() == null || op.getGroup().trim().isEmpty()) {
				// op.setGroup( OperatorGenerator.mapGroup( grp ) );
			}
		}
	}

	public String toXML() {
		StringBuffer s = new StringBuffer(
				"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n");
		s.append("<operators name=\"" + name + "\" version=\"" + version
				+ "\" docbundle=\"" + docBundle + "\">\n");

		String group = null;

		for (OperatorInfo info : operators) {

			if (group == null) {
				group = info.getGroup();
				s.append("<group key=\"" + group + "\">\n");
			}

			if (!group.equals(info.getGroup())) {
				s.append("</group>\n");
				s.append("<group key=\"" + info.getGroup() + "\">\n");
			}

			s.append(info.toXML());

			group = info.getGroup();
		}

		s.append("</operators>");
		return s.toString();
	}

	public void insertIntoOperatorsXml(InputStream source, File file)
			throws Exception {

		DocumentBuilder builder = DocumentBuilderFactory.newInstance()
				.newDocumentBuilder();
		Document doc = builder.parse(source);

		insertIntoDom(doc);

		Transformer trans = TransformerFactory.newInstance().newTransformer();
		trans.setOutputProperty(OutputKeys.STANDALONE, "no");
		trans.setOutputProperty(OutputKeys.ENCODING, "utf-8");
		trans.setOutputProperty(OutputKeys.VERSION, "1.0");
		trans.setOutputProperty(OutputKeys.INDENT, "yes");
		trans.setOutputProperty("{http://xml.apache.org/xslt}indent-amount",
				"2");

		OutputStream out = new FileOutputStream(file);
		trans.transform(new DOMSource(doc), new StreamResult(out));
		out.close();
		// trans.transform( new DOMSource( doc ), new StreamResult( System.out )
		// );
	}

	private void insertIntoDom(Document doc) {

		Map<String, List<OperatorInfo>> groups = new LinkedHashMap<String, List<OperatorInfo>>();
		for (OperatorInfo info : operators) {
			String grp = info.getGroup();
			if (grp == null)
				grp = "";
			List<OperatorInfo> list = groups.get(grp);
			if (list == null) {
				list = new ArrayList<OperatorInfo>();
			}

			list.add(info);
			groups.put(grp, list);
		}

		for (String group : groups.keySet()) {
			Element grp = findOrCreateGroupElement(group, doc);
			for (OperatorInfo info : groups.get(group)) {
				log.info("Adding new operator {}", info);
				this.insertOperator(info, grp, doc);
			}
		}
	}

	private Element findOrCreateGroupElement(String groupKey, Document doc) {

		NodeList list = doc.getElementsByTagName("group");
		for (int i = 0; i < list.getLength(); i++) {

			Node node = list.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE
					&& node.getNodeName().equals("group")) {

				Element element = (Element) node;
				if (groupKey.equals(element.getAttribute("key"))) {
					return element;
				}
			}
		}

		Element ops = doc.getDocumentElement();

		Element group = doc.createElement("group");
		group.setAttribute("key", groupKey);
		ops.appendChild(group);

		return group;
	}

	private void insertOperator(OperatorInfo info, Element group, Document doc) {

		Element operator = doc.createElement("operator");

		Element key = doc.createElement("key");
		key.setTextContent(info.getKey());
		operator.appendChild(key);

		Element clazz = doc.createElement("class");
		clazz.setTextContent(info.getClassName());
		operator.appendChild(clazz);

		if (info.getIcon() != null) {
			Element icon = doc.createElement("icon");
			icon.setTextContent(info.getIcon());
			operator.appendChild(icon);
		}

		group.appendChild(operator);
	}

	public static List<OperatorInfo> extractOperators(File file)
			throws Exception {
		List<OperatorInfo> ops = new ArrayList<OperatorInfo>();
		return ops;
	}

	public void writeOperatorsXml(OutputStream out) throws Exception {

		log.info("Writing operators.xml to output-stream {}", out);
		log.info("  this list contains {} operators", operators.size());

		if (doc == null) {
			DocumentBuilder builder = DocumentBuilderFactory.newInstance()
					.newDocumentBuilder();
			doc = builder.newDocument();
		}

		Transformer trans = TransformerFactory.newInstance().newTransformer();
		trans.setOutputProperty(OutputKeys.STANDALONE, "no");
		trans.setOutputProperty(OutputKeys.ENCODING, "utf-8");
		trans.setOutputProperty(OutputKeys.VERSION, "1.0");
		trans.setOutputProperty(OutputKeys.INDENT, "yes");
		trans.setOutputProperty("{http://xml.apache.org/xslt}indent-amount",
				"2");

		trans.transform(new DOMSource(doc), new StreamResult(out));
		out.close();
	}

	public void writerOperatorsDocXml(OutputStream out) throws Exception {

		DocumentBuilder builder = DocumentBuilderFactory.newInstance()
				.newDocumentBuilder();
		Document doc = builder.newDocument();

		Element help = doc.createElement("operatorHelp");
		help.setAttribute("lang", "en_EN");

		doc.appendChild(help);

		for (OperatorInfo info : operators) {

			Element docu = doc.createElement("operator");

			Element name = doc.createElement("name");
			name.setTextContent(info.getKey());

			docu.appendChild(name);

			Element synopsis = doc.createElement("synopsis");
			docu.appendChild(synopsis);

			// TODO: Extract the Description text here => synopsis

			if (info.getDocText() != null) {
				Element helpText = doc.createElement("help");
				helpText.setTextContent(info.getDocText());
				docu.appendChild(helpText);
			}

			Element key = doc.createElement("key");
			key.setTextContent(info.getKey());

			docu.appendChild(key);

			help.appendChild(docu);
		}

		Transformer trans = TransformerFactory.newInstance().newTransformer();
		trans.setOutputProperty(OutputKeys.STANDALONE, "no");
		trans.setOutputProperty(OutputKeys.ENCODING, "utf-8");
		trans.setOutputProperty(OutputKeys.VERSION, "1.0");
		trans.setOutputProperty(OutputKeys.INDENT, "yes");
		trans.setOutputProperty("{http://xml.apache.org/xslt}indent-amount",
				"2");

		trans.transform(new DOMSource(doc), new StreamResult(out));
		out.close();
	}

	public static void main(String[] args) throws Exception {

		OperatorList list = new OperatorList(
				OperatorList.class
						.getResource("/stream/plugin/resources/DataStreamOperators.xml")); // new
																							// File(
																							// "/Users/chris/Uni/Projekte/fact-analysis/fact-tools/src/main/resources/fact/resources/FACT-operators-core.xml")
																							// );

		args = new String[] {
				"/Users/chris/Uni/Projekte/fact-analysis/fact-tools/src/main/resources/fact/resources/FACT-operators-core.xml",
				"/tmp/FACT-operators.xml", };

		list.insertIntoOperatorsXml(new FileInputStream(args[0]), new File(
				args[1]));

	}
}