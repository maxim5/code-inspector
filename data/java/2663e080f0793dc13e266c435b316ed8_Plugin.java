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
package com.rapidminer.beans;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.PropertyConfigurator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import stream.annotations.Description;

import com.rapidminer.annotations.OperatorInfo;
import com.rapidminer.beans.utils.ParameterTypeFinder;
import com.rapidminer.beans.utils.ProcessorFinder;
import com.rapidminer.parameter.ParameterType;
import com.rapidminer.tools.OperatorService;

/**
 * @author chris
 * 
 */
public final class Plugin {

	static Logger log = LoggerFactory.getLogger(Plugin.class);

	public final static String NAME = "RapidMiner-Beans-Plugin";

	public final static String BEAN_DIR = ".RapidMiner5" + File.separator
			+ "beans";

	public final static String VERSION = Plugin.class.getPackage()
			.getImplementationVersion();

	public final static String DATA_ITEM_PORT_NAME = "data item";

	public final static String DATA_STREAM_PORT_NAME = "stream";

	final static Set<Class<?>> REGISTERED_PROCESSORS = new HashSet<Class<?>>();

	public static void initPlugin() {

		URL url = Plugin.class
				.getResource("/com/rapidminer/beans/resources/log4j.properties");
		if ("true"
				.equalsIgnoreCase(System.getProperty("RapidMinerBeans.debug"))) {
			url = Plugin.class
					.getResource("/com/rapidminer/beans/resources/log4j-debug.properties");
		}
		if (url != null)
			PropertyConfigurator.configure(url);

		log.info("");
		log.info("Initializing {}, {}", NAME, VERSION);
		log.info("");

		RapidMinerBeans.findAndRegisterBeans();

		if (System.getProperty("old") == null) {
			log.info("Ignoring old beans-code...");
			return;
		}

		List<File> beanPaths = new ArrayList<File>();

		// the default path to look for is $HOME/.RapidMiner5/beans/
		//
		beanPaths.add(new File(System.getProperty("user.home") + File.separator
				+ ".RapidMiner5/beans"));

		if (System.getProperty("RAPIDMINER_BEANS") != null) {
			String[] dirs = System.getProperty("RAPIDMINER_BEANS").split(
					File.pathSeparator);
			for (String dir : dirs) {
				beanPaths.add(new File(dir));
			}
		}

		URL[] externalJars = new URL[0];
		for (int i = 0; i < externalJars.length; i++) {
			try {
				externalJars[i] = beanPaths.get(i).toURI().toURL();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		try {
			log.debug("Creating new class-loader with jars: {}",
					(Object[]) externalJars);
			URLClassLoader classLoader = URLClassLoader.newInstance(
					externalJars, Plugin.class.getClassLoader());

			log.debug("Checking for processor classes...");
			List<Class<?>> processorClasses = ProcessorFinder.findProcessors(
					new String[] { "" }, classLoader);

			for (Class<?> clazz : processorClasses) {
				log.debug("Checking class: {}", clazz);
				if (!OperatorBeanDescription.canCreate(clazz)) {
					continue;
				}

				if (REGISTERED_PROCESSORS.contains(clazz)) {
					log.debug("Operator for processor {} already registered.",
							clazz);
					continue;
				}

				log.info("Registering operator for processor {}", clazz);
				Map<String, ParameterType> types = ParameterTypeFinder
						.getParameterTypes(clazz);
				for (String key : types.keySet()) {
					log.debug("   {} = {}", key, types.get(key).getClass());
				}

				String group = clazz.getPackage().getName();
				String key = clazz.getSimpleName();

				Description desc = clazz.getAnnotation(Description.class);
				if (desc != null) {
					if (desc.name() != null && !"".equals(desc.name().trim()))
						key = desc.name();

					if (desc.group() != null)
						group = desc.group();
				}

				OperatorInfo info = clazz.getAnnotation(OperatorInfo.class);
				if (info != null) {
					if (info.name() != null && !info.name().trim().isEmpty())
						key = info.name().trim();

					if (info.group() != null) {
						group = info.group();
					}
				}

				OperatorBeanDescription sod = new OperatorBeanDescription(
						group, key, clazz, Plugin.class.getClassLoader(), null,
						null);

				OperatorService.registerOperator(sod, null);
				REGISTERED_PROCESSORS.add(clazz);
			}

		} catch (Exception e) {
			log.error("Failed to initialized logging: {}", e.getMessage());
			e.printStackTrace();
		}

	}
}