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
package com.rapidminer.beans.utils;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import stream.annotations.Description;

import com.rapidminer.annotations.OperatorInfo;
import com.rapidminer.beans.OperatorBeanDescription;

/**
 * @author chris
 * 
 */
public class ProcessorFinder {

	static Logger log = LoggerFactory.getLogger(ProcessorFinder.class);

	public static List<Class<?>> findProcessors(String[] packageNames,
			ClassLoader classLoader) {

		List<Class<?>> result = new ArrayList<Class<?>>();

		for (String pkgName : packageNames) {
			result.addAll(findProcessors(pkgName, classLoader));
		}

		return result;
	}

	public static List<Class<?>> findProcessors(String pkgName,
			ClassLoader classLoader) {
		List<Class<?>> list = new ArrayList<Class<?>>();

		try {
			Class<?>[] classes = ClassFinder.getClasses(pkgName, classLoader);
			for (Class<?> clazz : classes) {

				if (clazz.isInterface()
						|| Modifier.isAbstract(clazz.getModifiers())) {
					continue;
				}

				if (clazz.toString().indexOf("$") > 0) {
					continue;
				}

				if (!OperatorBeanDescription.canCreate(clazz))
					continue;

				OperatorInfo info = clazz.getAnnotation(OperatorInfo.class);
				Description desc = clazz.getAnnotation(Description.class);
				if (desc == null && info == null) {
					log.debug(
							"Skipping processor class '{}' due to missing Description annotation...",
							clazz);
					continue;
				}

				Class<?> pc = (Class<?>) clazz;
				list.add(pc);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		log.debug("Found {} operator classes.", list.size());
		return list;
	}
}
