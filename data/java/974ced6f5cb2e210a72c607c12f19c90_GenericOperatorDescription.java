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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.rapidminer.beans.utils.OperatorHelpFinder;
import com.rapidminer.generic.GenericBeanOperator;
import com.rapidminer.operator.Operator;
import com.rapidminer.operator.OperatorDescription;
import com.rapidminer.tools.OperatorInfo;
import com.rapidminer.tools.plugin.Plugin;

/**
 * @author chris
 * 
 */
public class GenericOperatorDescription extends OperatorDescription {

	static Logger log = LoggerFactory
			.getLogger(GenericOperatorDescription.class);

	final Class<?> libClass;

	public GenericOperatorDescription(OperatorInfo info, Class<?> libClass,
			ClassLoader classLoader, Plugin plugin) {
		super(info.getGroup(), info.getKey(), GenericBeanOperator.class,
				classLoader, "", plugin, null);
		this.libClass = libClass;
	}

	/**
	 * @param fullyQualifiedGroupKey
	 * @param key
	 * @param clazz
	 * @param classLoader
	 * @param iconName
	 * @param provider
	 */
	public GenericOperatorDescription(String fullyQualifiedGroupKey,
			String key, Class<?> clazz, ClassLoader classLoader,
			String iconName, Plugin provider) {
		super(fullyQualifiedGroupKey, key, GenericBeanOperator.class,
				classLoader, iconName, provider, null);

		libClass = clazz;

		getOperatorDocumentation().setSynopsis("");
		getOperatorDocumentation().setDocumentation("");

		try {
			String html = OperatorHelpFinder.findOperatorHelp(libClass,
					classLoader);
			if (html != null) {
				log.trace("Adding operator-documentation:\n{}", html);
				getOperatorDocumentation().setDocumentation(html);
			}
		} catch (Exception e) {
			log.error("Failed to lookup documentation for class '{}': {}",
					libClass, e.getMessage());
			if (log.isDebugEnabled())
				e.printStackTrace();
		}

	}

	/**
	 * @see com.rapidminer.operator.OperatorDescription#createOperatorInstanceByDescription(com.rapidminer.operator.OperatorDescription)
	 */
	@Override
	protected Operator createOperatorInstanceByDescription(
			OperatorDescription description) throws IllegalArgumentException,
			InstantiationException, IllegalAccessException,
			InvocationTargetException, SecurityException, NoSuchMethodException {

		Operator op = null;

		if (description instanceof GenericOperatorDescription) {
			GenericOperatorDescription sod = (GenericOperatorDescription) description;

			if (Operator.class.isAssignableFrom(libClass)) {
				log.debug("Provided class already is a fully blown operator!");

				Constructor<?> constructor = libClass
						.getConstructor(OperatorDescription.class);

				op = (Operator) constructor.newInstance(sod);
				return op;
			}

			log.debug("Operator of class {} is {}", libClass, op);
			return op;
		}

		log.warn("No support for generic operator instantiation of class {}",
				libClass);
		return super.createOperatorInstanceByDescription(description);
	}

	public static boolean canCreate(Class<?> clazz) {

		if (Modifier.isAbstract(clazz.getModifiers()))
			return false;

		if (clazz.isAnnotation() || clazz.isInterface())
			return false;

		if (Operator.class.isAssignableFrom(clazz)
				&& (clazz
						.isAnnotationPresent(com.rapidminer.annotations.OperatorInfo.class))) {
			log.debug(
					"Yes, we support direct creation of Operators...(class {})",
					clazz);
			return true;
		}

		log.debug("No generic operator-support for class '{}'", clazz);
		return false;
	}
}
